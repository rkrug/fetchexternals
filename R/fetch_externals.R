#' Fetch external files from GitHub repositories
#'
#' @description
#' Downloads text files (e.g., `.qmd`) from external GitHub repositories
#' into the current project based on a JSON manifest. Handles authentication,
#' caching via ETag sidecars, and optional recording of a canonical source URL.
#'
#' @param manifest Path to a JSON manifest file describing repositories and files.
#' @param quiet Logical; if `TRUE`, suppresses progress messages. Default `FALSE`.
#'
#' @section Manifest format:
#' The manifest is a JSON object with top-level key `"repos"`, each entry containing:
#'
#' - `repo` (string): Repository in `"owner/name"` format.
#' - `ref`  (string): Git reference (tag, branch, or SHA). If you use `"latest"` in
#'   your workflow, resolve it before calling this function, or ensure your implementation
#'   resolves `"latest"` to a concrete tag.
#' - `api_env` (string, optional): Name of the environment variable that holds the
#'   GitHub API token to use for this repository.
#' - `files` (array): List of file objects with:
#'   - `path` (string): Path inside the repository to fetch.
#'   - `dest` (string): Destination path in the current project.
#'   - `source_url` (string): **One of** `"frontmatter"`, `"file"`, or `"none"`, controlling
#'     how the canonical source URL is recorded:
#'       * `"frontmatter"` — Insert or update a `source_url:` key **only if** the file
#'         has a **well-formed YAML front matter** block (opening `---` and a matching
#'         closing `---`). If no valid front matter is present, the file is left **unchanged**.
#'       * `"file"` — Write the canonical source URL to a sidecar file named `<dest>.source`.
#'       * `"none"` — Do not record the source URL.
#'
#' @section Authentication:
#' For each repository, if the environment variable named by `api_env` is set,
#' API calls are authenticated via the `Authorization: Bearer` header. If `api_env`
#' is not provided or is empty, use your package’s default token discovery (e.g.,
#' `GITHUB_TOKEN`). Authentication increases rate limits and allows access to private
#' repositories.
#'
#' @section ETag caching:
#' Each fetched file writes an `.etag` sidecar next to `dest`. On subsequent runs,
#' the request includes `If-None-Match` and the file is only re-downloaded if it changed.
#'
#' @return Invisibly, the number of files fetched or updated.
#'
#' @examples
#' \dontrun{
#' # Fetch according to a manifest file
#' fetch_externals("externals.json")
#' }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom curl curl_fetch_memory new_handle handle_setheaders
#' @export
fetch_externals <- function(
  manifest = "externals.json",
  quiet = FALSE
) {
  # soft dependency checks (internals will call namespaced functions)
  if (!base::file.exists(manifest)) {
    base::stop(base::sprintf("Manifest not found: %s", manifest), call. = FALSE)
  }

  # parse manifest (uses jsonlite internally via internals.R)
  manifest_obj <- jsonlite::fromJSON(manifest, simplifyVector = TRUE)

  repos <- manifest_obj$repos
  if (base::is.null(repos) || base::length(repos) == 0L) {
    if (!quiet) {
      base::message("No repos in manifest; nothing to do.")
    }
    return(base::invisible(0L))
  }

  defaults <- manifest_obj$defaults
  include_pre_default <- base::isTRUE(defaults$include_prereleases)
  fallback_default <- if (base::is.null(defaults$fallback_to_default_branch)) {
    TRUE
  } else {
    base::isTRUE(defaults$fallback_to_default_branch)
  }

  token <- .bearer_token()
  if (base::is.null(token) && !quiet) {
    base::message(
      "Note: no GITHUB_TOKEN found; API rate limit is low and private repos won't work."
    )
  }

  updated <- 0L

  for (entry in repos) {
    owner_repo <- entry$repo
    if (base::is.null(owner_repo) || !base::nzchar(owner_repo)) {
      next
    }

    ref <- entry$ref
    use_pre <- if (!base::is.null(entry$include_prereleases)) {
      base::isTRUE(entry$include_prereleases)
    } else {
      include_pre_default
    }
    fallback <- if (!base::is.null(entry$fallback_to_default_branch)) {
      base::isTRUE(entry$fallback_to_default_branch)
    } else {
      fallback_default
    }

    resolved <- ref
    if (
      base::is.null(ref) ||
        base::identical(ref, "") ||
        base::identical(ref, "latest")
    ) {
      tag <- .latest_release_tag(
        owner_repo,
        include_prereleases = use_pre,
        token = token
      )
      if (base::is.null(tag) && base::isTRUE(fallback)) {
        tag <- .default_branch(owner_repo, token = token)
      }
      if (base::is.null(tag)) {
        base::stop(
          base::sprintf(
            "No releases and no default branch found for %s",
            owner_repo
          ),
          call. = FALSE
        )
      }
      resolved <- tag
    }

    files <- entry$files
    if (base::is.null(files) || base::length(files) == 0L) {
      next
    }

    for (f in files) {
      src <- f$path
      dest <- f$dest
      if (is.null(src) || is.null(dest) || !nzchar(src) || !nzchar(dest)) {
        next
      }

      # -------- determine source_url mode and URL value -----------------------
      # Allowed symbolic modes: "frontmatter", "file", "none"
      # Back-compat: if `f$source_url` looks like a URL, use that value and default mode = "frontmatter"
      mode <- "frontmatter"
      url_value <- NULL
      su_field <- f$source_url

      is_symbol <- is.character(su_field) &&
        length(su_field) == 1L &&
        su_field %in% c("frontmatter", "file", "none")

      if (!is.null(su_field) && is_symbol) {
        mode <- su_field
      } else if (is.null(su_field) || identical(su_field, "")) {
        mode <- "file"
      } else {
        # Defensive: unknown value → treat as "frontmatter"
        mode <- "file"
      }

      if (!mode %in% c("frontmatter", "file", "none")) {
        stop(
          sprintf(
            "Invalid `source_url` mode for %s: %s (allowed: frontmatter|file|none)",
            dest,
            mode
          ),
          call. = FALSE
        )
      }

      # Default URL value if not explicitly provided
      if (is.null(url_value)) {
        url_value <- sprintf(
          "https://github.com/%s/blob/%s/%s",
          owner_repo,
          resolved,
          src
        )
      }

      # ---------------- ETag sidecar & fetch via Contents API -----------------
      etag_path <- paste0(dest, ".etag")
      etag <- .read_etag(etag_path)

      res <- .fetch_contents_api(
        owner_repo,
        resolved,
        src,
        token = token,
        etag = etag
      )

      if (identical(res$status, 304L)) {
        existing <- .read_text_file(dest)
        if (!is.null(existing)) {
          if (mode == "frontmatter") {
            newtxt <- .upsert_source_url(existing, url_value) # only if YAML is well-formed
            if (!identical(existing, newtxt)) {
              .write_text_file(dest, newtxt)
              updated <- updated + 1L
              if (!quiet) message(sprintf("Updated front matter: %s", dest))
            }
          } else if (mode == "file") {
            .write_source_sidecar(dest, url_value)
            # (Optionally bump `updated` if you want to count sidecar writes)
          } else if (mode == "none") {
            # Do nothing
          }
        }
        next
      }

      # Fresh content
      txt <- res$text
      if (mode == "frontmatter") {
        txt <- .upsert_source_url(txt, url_value) # no creation if not well-formed
        .write_text_file(dest, txt)
      } else if (mode == "file") {
        .write_text_file(dest, txt)
        .write_source_sidecar(dest, url_value)
      } else if (mode == "none") {
        .write_text_file(dest, txt) # just write the content; no source recording
      }
      .write_etag(etag_path, res$etag)
      updated <- updated + 1L
      if (!quiet) {
        message(sprintf(
          "Fetched %s@%s: %s -> %s",
          owner_repo,
          resolved,
          src,
          dest
        ))
      }
    }
  }

  if (!quiet) {
    base::message(base::sprintf("Done. Files updated: %d", updated))
  }
  base::invisible(updated)
}
