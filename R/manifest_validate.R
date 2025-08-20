#' Validate an externals manifest
#'
#' @description
#' Checks that a manifest (as loaded from JSON or returned by
#' [manifest_template()]) conforms to the expected schema.
#' Validation ensures that required fields are present, types are correct,
#' and values are within allowed sets.
#'
#' @param manifest Either:
#'   - A file path to a JSON manifest, or
#'   - A list already parsed from JSON (e.g., result of
#'     `jsonlite::fromJSON(..., simplifyVector = FALSE)`).
#' @param error_on_false Logical.
#'   - If `TRUE` (default): **aggregate all problems** and stop with a single
#'     error message listing each problem on its own line.
#'   - If `FALSE`: return a character vector of all problems (or
#'     `character(0)` if valid).
#'
#' @return
#' - Invisibly `TRUE` if validation succeeds and `error_on_false = TRUE`.
#' - A character vector of problems if `error_on_false = FALSE`. Empty vector if valid.
#'
#' @details
#' ### Required structure
#' - Top-level object with key `"repos"` (array of repositories).
#' - Optional `"defaults"` object.
#'
#' ### Repository fields
#' - `repo` (**required**, string): GitHub repository in `"owner/name"` form.
#' - `ref` (optional, string): Git reference (tag, branch, or SHA). Must be a single string if provided.
#' - `api_env` (optional, string): Name of environment variable for token.
#'   - Must be a single string if provided.
#'   - If present but not set in the current R session, a warning is issued.
#' - `files` (**required**, array of file objects).
#'
#' ### File fields
#' - `path` (**required**, string): Path in the repo.
#' - `dest` (**required**, string): Destination path in project.
#' - `source_url` (**required**, string): Must be one of:
#'   - `"frontmatter"`
#'   - `"file"`
#'   - `"none"`
#'
#' See [manifest_template()] for a detailed description of the manifest schema.
#'
#' @examples
#' \dontrun{
#' # Validate a manifest file (stop with a single aggregated error if invalid)
#' manifest_validate("externals.json")
#'
#' # Validate but collect all problems instead of stopping
#' issues <- manifest_validate("externals.json", error_on_false = FALSE)
#' if (length(issues)) writeLines(issues)
#' }
#'
#' @importFrom jsonlite fromJSON
#' @export
manifest_validate <- function(manifest, error_on_false = TRUE) {
  problems <- character()

  # Load from file if character path
  if (is.character(manifest) && length(manifest) == 1L) {
    if (!file.exists(manifest)) {
      problems <- c(problems, paste0("Manifest file not found: ", manifest))
      if (error_on_false) {
        stop(paste(problems, collapse = "\n"), call. = FALSE)
      }
      return(problems)
    }
    manifest <- jsonlite::fromJSON(manifest, simplifyVector = FALSE)
  }

  if (!is.list(manifest)) {
    problems <- c(problems, "Manifest must be a list or a path to a JSON file.")
    if (error_on_false) {
      stop(paste(problems, collapse = "\n"), call. = FALSE)
    }
    return(problems)
  }

  # repos must exist and be a list
  repos <- manifest$repos
  if (is.null(repos) || !is.list(repos) || !length(repos)) {
    problems <- c(problems, "Manifest must contain a non-empty 'repos' array.")
  }

  # Validate repos/files if present
  if (is.list(repos) && length(repos)) {
    for (i in seq_along(repos)) {
      repo <- repos[[i]]
      prefix <- sprintf("repos[[%d]]", i)

      # repo field
      if (
        is.null(repo$repo) ||
          !is.character(repo$repo) ||
          length(repo$repo) != 1L
      ) {
        problems <- c(
          problems,
          paste0(prefix, ": missing or invalid 'repo' (string 'owner/name').")
        )
      } else if (!grepl("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$", repo$repo)) {
        problems <- c(
          problems,
          paste0(
            prefix,
            ": 'repo' must be in 'owner/name' form (got '",
            repo$repo,
            "')."
          )
        )
      }

      # ref field
      if (
        !is.null(repo$ref) &&
          (!is.character(repo$ref) || length(repo$ref) != 1L)
      ) {
        problems <- c(
          problems,
          paste0(prefix, ": 'ref' must be a single string if provided.")
        )
      }

      # api_env field
      if (!is.null(repo$api_env)) {
        if (!is.character(repo$api_env) || length(repo$api_env) != 1L) {
          problems <- c(
            problems,
            paste0(prefix, ": 'api_env' must be a single string if provided.")
          )
        } else {
          if (identical(Sys.getenv(repo$api_env, unset = ""), "")) {
            warning(
              sprintf(
                "%s: environment variable '%s' is not set.",
                prefix,
                repo$api_env
              ),
              call. = FALSE
            )
          }
        }
      }

      # files array
      if (is.null(repo$files) || !is.list(repo$files) || !length(repo$files)) {
        problems <- c(
          problems,
          paste0(prefix, ": must contain a non-empty 'files' array.")
        )
      } else {
        for (j in seq_along(repo$files)) {
          file <- repo$files[[j]]
          fprefix <- sprintf("%s$files[[%d]]", prefix, j)

          # path
          if (
            is.null(file$path) ||
              !is.character(file$path) ||
              length(file$path) != 1L
          ) {
            problems <- c(
              problems,
              paste0(fprefix, ": missing or invalid 'path' (string).")
            )
          }

          # dest
          if (
            is.null(file$dest) ||
              !is.character(file$dest) ||
              length(file$dest) != 1L
          ) {
            problems <- c(
              problems,
              paste0(fprefix, ": missing or invalid 'dest' (string).")
            )
          }

          # source_url
          if (
            is.null(file$source_url) ||
              !is.character(file$source_url) ||
              length(file$source_url) != 1L
          ) {
            problems <- c(
              problems,
              paste0(fprefix, ": missing or invalid 'source_url' (string).")
            )
          } else if (!file$source_url %in% c("frontmatter", "file", "none")) {
            problems <- c(
              problems,
              sprintf(
                "%s: invalid 'source_url'='%s' (allowed: 'frontmatter', 'file', 'none').",
                fprefix,
                file$source_url
              )
            )
          }
        }
      }
    }
  }
  if (error_on_false) {
    if (length(problems)) {
      stop(paste(problems, collapse = "\n"), call. = FALSE)
    }
    invisible(TRUE)
  } else {
    problems
  }
}
