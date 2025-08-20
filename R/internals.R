#' Read a UTF-8 text file if it exists
#'
#' Safely reads a text file and returns its contents as a single character
#' string with embedded newlines. Returns `NULL` if the file does not exist.
#'
#' @param path Character scalar; path to the file.
#'
#' @return `NULL` if the file does not exist, otherwise a single-length character string.
#'
#' @keywords internal
#' @noRd
read_text_file <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  paste0(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

#' Write UTF-8 text to a file (creating parent directories)
#'
#' Creates parent directories as needed and writes the provided text in UTF-8.
#'
#' @param path Character scalar; destination file path.
#' @param text Character scalar; file contents.
#'
#' @return Invisibly returns `TRUE` on success.
#'
#' @keywords internal
#' @noRd
.write_text_file <- function(path, text) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(text, con, useBytes = TRUE)
  invisible(TRUE)
}

#' Read an HTTP ETag sidecar file
#'
#' Reads the contents of an `.etag` sidecar (if present) and returns a trimmed
#' string suitable for sending as `If-None-Match`.
#'
#' @param sidecar Character scalar; path to the sidecar (e.g., `"<file>.etag"`).
#'
#' @return `NULL` if missing, otherwise a trimmed character scalar.
#'
#' @keywords internal
#' @noRd
.read_etag <- function(sidecar) {
  txt <- .read_text_file(sidecar)
  if (is.null(txt)) {
    return(NULL)
  }
  trimws(txt)
}

#' Write an HTTP ETag sidecar file
#'
#' Writes an ETag string to `sidecar` if non-empty.
#'
#' @param sidecar Character scalar; sidecar path.
#' @param etag Character scalar; ETag value returned by the server.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
#' @noRd
.write_etag <- function(sidecar, etag) {
  if (is.null(etag) || is.na(etag) || etag == "") {
    return(invisible(NULL))
  }
  .write_text_file(sidecar, etag)
}

#' Resolve a GitHub bearer token from environment variables
#'
#' If `env` is provided (character name of an environment variable), then the
#' function tries to read it first. Otherwise it falls back to `GITHUB_TOKEN`,
#' then `SITE_A_TOKEN`. Returns `NULL` if no usable token is found.
#'
#' @param env Optional character; name of an environment variable to read.
#'
#' @return Character scalar token or `NULL`.
#'
#' @keywords internal
#' @noRd
.bearer_token <- function(env = NULL) {
  if (!is.null(env) && nzchar(env)) {
    tok <- Sys.getenv(env, unset = NA)
    if (!is.na(tok) && tok != "") {
      return(tok)
    }
  }
  tok <- Sys.getenv("GITHUB_TOKEN", unset = NA)
  if (is.na(tok) || tok == "") {
    tok <- Sys.getenv("SITE_A_TOKEN", unset = NA)
  }
  if (is.na(tok) || tok == "") {
    return(NULL)
  }
  tok
}

#' Minimal HTTP GET using curl with custom headers
#'
#' Thin wrapper around `curl::curl_fetch_memory()` that sets headers and collects
#' the response body into memory. Intended for JSON/API requests.
#'
#' @param url Character scalar; request URL.
#' @param headers Named list of HTTP headers to send.
#'
#' @return A list with elements like `curl::curl_fetch_memory()`:
#' `status_code` (integer), `headers` (character vector), and `content` (raw).
#'
#' @importFrom curl curl_fetch_memory new_handle handle_setheaders
#'
#' @keywords internal
#' @noRd
.curl_get <- function(url, headers = list()) {
  h <- curl::new_handle()
  curl::handle_setheaders(h, .list = headers)
  mem <- raw()
  cb <- function(x) {
    mem <<- c(mem, x)
    nchar(x)
  }
  curl::curl_fetch_memory(url, handle = h, writefunction = cb)
}

#' GET and parse JSON from the GitHub API
#'
#' Performs an HTTP GET with GitHub-appropriate headers and optional bearer
#' token. On non-200 responses, raises an error including the response body.
#'
#' @param url Character; GitHub API URL.
#' @param token Optional character; bearer token for Authorization.
#' @param extra_headers Optional named list of headers to add.
#'
#' @return Parsed JSON as an R list via `jsonlite::fromJSON()`.
#'
#' @importFrom jsonlite fromJSON
#'
#' @keywords internal
#' @noRd
.json_get <- function(url, token = NULL, extra_headers = list()) {
  headers <- c(
    list(
      "Accept" = "application/vnd.github+json",
      "User-Agent" = "fetch-external-qmds-r/1.0"
    ),
    extra_headers
  )
  if (!is.null(token)) {
    headers <- c(headers, list("Authorization" = paste("Bearer", token)))
  }
  res <- .curl_get(url, headers)
  code <- res$status_code
  if (!identical(code, 200L)) {
    stop(
      sprintf("GET %s -> %s\n%s", url, code, rawToChar(res$content)),
      call. = FALSE
    )
  }
  jsonlite::fromJSON(rawToChar(res$content), simplifyVector = TRUE)
}

#' Resolve the latest release tag for a repository
#'
#' Determines the appropriate tag when the manifest indicates `"latest"`.
#' If `include_prereleases = FALSE`, uses the official GitHub "latest" endpoint
#' (which excludes drafts and prereleases). If `TRUE`, lists releases and picks
#' the newest non-draft (which may be a prerelease).
#'
#' @param owner_repo Character; `"ORG/REPO"` identifier.
#' @param include_prereleases Logical; include prereleases if `TRUE`.
#' @param token Optional bearer token for authentication.
#'
#' @return Character tag name (e.g., `"v1.2.3"`) or `NULL` if none found.
#'
#' @keywords internal
#' @noRd
.latest_release_tag <- function(
  owner_repo,
  include_prereleases = FALSE,
  token = NULL
) {
  GITHUB_API <- "https://api.github.com"
  if (isTRUE(include_prereleases)) {
    url <- sprintf("%s/repos/%s/releases?per_page=1", GITHUB_API, owner_repo)
    js <- tryCatch(.json_get(url, token = token), error = function(e) NULL)
    if (!is.null(js) && length(js) >= 1 && isFALSE(js[[1]]$draft)) {
      return(js[[1]]$tag_name)
    }
  } else {
    url <- sprintf("%s/repos/%s/releases/latest", GITHUB_API, owner_repo)
    tag <- tryCatch(
      .json_get(url, token = token)$tag_name,
      error = function(e) NULL
    )
    if (!is.null(tag)) return(tag)
  }
  NULL
}

#' Get the default branch name for a repository
#'
#' Fetches repository metadata and returns its default branch (e.g., `"main"`).
#' Used as a fallback when no releases exist and the manifest allows fallback.
#'
#' @param owner_repo Character; `"ORG/REPO"` identifier.
#' @param token Optional bearer token.
#'
#' @return Character branch name or `NULL` on error.
#'
#' @keywords internal
#' @noRd
.default_branch <- function(owner_repo, token = NULL) {
  GITHUB_API <- "https://api.github.com"
  url <- sprintf("%s/repos/%s", GITHUB_API, owner_repo)
  tryCatch(.json_get(url, token = token)$default_branch, error = function(e) {
    NULL
  })
}

#' Fetch a repository file via the GitHub Contents API (with ETag support)
#'
#' Uses the GitHub Contents API (`/repos/{owner}/{repo}/contents/{path}?ref={ref}`)
#' to retrieve the specified file. Supports Authorization for private repos and
#' `If-None-Match` conditional requests using the stored ETag.
#'
#' @param owner_repo Character; `"ORG/REPO"` identifier.
#' @param ref Character; tag/branch/commit to fetch from.
#' @param path Character; file path within the repository.
#' @param token Optional bearer token for authentication.
#' @param etag Optional character; previously stored ETag for conditional GET.
#'
#' @return A list with:
#' \itemize{
#'   \item `status`: integer `200` (fetched) or `304` (not modified)
#'   \item `text`: decoded UTF-8 file content (or `NULL` when `304`)
#'   \item `etag`: ETag string returned by the server (or previous one on `304`)
#' }
#'
#' @importFrom utils URLencode
#' @importFrom jsonlite fromJSON
#' @importFrom base64enc base64decode
#'
#' @keywords internal
#' @noRd
.fetch_contents_api <- function(
  owner_repo,
  ref,
  path,
  token = NULL,
  etag = NULL
) {
  GITHUB_API <- "https://api.github.com"
  url <- sprintf(
    "%s/repos/%s/contents/%s?ref=%s",
    GITHUB_API,
    owner_repo,
    utils::URLencode(path, reserved = TRUE),
    utils::URLencode(ref, reserved = TRUE)
  )
  headers <- list(
    "Accept" = "application/vnd.github+json",
    "User-Agent" = "fetch-external-qmds-r/1.0"
  )
  if (!is.null(token)) {
    headers[["Authorization"]] <- paste("Bearer", token)
  }
  if (!is.null(etag) && nzchar(etag)) {
    headers[["If-None-Match"]] <- etag
  }

  res <- .curl_get(url, headers)
  code <- res$status_code
  if (identical(code, 304L)) {
    return(list(status = 304L, text = NULL, etag = etag))
  }
  if (!identical(code, 200L)) {
    stop(
      sprintf("GET %s -> %s\n%s", url, code, rawToChar(res$content)),
      call. = FALSE
    )
  }

  # Extract ETag from raw header vector (case-insensitive "etag:")
  hdrs <- res$headers
  et <- hdrs[grepl("^etag:", hdrs, ignore.case = TRUE)]
  et <- if (length(et)) sub("^etag:\\s*", "", et, ignore.case = TRUE) else NULL

  js <- jsonlite::fromJSON(rawToChar(res$content), simplifyVector = TRUE)
  if (!identical(js$encoding, "base64")) {
    stop(
      "Unexpected encoding from Contents API (expected base64).",
      call. = FALSE
    )
  }
  rawtxt <- base64enc::base64decode(js$content)
  txt <- rawToChar(rawtxt)
  Encoding(txt) <- "UTF-8"
  list(status = 200L, text = txt, etag = et)
}

#' Detect well-formed YAML front matter
#'
#' @param text Character scalar; file content.
#' @return Integer index of the closing '---' fence if front matter is well-formed,
#'   otherwise NA_integer_.
#' @keywords internal
#' @noRd
.front_matter_end <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  if (length(lines) < 3L) {
    return(NA_integer_)
  }
  if (!identical(trimws(lines[1L]), "---")) {
    return(NA_integer_)
  }
  for (i in 2:min(length(lines), 2000L)) {
    if (identical(trimws(lines[i]), "---")) return(i)
  }
  NA_integer_
}

#' Insert or update `source_url:` in YAML front matter (non-creating)
#'
#' Only operates when the document has a well-formed YAML front matter block.
#' If there is no such block, the input text is returned unchanged.
#'
#' @param text Character scalar; contents of a `.qmd` (or other) file.
#' @param source_url Character scalar; URL to insert/replace under `source_url:`.
#'
#' @return Character scalar; modified text if front matter was present, otherwise
#'   the original text unchanged.
#' @keywords internal
#' @noRd
.upsert_source_url <- function(text, source_url) {
  if (is.null(source_url) || !nzchar(source_url)) {
    return(text)
  }
  end <- .front_matter_end(text)
  if (is.na(end)) {
    return(text)
  } # do not create front matter

  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  # search existing key within the YAML block (lines 2..end-1)
  replaced <- FALSE
  if (end > 2L) {
    for (i in 2:(end - 1L)) {
      if (startsWith(trimws(lines[i]), "source_url:")) {
        lines[i] <- sprintf("source_url: %s", source_url)
        replaced <- TRUE
        break
      }
    }
  }
  if (!replaced) {
    lines <- append(
      lines,
      values = sprintf("source_url: %s", source_url),
      after = end - 1L
    )
  }
  out <- paste0(lines, collapse = "\n")
  if (!endsWith(out, "\n")) {
    out <- paste0(out, "\n")
  }
  out
}

#' Write a .source sidecar file containing a URL
#'
#' @param dest Character; destination of the main file.
#' @param url Character; URL to write into the sidecar.
#' @return Invisibly returns the sidecar path.
#' @keywords internal
#' @noRd
.write_source_sidecar <- function(dest, url) {
  side <- paste0(dest, ".source")
  dir.create(dirname(side), recursive = TRUE, showWarnings = FALSE)
  con <- file(side, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(url, con, useBytes = TRUE)
  invisible(side)
}
