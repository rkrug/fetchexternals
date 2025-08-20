#' Access or write the bundled `externals.json` template
#'
#' @description
#' Returns or writes the package's template manifest located at
#' `inst/extdata/externals.json`.
#'
#' - If `dest` is `NULL`, the template JSON is read and returned
#'   as an R list (no files are written).
#' - If `dest` is a file path, the template is copied there (creating parent
#'   directories as needed). Use `overwrite = TRUE` to replace an existing file.
#' - If `dest` is missing, an error is raised.
#'
#' @param dest `NULL` or a character scalar file path.
#'   - `NULL`: return the parsed template as a list.
#'   - character path (e.g., `"externals.json"`): write the template to that path.
#' @param overwrite Logical. Overwrite `dest` if it already exists? Default `FALSE`.
#'
#' @section Manifest structure:
#' The manifest is a JSON object with two top-level keys:
#'
#' - **`"repos"`** (array of objects): required. Each entry configures one source repository.
#' - **`"defaults"`** (object): optional. Provides default behavior for all repos unless
#'   overridden per-repo.
#'
#' ### Repositories (`repos[]`)
#' Each repository entry supports the following fields:
#'
#' - `name` (string, optional): Human-readable identifier used only for organization.
#' - `repo` (string, **required**): GitHub repository in `"owner/name"` form,
#'   e.g., `"example-org/project-a"`.
#' - `ref` (string, optional; default: `"latest"`): The Git reference to fetch from.
#'   Allowed values:
#'   - A tag (e.g., `"v1.2.3"`),
#'   - A branch name (e.g., `"main"`),
#'   - A commit SHA,
#'   - Or the string `"latest"` meaning “use the latest non-draft, non-prerelease
#'     GitHub release tag”. If no release exists and fallback is enabled (see `defaults`),
#'     the repository’s default branch is used.
#' - `api_env` (string, optional): Name of the environment variable that holds the
#'   GitHub API token to use **for this repository** (e.g., `"PROJECTA_TOKEN"`). If omitted
#'   or empty, your package’s token discovery (e.g., `GITHUB_TOKEN`) is used.
#' - `include_prereleases` (logical, optional): If `TRUE` and `ref = "latest"`, allow
#'   prereleases to be selected as the “latest”. Default inherited from `defaults`,
#'   otherwise `FALSE`.
#' - `fallback_to_default_branch` (logical, optional): If `TRUE` and no release is found,
#'   fall back to the repo’s default branch when `ref = "latest"`. Default inherited from
#'   `defaults`, otherwise `TRUE`.
#' - `files` (array of objects, **required**): The files to fetch from this repo. Each item:
#'   - `path` (string, **required**): Path inside the repo (e.g., `"docs/page.qmd"`).
#'   - `dest` (string, **required**): Destination path in your project (e.g.,
#'     `"external/ProjectA/page.qmd"`). Parent directories will be created.
#'   - `source_url` (string, **required**): How to record the canonical source URL
#'     for this file. Allowed values (exact strings):
#'       - `"frontmatter"` — Insert or update a `source_url:` key **only if** the file has a
#'         **well-formed YAML front matter** block (opening `---` and a matching closing `---`).
#'         If no valid front matter is present, the file is left **unchanged**.
#'       - `"file"` — Write the canonical URL to a sidecar file named `<dest>.source`.
#'       - `"none"` — Do not record the source URL.
#'
#'   The actual URL string recorded (when mode is `"frontmatter"` or `"file"`)
#'   is typically computed by your fetch function as:
#'   `https://github.com/{owner}/{repo}/blob/{resolved-ref}/{path}`.
#'
#' ### Defaults (`defaults`)
#' All fields here are optional; they provide global defaults that per-repo settings can override:
#'
#' - `include_prereleases` (logical; default `FALSE`): If `TRUE`, allow prereleases to be chosen
#'   when resolving `"latest"`.
#' - `fallback_to_default_branch` (logical; default `TRUE`): If `TRUE`, use the repo’s default
#'   branch when no release is available and `ref = "latest"`.
#'
#' @section Example manifest:
#' ```json
#' {
#'   "repos": [
#'     {
#'       "name": "ProjectA",
#'       "repo": "example-org/project-a",
#'       "ref": "latest",
#'       "api_env": "PROJECTA_TOKEN",
#'       "files": [
#'         { "path": "docs/page.qmd",  "dest": "external/ProjectA/page.qmd",  "source_url": "frontmatter" },
#'         { "path": "docs/notes.txt", "dest": "external/ProjectA/notes.txt", "source_url": "file" },
#'         { "path": "static/logo.svg","dest": "external/ProjectA/logo.svg",  "source_url": "none" }
#'       ]
#'     },
#'     {
#'       "name": "ProjectB",
#'       "repo": "example-org/project-b",
#'       "ref": "v1.0.0",
#'       "files": [
#'         { "path": "reports/summary.qmd", "dest": "external/ProjectB/summary.qmd", "source_url": "frontmatter" }
#'       ]
#'     }
#'   ],
#'   "defaults": {
#'     "fallback_to_default_branch": true,
#'     "include_prereleases": false
#'   }
#' }
#' ```
#'
#' @return
#' - If `dest` is `NULL`: the template manifest as a list.
#' - Otherwise (when writing): invisibly, the normalized destination path (character).
#'
#' @examples
#' \dontrun{
#' # Get the template as an R list (no file written):
#' tpl <- manifest_template(NULL)
#' str(tpl)
#'
#' # Write the template into the project root:
#' manifest_template("externals.json")
#'
#' # Write into a subfolder, creating it if needed:
#' manifest_template("config/externals.json")
#'
#' # Overwrite an existing file:
#' manifest_template("externals.json", overwrite = TRUE)
#' }
#'
#' @importFrom utils packageName
#' @importFrom jsonlite fromJSON
#' @export
manifest_template <- function(dest, overwrite = FALSE) {
  # Error if dest is missing
  if (missing(dest)) {
    stop(
      "`dest` must be provided. Use NULL to return the template as a list.",
      call. = FALSE
    )
  }

  # locate the shipped template inside the installed package
  tpl <- system.file(
    "extdata",
    "externals.json",
    package = "fetchexternals"
  )
  if (!nzchar(tpl) || !file.exists(tpl)) {
    stop(
      "Template externals.json not found in the installed package. ",
      "Reinstall the package or ensure inst/extdata/externals.json exists.",
      call. = FALSE
    )
  }

  # If dest is NULL, return parsed JSON as list
  if (is.null(dest)) {
    return(jsonlite::fromJSON(tpl, simplifyVector = FALSE))
  }

  # Validate dest
  if (!is.character(dest) || length(dest) != 1L) {
    stop("`dest` must be a single character file path or NULL.", call. = FALSE)
  }

  # Ensure parent directory exists
  parent <- dirname(dest)
  if (!dir.exists(parent)) {
    dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  }

  # Overwrite handling
  if (file.exists(dest) && !overwrite) {
    stop(
      sprintf(
        "Destination already exists: %s (set `overwrite = TRUE` to replace).",
        dest
      ),
      call. = FALSE
    )
  }

  ok <- file.copy(from = tpl, to = dest, overwrite = TRUE)
  if (!ok) {
    stop(sprintf("Failed to write template to: %s", dest), call. = FALSE)
  }

  invisible(normalizePath(dest, winslash = "/", mustWork = FALSE))
}
