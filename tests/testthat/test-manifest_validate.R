test_that("manifest_template(NULL) returns a list", {
  tpl <- manifest_template(NULL)
  expect_type(tpl, "list")
  expect_true(is.list(tpl$repos))
})

make_min_valid <- function() {
  # Start from the shipped template if present; otherwise construct a minimal valid manifest
  tpl <- try(manifest_template(NULL), silent = TRUE)
  if (inherits(tpl, "try-error") || is.null(tpl$repos)) {
    tpl <- list(
      repos = list(
        list(
          name = "ProjectA",
          repo = "example-org/project-a",
          ref = "v1.2.3",
          files = list(
            list(
              path = "docs/page.qmd",
              dest = "external/ProjectA/page.qmd",
              source_url = "frontmatter"
            ),
            list(
              path = "docs/notes.txt",
              dest = "external/ProjectA/notes.txt",
              source_url = "file"
            ),
            list(
              path = "static/logo.svg",
              dest = "external/ProjectA/logo.svg",
              source_url = "none"
            )
          )
        )
      ),
      defaults = list(
        fallback_to_default_branch = TRUE,
        include_prereleases = FALSE
      )
    )
  } else {
    # Ensure at least one repo has the required fields in expected shapes
    # (some user-edited template variants may omit optional bits)
    if (length(tpl$repos) == 0L) {
      tpl$repos <- list(
        list(
          name = "ProjectA",
          repo = "example-org/project-a",
          ref = "v1.2.3",
          files = list(
            list(
              path = "docs/page.qmd",
              dest = "external/ProjectA/page.qmd",
              source_url = "frontmatter"
            )
          )
        )
      )
    }
    # guarantee required per-file 'source_url' in the first repo for the test
    if (!is.null(tpl$repos[[1]]$files) && length(tpl$repos[[1]]$files)) {
      if (is.null(tpl$repos[[1]]$files[[1]]$source_url)) {
        tpl$repos[[1]]$files[[1]]$source_url <- "frontmatter"
      }
    }
    # ensure repo has a valid owner/name form
    tpl$repos[[1]]$repo <- "example-org/project-a"
    # ensure ref is a single string
    tpl$repos[[1]]$ref <- "v1.2.3"
  }
  tpl
}

test_that("valid manifest passes (TRUE) and returns empty problems when collecting", {
  m <- make_min_valid()
  expect_silent(suppressWarnings(manifest_validate(m, error_on_false = TRUE)))
  expect_silent(suppressWarnings(manifest_validate(m, error_on_false = FALSE)))
  expect_warning(
    manifest_validate(m, error_on_false = FALSE),
    regexp = "'PROJECTC_TOKEN'"
  )
  probs <- manifest_validate(m, error_on_false = FALSE)
  expect_type(probs, "character")
  expect_length(probs, 0L)
})

test_that("invalid repo format is reported", {
  m <- make_min_valid()
  m$repos[[1]]$repo <- "not-a-valid-repo-name"
  # collect problems
  probs <- manifest_validate(m, error_on_false = FALSE)
  expect_true(any(grepl("repo.*owner/name", probs, ignore.case = TRUE)))
  # stop with aggregated error
  expect_error(
    manifest_validate(m, error_on_false = TRUE),
    "owner/name",
    fixed = FALSE
  )
})

test_that("ref must be a single string if provided", {
  m <- make_min_valid()
  m$repos[[1]]$ref <- c("tag1", "tag2")
  probs <- manifest_validate(m, error_on_false = FALSE)
  expect_true(any(grepl("'ref'.*single string", probs)))
})

test_that("api_env must be a single string when provided", {
  m <- make_min_valid()
  m$repos[[1]]$api_env <- c("A", "B")
  expect_error(
    manifest_validate(m, error_on_false = TRUE),
    "api_env.*single string"
  )
})

test_that("warning is issued if api_env variable is not set", {
  m <- make_min_valid()
  # choose a likely-unused, unique env var name for the test
  var <- paste0("FX_TEST_TOKEN_", as.integer(runif(1, 1e6, 9e6)))
  # ensure it's unset
  old <- Sys.getenv(var, unset = NA_character_)
  if (!is.na(old) && nzchar(old)) {
    # temporarily unset
    Sys.unsetenv(var)
    on.exit(
      {
        Sys.setenv(structure(old, .Names = var))
      },
      add = TRUE
    )
  }
  m$repos[[1]]$api_env <- var
  expect_silent(suppressWarnings(manifest_validate(m, error_on_false = FALSE)))
  expect_silent(suppressWarnings(manifest_validate(m, error_on_false = TRUE)))
  expect_warning(
    manifest_validate(m, error_on_false = FALSE),
    var
  )
})

test_that("files array must exist and be non-empty", {
  m <- make_min_valid()
  m$repos[[1]]$files <- list()
  probs <- manifest_validate(m, error_on_false = FALSE)
  expect_true(any(grepl("non-empty 'files'", probs)))
})

test_that("source_url must be one of frontmatter|file|none", {
  m <- make_min_valid()
  m$repos[[1]]$files[[1]]$source_url <- "invalid-mode"
  probs <- manifest_validate(m, error_on_false = FALSE)
  expect_true(any(grepl("invalid 'source_url'", probs, ignore.case = TRUE)))
  expect_error(
    suppressWarnings(manifest_validate(m, error_on_false = TRUE)),
    "source_url"
  )
})
