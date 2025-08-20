## **This package was created with ChatGPT 5 via the chat doing the heavy lifting!**

## **If you are not feeling comfortable with this - no bad feelings from my side!**

## This is pre-alph version and not ready for production use AT ALL!

# fetchexternals

Fetch external files from GitHub repositories into a local project.

## Overview

The `fetchexternals` package provides a helper function `fetch_externals()` that
pulls Quarto (`.qmd`) files from multiple GitHub repositories into your site.
The function:

- Reads a JSON manifest (`externals.json`) describing repositories and files.
- Resolves which ref to use (latest release by default, with prerelease and
  default-branch fallback options).
- Downloads the requested files via the GitHub Contents API.
- Caches downloads using HTTP ETags to skip unchanged files.
- Injects a `source_url:` into each page’s YAML front matter so the website can
  render a “View source” button pointing to the authoritative repository.

This allows you to maintain pages in separate repositories, release them
independently, and automatically integrate them into a single unified Quarto site.

## Installation

You can install the development version from GitHub:

You have to figure this out yorself - this is not more then a proof of concept at the moment!

```r

```

## Usage

Create a manifest file `externals.json` in your project root directory and modify it as needed

```r
write_manifest_template("externals.json)
```

You need to adapt that file based on your situation.

Then run in R:

```r
library(fetchexternals)

# Fetch into external/ and update front matter
fetch_externals("externals.json")
```

On subsequent runs, unchanged files are skipped thanks to ETag caching.

## Authentication

- Each repo entry may optionally specify an `"api_env"` field, giving the name
  of an environment variable holding its GitHub token.
- If `"api_env"` is omitted, the function falls back to `GITHUB_TOKEN` and then
  `SITE_A_TOKEN`.
- Providing a token raises rate limits (5000 requests/hour) and enables access
  to private repositories. Without a token, only public repos can be fetched and
  you are limited to ~60 requests/hour.

## License

MIT © Rainer M Krug
