# Refresh the recorded API responses in tests/testthat/fixtures/.
# Run from the repo root: make update-fixtures
#
# Pulls the raw response body for each query in fixture_specs()
# (tests/testthat/helper-fixtures.R). After refreshing, rerun
# test-fixtures.R and review the snapshot diffs: changes there are upstream
# contract changes.
devtools::load_all(quiet = TRUE)
source("tests/testthat/helper-fixtures.R")

fixture_dir <- "tests/testthat/fixtures"
dir.create(fixture_dir, showWarnings = FALSE)

for (spec in fixture_specs()) {
  call <- spec$call(fetch_args_list(dry_run = TRUE))
  res <- do_request(
    call,
    format_type = call$response_format,
    timeout_seconds = 300,
    fields = NULL
  )
  path <- file.path(fixture_dir, spec$file)
  writeBin(httr2::resp_body_raw(res), path)
  cat(sprintf(
    "%-24s %8d bytes  %s\n",
    spec$file,
    file.size(path),
    call$request$url
  ))
}
