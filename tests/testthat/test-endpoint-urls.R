# Snapshot the constructed request URL for every endpoint_calls() row
# (helper-endpoints.R). Runs offline via dry_run: catches accidental changes to
# parameter names, value formatting (dates, epiweeks, wildcards), and routing.
test_that("endpoint request URLs are stable", {
  rows <- endpoint_calls(auth = "test-auth-key")
  lines <- unlist(lapply(rows, function(row) {
    call <- row$call(fetch_args_list(dry_run = TRUE))
    # multi-signal cast dry runs return a list of calls
    calls <- if (inherits(call, "epidata_call")) list(call) else call
    urls <- vapply(calls, function(x) x$request$url, character(1))
    paste0(row$name, "\n  ", urls)
  }))
  expect_snapshot(cat(lines, sep = "\n"))
})
