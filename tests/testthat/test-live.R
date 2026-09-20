# Live API tests. Each test calls skip_unless_live(); the suite only runs
# under `make test-live` (which sets EPIDATR_LIVE_TEST=TRUE).
# DELPHI_EPIDATA_KEY must be in the environment for the pvt_* endpoints.
#
# The classic endpoints are driven by the endpoint_calls() table in
# helper-endpoints.R (shared with the URL snapshot tests). Each row asserts,
# via expect_live_call_parses(): non-empty results, a warning-free fetch, and
# column classes matching the endpoint's field metadata.

for (endpoint_row in endpoint_calls(auth = Sys.getenv("DELPHI_EPIDATA_KEY"))) {
  local({
    row <- endpoint_row
    if (!row$live) {
      return()
    }
    test_that(paste0("live: ", row$name), {
      skip_unless_live()
      if (startsWith(row$name, "pvt")) {
        skip_unless_pvt()
      }
      expect_live_call_parses(row$call)
    })
  })
}
