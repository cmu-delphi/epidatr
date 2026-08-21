# Gate for tests that make real network calls to the Delphi Epidata API.
# Default `devtools::test()` does NOT run these. Use `make test-live`, which
# sets EPIDATR_LIVE_TEST=TRUE, to enable.
#
# We use a package-specific env var (not NOT_CRAN) because testthat's
# skip_on_cran() also enables tests in interactive R sessions — too leaky.
skip_unless_live <- function() {
  testthat::skip_on_cran()
  if (!isTRUE(as.logical(Sys.getenv("EPIDATR_LIVE_TEST")))) {
    testthat::skip("set EPIDATR_LIVE_TEST=TRUE to run live tests")
  }
  if (!curl::has_internet()) {
    testthat::skip("no internet")
  }
  if (Sys.getenv("DELPHI_EPIDATA_KEY") == "") {
    testthat::skip("DELPHI_EPIDATA_KEY not set")
  }
}

# Gate for pvt_* endpoints, which need an API key with private-endpoint
# access. Skip them with `make test-live pvt=FALSE` (sets EPIDATR_TEST_PVT).
skip_unless_pvt <- function() {
  if (isFALSE(as.logical(Sys.getenv("EPIDATR_TEST_PVT", "TRUE")))) {
    testthat::skip("EPIDATR_TEST_PVT=FALSE: skipping private endpoints")
  }
}

# Live contract check for one endpoint_calls() row: the endpoint returns
# non-empty data, the fetch is warning-free (so schema drift surfacing as
# epidatr__missing_meta_fields or epidatr__int_nonzero_decimal_digits fails the
# test), and each column has the class its field metadata promises.
expect_live_call_parses <- function(thunk) {
  call <- thunk(fetch_args_list(dry_run = TRUE))
  result <- NULL
  testthat::expect_no_warning(result <- thunk(fetch_args_list()))
  if (inherits(result, "data.frame")) {
    testthat::expect_gt(nrow(result), 0)
    expect_meta_classes(result, call$meta) # nolint: object_usage_linter.
  } else {
    # classic list endpoints (pub_delphi, pub_meta, pvt_meta_norostat)
    testthat::expect_gt(length(result), 0)
  }
}
