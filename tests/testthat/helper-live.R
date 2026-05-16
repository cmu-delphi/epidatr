# Gate for tests that make real network calls to the Delphi Epidata API.
# Default `devtools::test()` does NOT run these. Use `make test-live`, which
# sets EPIDATR_LIVE_TEST=1, to enable.
#
# We use a package-specific env var (not NOT_CRAN) because testthat's
# skip_on_cran() also enables tests in interactive R sessions — too leaky.
skip_unless_live <- function() {
  testthat::skip_on_cran()
  if (!isTRUE(as.logical(Sys.getenv("EPIDATR_LIVE_TEST")))) {
    testthat::skip("set EPIDATR_LIVE_TEST=1 to run live tests")
  }
  if (!curl::has_internet()) testthat::skip("no internet")
  if (Sys.getenv("DELPHI_EPIDATA_KEY") == "") {
    testthat::skip("DELPHI_EPIDATA_KEY not set")
  }
}
