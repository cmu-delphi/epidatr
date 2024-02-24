test_that("requesting works", {
  res <- do_request("https://httpbin.org/status/414", list(), timeout_seconds = 10 * 60)
  expect_equal(res$status_code, 414)
})
