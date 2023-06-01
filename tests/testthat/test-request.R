test_that("requesting works", {
  res <- do_request("https://httpbin.org/status/414", list())
  expect_equal(res$status_code, 414)
})
