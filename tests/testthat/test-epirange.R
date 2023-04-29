test_that("epirange", {
  expect_error(epirange(1, 2, 3), "unused argument")
  expect_no_error(epirange(1, 2))
  expect_no_error(epirange(1, "2"))
  expect_no_error(epirange("1", "2"))
  expect_no_error(epirange(as.Date("2020-01-01"), as.Date("2020-01-02")))
})
