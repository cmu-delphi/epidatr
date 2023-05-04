test_that("covidcast", {
  covidcast_api <- covidcast_epidata()
  expect_equal(
    covidcast_api$sources$`fb-survey`$signals$smoothed_cli$call("nation", "us", epirange(20210405, 20210410)),
    covidcast("fb-survey", "smoothed_cli", "nation", "day", "us", epirange(20210405, 20210410))
  )
})
