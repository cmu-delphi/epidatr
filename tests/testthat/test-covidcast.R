test_that("covidcast", {
  covidcast_api <- epidatr::covidcast_epidata()
  expect_identical(
    covidcast_api$sources$`fb-survey`$signals$smoothed_cli$call(
      "nation",
      "us",
      epirange(20210405, 20210410),
      fetch_args = fetch_args_list(dry_run = TRUE)
    ),
    pub_covidcast(
      "fb-survey",
      "smoothed_cli",
      "nation",
      "day",
      "us",
      epirange(20210405, 20210410),
      fetch_args = fetch_args_list(dry_run = TRUE)
    )
  )
})


test_that("http errors", {
  # see generate_test_data.R
  local_mocked_bindings(
    do_request = function(...) readRDS(testthat::test_path("data/test-do_request-httpbin.rds"))
  )
  expect_error(epidatr::covidcast_epidata(), class = "http_400")
})


test_that("name completion", {
  all_names <- names(epidatr::covidcast_epidata()$signals)
  expect_identical(all_names, all_names)
})
