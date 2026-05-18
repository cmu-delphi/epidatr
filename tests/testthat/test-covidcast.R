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
  local_mocked_bindings(
    req_perform = function(...) {
      create_mock_response("", status_code = 400L, headers = list("content-type" = "text/html"))
    },
    .package = "httr2"
  )
  expect_error(epidatr::covidcast_epidata(), class = "httr2_http_400")
})
