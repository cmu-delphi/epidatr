test_that("covidcast", {
  # covidcast-meta.json is the real covidcast/meta response trimmed to the
  # fb-survey source and its smoothed_cli signal, so this runs offline.
  meta <- readChar(
    test_path("fixtures", "covidcast-meta.json"),
    file.size(test_path("fixtures", "covidcast-meta.json"))
  )
  covidcast_api <- with_mocked_response(meta, epidatr::covidcast_epidata()) # nolint: object_usage_linter.
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
