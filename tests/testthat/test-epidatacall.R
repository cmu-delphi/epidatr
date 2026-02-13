test_that("do_request http errors", {
  # should give a 401 error
  epidata_call <- pvt_cdc(
    auth = "ImALittleTeapot",
    epiweeks = epirange(202003, 202304),
    locations = "ma",
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
  local_mocked_bindings(
    req_perform = function(...) {
      create_mock_response(
        "<p>API key does not exist.</p>",
        status_code = 401L,
        headers = list("content-type" = "text/html")
      )
    },
    .package = "httr2"
  )
  expect_error(
    epidata_call %>% do_request("csv", timeout_seconds = 30, fields = NULL),
    class = "httr2_http_401"
  )

  # should give a 500 error (the afhsb endpoint is removed)

  local_mocked_bindings(
    req_perform = function(...) {
      create_mock_response(
        '{"epidata": [], "message": "database error", "result": -1}',
        status_code = 500L
      )
    },
    .package = "httr2"
  )
  expect_error(
    epidata_call %>% do_request("csv", timeout_seconds = 30, fields = NULL),
    class = "httr2_http_500"
  )
})

test_that("fetch_args", {
  expect_snapshot_value(fetch_args_list(), style = "json2", cran = TRUE)
  expect_snapshot_value(
    fetch_args_list(
      fields = c("a", "b"),
      disable_date_parsing = TRUE,
      disable_data_frame_parsing = TRUE,
      return_empty = TRUE,
      timeout_seconds = 10,
      base_url = "https://example.com",
      dry_run = TRUE,
      refresh_cache = TRUE,
      reference_week_day = 1
    ),
    style = "json2",
    cran = TRUE
  )
})

test_that("fetch non-classic passes along api warnings", {
  epidata_call <- pub_covidcast(
    source = "jhu-csse",
    signals = "confirmed_7dav_incidence_prop",
    time_type = "day",
    geo_type = "state",
    time_values = epirange("2020-06-01", "2020-08-01"),
    geo_values = "ca,fl",
    fetch_args = fetch_args_list(dry_run = TRUE)
  )

  artificial_warning <- paste0(
    "* This is a warning with a leading asterisk and {braces}",
    " to make sure we don't have bulleting/glue bugs."
  )
  mock_response <- list(
    epidata = list(list(
      source = "jhu-csse", signal = "confirmed_7dav_incidence_prop",
      geo_type = "state", time_type = "day", geo_value = "ca",
      time_value = 20200601L, issue = 20200602L, lag = 1L,
      value = 1.5, stderr = 0.1, sample_size = 100.0,
      direction = 1.0, missing_value = 0L, missing_stderr = 0L,
      missing_sample_size = 0L
    )),
    result = 1,
    message = artificial_warning
  )
  local_mocked_bindings(
    do_request = function(...) as.character(jsonlite::toJSON(mock_response, auto_unbox = TRUE)),
    .package = "epidatr"
  )

  expect_snapshot(epidata_call %>% fetch(), cran = TRUE)
})

test_that("fetch classic works", {
  # Minimal example
  mock_classic <- list(
    epidata = list(list(
      epiweek = 201501,
      forecast = list(
        `_version` = 1,
        baselines = list(nat = 2.0),
        data = list()
      )
    )),
    result = 1,
    message = "success"
  )

  local_mocked_bindings(
    do_request = function(...) as.character(jsonlite::toJSON(mock_classic, auto_unbox = TRUE)),
    .package = "epidatr"
  )

  # pub_delphi calls request_epidata directly; make sure the return is a list
  fetch_out <- pub_delphi(
    system = "ec",
    epiweek = 201501
  )
  expect_true(inherits(fetch_out, "list"))
  expect_snapshot_value(fetch_out, style = "json2", cran = TRUE)
})

test_that("create_epidata_call basic behavior", {
  endpoint <- "endpoint"
  base_url <- "https://api.delphi.cmu.edu/epidata/"
  params <- list()

  # Success
  meta <- list(
    create_epidata_field_info("time_value", "date"),
    create_epidata_field_info("value", "float")
  )

  formatted_params <- format_params_for_api(params)

  r <- httr2::request(global_base_url) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_url_query(!!!formatted_params)

  expected <- list(
    request = r,
    base_url = base_url,
    meta = meta
  )
  class(expected) <- "epidata_call"
  expect_identical(create_epidata_call(endpoint, params, meta = meta), expected)

  expected$meta <- list()
  expect_identical(create_epidata_call(endpoint, params, meta = NULL), expected)
  expect_identical(create_epidata_call(endpoint, params, meta = list()), expected)
})


test_that("create_epidata_call fails when meta arg contains duplicates", {
  endpoint <- "endpoint"
  params <- list()

  # Duplicate names
  meta <- list(
    create_epidata_field_info("time_value", "date"),
    create_epidata_field_info("time_value", "int")
  )
  expect_error(
    create_epidata_call(endpoint, params, meta = meta),
    class = "epidatr__duplicate_meta_names"
  )

  # Duplicate entries
  meta <- list(
    create_epidata_field_info("time_value", "date"),
    create_epidata_field_info("time_value", "date")
  )
  expect_error(
    create_epidata_call(endpoint, params, meta = meta),
    class = "epidatr__duplicate_meta_entries"
  )
})

test_that("with_base_url works as expected", {
  # Create a dummy epidata_call
  epidata_call <- pub_covidcast(
    source = "jhu-csse",
    signals = "confirmed_7dav_incidence_prop",
    time_type = "day",
    geo_type = "state",
    time_values = epirange(20200601, 20200801),
    geo_values = "ca",
    fetch_args = fetch_args_list(dry_run = TRUE)
  )

  # Basic replacement
  new_url <- "https://example.com"
  new_call <- with_base_url(epidata_call, new_url)

  expect_s3_class(new_call, "epidata_call")
  expect_match(new_call$request$url, "^https://example.com/covidcast")
  expect_equal(new_call$base_url, "https://example.com/")

  # Replacement with path
  new_url_path <- "https://example.com/api.php"
  new_call_path <- with_base_url(epidata_call, new_url_path)

  expect_s3_class(new_call_path, "epidata_call")
  expect_match(new_call_path$request$url, "^https://example.com/api.php/covidcast")
  expect_equal(new_call_path$base_url, "https://example.com/api.php/")
  # Ensure query params are preserved (rough check)
  expect_match(new_call_path$request$url, "data_source=jhu-csse")
})

test_that("fetch_args_list triggers deprecation warnings for debug and format_type", {
  expect_warning(fetch_args_list(debug = TRUE), "The `debug` argument is no longer supported")
  expect_warning(fetch_args_list(format_type = "json"), "The `format_type` argument is now managed internally")
})
