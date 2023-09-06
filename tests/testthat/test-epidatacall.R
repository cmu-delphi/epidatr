test_that("request_impl http errors", {
  # should give a 401 error
  epidata_call <- pvt_cdc(
    auth = "ImALittleTeapot",
    epiweeks = epirange(202003, 202304),
    locations = "ma",
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
  local_mocked_bindings(
    # see generate_test_data.R
    do_request = function(...) readRDS(testthat::test_path("data/test-http401.rds")),
  )
  expect_error(response <- epidata_call %>% request_impl("csv"), class = "http_401")

  # should give a 500 error (the afhsb endpoint is removed)

  # see generate_test_data.R
  local_mocked_bindings(
    do_request = function(...) readRDS(testthat::test_path("data/test-http500.rds"))
  )
  expect_error(response <- epidata_call %>% request_impl("csv"), class = "http_500")
})

test_that("fetch_args", {
  expect_identical(
    fetch_args_list(),
    structure(
      list(
        fields = NULL,
        disable_date_parsing = FALSE,
        disable_data_frame_parsing = FALSE,
        return_empty = FALSE,
        timeout_seconds = 30,
        base_url = NULL,
        dry_run = FALSE,
        debug = FALSE,
        format_type = "json"
      ),
      class = "fetch_args"
    )
  )
  expect_identical(
    fetch_args_list(
      fields = c("a", "b"),
      disable_date_parsing = TRUE,
      disable_data_frame_parsing = TRUE,
      return_empty = TRUE,
      timeout_seconds = 10,
      base_url = "https://example.com",
      dry_run = TRUE,
      debug = TRUE,
      format_type = "classic"
    ),
    structure(
      list(
        fields = c("a", "b"),
        disable_date_parsing = TRUE,
        disable_data_frame_parsing = TRUE,
        return_empty = TRUE,
        timeout_seconds = 10,
        base_url = "https://example.com",
        dry_run = TRUE,
        debug = TRUE,
        format_type = "classic"
      ),
      class = "fetch_args"
    )
  )
})

test_that("fetch and fetch_tbl", {
  epidata_call <- pub_covidcast(
    source = "jhu-csse",
    signals = "confirmed_7dav_incidence_prop",
    time_type = "day",
    geo_type = "state",
    time_values = epirange("2020-06-01", "2020-08-01"),
    geo_values = "ca,fl",
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
  local_mocked_bindings(
    request_impl = function(...) NULL,
    .package = "epidatr"
  )
  local_mocked_bindings(
    # see generate_test_data.R
    content = function(...) readRDS(testthat::test_path("data/test-classic.rds")),
    .package = "httr"
  )

  tbl_out <- epidata_call %>% fetch_tbl()
  out <- epidata_call %>% fetch()
  expect_identical(out, tbl_out)

  local_mocked_bindings(
    # see generate_test_data.R
    content = function(...) readRDS(testthat::test_path("data/test-narrower-fields.rds")),
    .package = "httr"
  )
  # testing that the fields fill as expected
  res <- epidata_call %>% fetch(fetch_args_list(fields = c("time_value", "value")))
  expect_equal(res, tbl_out[c("time_value", "value")])
})

test_that("fetch_tbl warns on non-success", {
  epidata_call <- pub_covidcast(
    source = "jhu-csse",
    signals = "confirmed_7dav_incidence_prop",
    time_type = "day",
    geo_type = "state",
    time_values = epirange("2020-06-01", "2020-08-01"),
    geo_values = "ca,fl",
    fetch_args = fetch_args_list(dry_run = TRUE)
  )

  local_mocked_bindings(
    request_impl = function(...) NULL,
    .package = "epidatr"
  )
  local_mocked_bindings(
    content = function(...) NULL,
    .package = "httr"
  )
  artificial_warning <- paste0(
    "* This is a warning with a leading asterisk and {braces}",
    " to make sure we don't have bulleting/glue bugs."
  )
  debug_triplet <- readRDS(testthat::test_path("data/test-classic.rds")) %>%
    jsonlite::fromJSON() %>%
    `[[<-`("message", artificial_warning)
  local_mocked_bindings(
    # see generate_test_data.R
    fromJSON = function(...) debug_triplet,
    .package = "jsonlite"
  )

  expect_warning(epidata_call %>% fetch_tbl(),
    regexp = paste0("epidata warning: `", artificial_warning, "`"),
    fixed = TRUE
  )
})

test_that("classic only fetch", {
  # delphi is an example endpoint that only suports the classic call
  epidata_call <- pub_delphi(
    system = "ec",
    epiweek = 201501,
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
  local_mocked_bindings(
    # see generate_test_data.R
    content = function(...) readRDS(testthat::test_path("data/test-classic-only.rds")),
    .package = "httr"
  )
  # make sure that fetch actually uses the classic method on endpoints that only support the classic
  fetch_out <- epidata_call %>% fetch()
  fetch_classic_out <- epidata_call %>% fetch_classic()
  expect_identical(fetch_out, fetch_classic_out)

  # making sure that fetch_tbl and throws the expected error on classic only
  expect_error(epidata_call %>% fetch_tbl(), class = "only_supports_classic_format")
})
