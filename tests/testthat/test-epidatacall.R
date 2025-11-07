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
  expect_error(
    response <- epidata_call %>%
      request_impl("csv", timeout_seconds = 30, fields = NULL),
    class = "http_401"
  )

  # should give a 500 error (the afhsb endpoint is removed)

  # see generate_test_data.R
  local_mocked_bindings(
    do_request = function(...) readRDS(testthat::test_path("data/test-http500.rds"))
  )
  expect_error(
    response <- epidata_call %>%
      request_impl("csv", timeout_seconds = 30, fields = NULL),
    class = "http_500"
  )
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
        timeout_seconds = 15 * 60,
        base_url = NULL,
        dry_run = FALSE,
        debug = FALSE,
        format_type = "json",
        refresh_cache = FALSE
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
      format_type = "classic",
      refresh_cache = TRUE
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
        format_type = "classic",
        refresh_cache = TRUE
      ),
      class = "fetch_args"
    )
  )
})

test_that("fetch non-classic works", {
  # only_supports_classic is FALSE
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
  local_mocked_bindings(
    # see generate_test_data.R
    content = function(...) readRDS(testthat::test_path("data/test-narrower-fields.rds")),
    .package = "httr"
  )

  # testing that the fields fill as expected
  out <- epidata_call %>% fetch()
  res <- epidata_call %>% fetch(fetch_args_list(fields = c("time_value", "value")))
  expect_equal(res, out[c("time_value", "value")])
})

test_that("fetch non-classic passes along api warnings", {
  # only_supports_classic is FALSE
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

  expect_warning(epidata_call %>% fetch(),
    regexp = paste0("epidata warning: `", artificial_warning, "`"),
    fixed = TRUE
  )
})

test_that("fetch classic works", {
  # only_supports_classic is TRUE
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

  # make sure the return from this is a list
  fetch_out <- epidata_call %>% fetch()
  expect_true(inherits(fetch_out, "list"))
})

test_that("create_epidata_call basic behavior", {
  endpoint <- "endpoint"
  params <- list()

  # Success
  meta <- list(
    create_epidata_field_info("time_value", "date"),
    create_epidata_field_info("value", "float")
  )
  expected <- list(
    endpoint = endpoint,
    params = params,
    base_url = "https://api.delphi.cmu.edu/epidata/",
    meta = meta,
    only_supports_classic = FALSE
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
