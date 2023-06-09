test_that("request_impl http errors", {
  # should give a 401 error
  epidata_call <- pvt_cdc(auth = "ImALittleTeapot", epiweeks = epirange(202003, 202304), locations = "ma")
  local_mocked_bindings(
    # generated via
    # url <- full_url(epidata_call)
    # params <- request_arguments(epidata_call, "csv", NULL)
    # result <- do_request(url, params) %>% readr::write_rds(testthat::test_path("data/test-http401.rds"))
    do_request = function(...) readRDS(testthat::test_path("data/test-http401.rds")),
  )
  expect_error(response <- epidata_call %>% request_impl("csv"), class = "http_401")

  # should give a 500 error (the afhsb endpoint is removed)

  # generated via
  # epidata_call <- pvt_afhsb(auth = Sys.getenv("SECRET_API_AUTH_AFHSB"), locations = "mn", epiweeks = epirange(202002, 202110), flu_types = "flu1")
  # url <- full_url(epidata_call)
  # params <- request_arguments(epidata_call, "csv", NULL)
  # response <- do_request(url, params) %>% readr::write_rds(testthat::test_path("data/test-http500.rds"))
  local_mocked_bindings(
    do_request = function(...) readRDS(testthat::test_path("data/test-http500.rds"))
  )
  expect_error(response <- epidata_call %>% request_impl("csv"), class = "http_500")
})

test_that("fetch and fetch_tbl", {
  epidata_call <- covidcast(
    data_source = "jhu-csse",
    signals = "confirmed_7dav_incidence_prop",
    time_type = "day",
    geo_type = "state",
    time_values = epirange("2020-06-01", "2020-08-01"),
    geo_values = "ca,fl"
  )
  local_mocked_bindings(
    request_impl = function(...) NULL,
    .package = "epidatr"
  )
  local_mocked_bindings(
    # RDS file generated with
    # epidata_call %>%
    # fetch_debug(format_type = "classic") %>%
    # readr::write_rds(testthat::test_path("data/test-classic.rds"))
    content = function(...) readRDS(testthat::test_path("data/test-classic.rds")),
    .package = "httr"
  )

  tbl_out <- epidata_call %>% fetch_tbl()
  out <- epidata_call %>% fetch()
  expect_identical(out, tbl_out)

  local_mocked_bindings(
    # RDS file generated with
    # epidata_call %>%
    # fetch_debug(format_type = "classic", fields = c("time_value", "value")) %>%
    # readr::write_rds(testthat::test_path("data/test-narrower-fields.rds"))
    content = function(...) readRDS(testthat::test_path("data/test-narrower-fields.rds")),
    .package = "httr"
  )
  # testing that the fields fill as expected
  res <- epidata_call %>% fetch(fields = c("time_value", "value"))
  expect_equal(res, tbl_out[c("time_value", "value")])
})

test_that("fetch_tbl warns on non-success", {
  epidata_call <- covidcast(
    data_source = "jhu-csse",
    signals = "confirmed_7dav_incidence_prop",
    time_type = "day",
    geo_type = "state",
    time_values = epirange("2020-06-01", "2020-08-01"),
    geo_values = "ca,fl"
  )

  local_mocked_bindings(
    request_impl = function(...) NULL,
    .package = "epidatr"
  )
  local_mocked_bindings(
    content = function(...) NULL,
    .package = "httr"
  )
  # TODO: Turn these tests back on, when the API is fully online
  # Remove on 2023-06-21
  # artificial_warning <- "* This is a warning with a leading asterisk and {braces} to make sure we don't have bulleting/glue bugs."
  # debug_triplet <- readRDS(testthat::test_path("data/test-classic.rds")) %>%
  #   jsonlite::fromJSON() %>%
  #   `[[<-`("message", artificial_warning)
  # local_mocked_bindings(
  #   # see generation code above
  #   fromJSON = function(...) debug_triplet,
  #   .package = "jsonlite"
  # )

  # expect_warning(epidata_call %>% fetch_tbl(),
  #   regexp = paste0("epidata warning: ", artificial_warning),
  #   fixed = TRUE
  # )
})

test_that("classic only fetch", {
  # delphi is an example endpoint that only suports the classic call
  epidata_call <- delphi(system = "ec", epiweek = 201501)
  local_mocked_bindings(
    # generated using
    # epidata_call %>%
    #   fetch_debug(format_type = "classic") %>%
    #   readr::write_rds(testthat::test_path("data/test-classic-only.rds"))
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
