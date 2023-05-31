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
  artificial_warning <- "* This is a warning with a leading asterisk and {braces} to make sure we don't have bulleting/glue bugs."  
  debug_triplet <- readRDS(testthat::test_path("data/test-classic.rds")) %>%
                              jsonlite::fromJSON() %>%
                              `[[<-`("message", artificial_warning)
  local_mocked_bindings(
    # see generation code above
    fromJSON = function(...) debug_triplet,
    .package = "jsonlite"
  )

  expect_warning(epidata_call %>% fetch_tbl(),
    regexp = paste0("epidata warning: ", artificial_warning),
      fixed = TRUE
  )
})
