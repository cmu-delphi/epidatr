test_that("fetch_tbl", {
  epidata_call <- covidcast(
    data_source = "jhu-csse",
    signals = "confirmed_7dav_incidence_prop",
    time_type = "day",
    geo_type = "state",
    time_values = epirange("2020-06-01", "2020-08-01"),
    geo_values = "ca,fl"
  )
  # Generated with
  # epidata_call %>%
  # fetch_debug(format_type = "classic") %>%
  # readr::write_rds(testthat::test_path("data/test-classic.rds"))
  mockery::stub(fetch_classic, "httr::content", readRDS(testthat::test_path("data/test-classic.rds")))
  mockery::stub(fetch_tbl, "fetch_classic", fetch_classic)
  # Generated with
  # epidata_call %>%
  # fetch_debug(format_type = "csv") %>%
  # readr::write_rds(testthat::test_path("data/test-csv.rds"))
  mockery::stub(fetch_csv, "httr::content", readRDS(testthat::test_path("data/test-csv.rds")))

  # This test compares the output of a tibble using fetch_tbl and fetch_csv.
  #
  # 1) fetch_tbl calls fetch_classic, which requests the default (classic
  # format) from the API, uses jsonlite::fromJSON to convert the underlying data
  # to a data.frame, and finally applies parse_data_frame is used to do the data
  # type coersion specified by the epidata_call metadata.
  # 2) fetch_csv requests the csv format from the API, then uses readr::read_csv
  # to get a data.frame, and has its own methods to enforce data types.
  tbl_out <- epidata_call %>% fetch_tbl()
  csv_out <- epidata_call %>% fetch_csv()
  expect_identical(tbl_out, csv_out)

  # # This test compares fetch_tbl with the output of fetch, which should be identical.
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
  artificial_warning <- "* This is a warning with a leading asterisk and {braces} to make sure we don't have bulleting/glue bugs."
  debug_response_content_triplet <-
    # see generation code above
    readRDS(testthat::test_path("data/test-classic.rds")) %>%
    jsonlite::fromJSON() %>%
    `[[<-`("message", "* This is a warning with a leading asterisk and {braces} to make sure we don't have bulleting/glue bugs.")
  mockery::stub(fetch_classic, "jsonlite::fromJSON", debug_response_content_triplet)
  mockery::stub(fetch_tbl, "fetch_classic", fetch_classic)
  expect_warning(epidata_call %>% fetch_tbl(),
    regexp = paste0("epidata warning: ", artificial_warning),
    fixed = TRUE
  )
})
