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
  # fetch_debug(format="classic") %>%
  # write_rds(testthat::test_path("data/test-classic.rds"))
  mockery::stub(fetch_classic, "httr::content", readRDS(testthat::test_path("data/test-classic.rds")))
  # Generated with
  # epidata_call %>%
  # fetch_debug(format="csv") %>%
  # write_rds(testthat::test_path("data/test-csv.rds"))
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

  # This test compares fetch_tbl with the output of fetch, which should be identical.
  out <- epidata_call %>% fetch()
  expect_identical(out, tbl_out)
})
