test_that("fetch_tbl", {
  # Generated with
  # covidcast(
  #     data_source = "jhu-csse",
  #     signals = "confirmed_7dav_incidence_prop",
  #     time_type = "day",
  #     geo_type = "state",
  #     time_values = epirange("2020-06-01", "2020-08-01"),
  #     geo_values = "ca,fl"
  # ) %>%
  # fetch_debug(format="classic") %>%
  # write_rds(testthat::test_path("fixtures/test-classic.rds"))
  mockery::stub(fetch_classic, "httr::content", readRDS(testthat::test_path("fixtures/test-classic.rds")))
  # Generated with
  # covidcast(
  #     data_source = "jhu-csse",
  #     signals = "confirmed_7dav_incidence_prop",
  #     time_type = "day",
  #     geo_type = "state",
  #     time_values = epirange("2020-06-01", "2020-08-01"),
  #     geo_values = "ca,fl"
  # ) %>%
  # fetch_debug(format="csv") %>%
  # write_rds(testthat::test_path("fixtures/test-csv.rds"))
  mockery::stub(fetch_csv, "httr::content", readRDS(testthat::test_path("fixtures/test-csv.rds")))

  # This test compares the output of fetch_tbl via classic and csv formats.
  # fetch_classic uses jsonlite::fromJSON, which converts the underlying data to a data.frame.
  # fetch_classic uses parse_data_frame to enforce data types.
  # fetch_csv uses readr::read_csv, which converts the underlying data to a tibble.
  # fetch_csv uses its own approach based on readr to enforce data types.
  classic_out <- covidcast(
    data_source = "jhu-csse",
    signals = "confirmed_7dav_incidence_prop",
    time_type = "day",
    geo_type = "state",
    time_values = epirange("2020-06-01", "2020-08-01"),
    geo_values = "ca,fl"
  ) %>%
    fetch_tbl(method = "data.frame") %>%
    dplyr::mutate(
      geo_type = as.character(geo_type),
      time_type = as.character(time_type)
    )
  csv_out <- covidcast(
    data_source = "jhu-csse",
    signals = "confirmed_7dav_incidence_prop",
    time_type = "day",
    geo_type = "state",
    time_values = epirange("2020-06-01", "2020-08-01"),
    geo_values = "ca,fl"
  ) %>%
    fetch_tbl(method = "csv") %>%
    dplyr::mutate(
      geo_type = as.character(geo_type),
      time_type = as.character(time_type)
    )

  expect_identical(classic_out, csv_out)

  out <- covidcast(
    data_source = "jhu-csse",
    signals = "confirmed_7dav_incidence_prop",
    time_type = "day",
    geo_type = "state",
    time_values = epirange("2020-06-01", "2020-08-01"),
    geo_values = "ca,fl"
  ) %>%
    fetch() %>%
    dplyr::mutate(
      geo_type = as.character(geo_type),
      time_type = as.character(time_type)
    )

  expect_identical(out, classic_out)
})
