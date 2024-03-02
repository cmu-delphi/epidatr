test_that("`parse_timeset_input` on valid inputs", {
  # Date-like: we can get out Date vector, or character or integerish YYYYmmdd
  #            format only
  expect_identical(
    parse_timeset_input(as.Date("2018-01-01")),
    as.Date("2018-01-01")
  )
  expect_identical(
    parse_timeset_input(as.Date(c("2018-01-01", "2018-01-02"))),
    as.Date(c("2018-01-01", "2018-01-02"))
  )
  expect_identical(parse_timeset_input("2018-01-01"), "20180101")
  expect_identical(
    parse_timeset_input(c("2018-01-01", "2018-01-02")),
    c("20180101", "20180102")
  )
  expect_identical(parse_timeset_input("20180101"), "20180101")
  expect_identical(
    parse_timeset_input(c("20180101", "20180102")),
    c("20180101", "20180102")
  )
  expect_identical(parse_timeset_input(20180101), 20180101)
  expect_identical(
    parse_timeset_input(c(20180101, 20180102)),
    c(20180101, 20180102)
  )
  # Epiweeks: we can get out character or integerish
  expect_identical(parse_timeset_input("201801"), "201801")
  expect_identical(
    parse_timeset_input(c("201801", "201802")),
    c("201801", "201802")
  )
  expect_identical(parse_timeset_input(201801), 201801)
  expect_identical(
    parse_timeset_input(c(201801, 201802)),
    c(201801, 201802)
  )
  # EpiRanges: aren't changed
  expect_identical(
    epirange(as.Date("2018-01-01"), as.Date("2018-01-05")),
    epirange(as.Date("2018-01-01"), as.Date("2018-01-05"))
  )
  expect_identical(epirange(201801, 201805), epirange(201801, 201805))
  # Wildcard:
  expect_identical(parse_timeset_input("*"), "*")
  # NULL: allow this as a missing argument marker
  expect_identical(parse_timeset_input(NULL), NULL)
})

test_that("null parsing", {
  # parse_data_frame (df[[info$name]] = NULL)-> parse_value
  epidata_call <- pub_flusurv(
    locations = "ca",
    epiweeks = 202001,
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
  # see generate_test_data.R
  mock_df <- as.data.frame(readr::read_rds(testthat::test_path("data/flusurv-epiweeks.rds")))
  metadata <- epidata_call$meta
  mock_df[[metadata[[1]]$name]][1] <- NA
  mock_df[[metadata[[2]]$name]] <- c(TRUE)
  epidata_call$meta[[2]]$type <- "bool"
  expect_no_error(res <- parse_data_frame(epidata_call, mock_df) %>% as_tibble())
  expect_true(res$location)
  expect_identical(res$release_date, as.Date(NA))

  # if the call has no metadata, return the whole frame as is
  epidata_call$meta <- NULL
  expect_identical(parse_data_frame(epidata_call, mock_df), mock_df)
})

test_that("parse invalid time", {
  value <- list(3)
  value$class <- "my nonexistant class"
  expect_error(parse_timeset_input(value))
})

test_that("parse_data_frame warns when df contains fields not listed in meta", {
  epidata_call <- pub_flusurv(
    locations = "ca",
    epiweeks = 202001,
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
  # see generate_test_data.R
  mock_df <- as.data.frame(readr::read_rds(testthat::test_path("data/flusurv-epiweeks.rds")))

  # Success when meta and df fields match exactly
  expect_no_warning(parse_data_frame(epidata_call, mock_df))

  # Warning when df contains extra fields
  mock_df$extra <- 5
  expect_warning(
    parse_data_frame(epidata_call, mock_df),
    class = "epidatr__missing_meta_fields"
  )
  mock_df$extra <- NULL

  # Success when meta contains extra fields
  mock_df$rate_age_0 <- NULL
  expect_no_warning(parse_data_frame(epidata_call, mock_df))
})

test_that("parse_data_frame warns when df contains int values with decimal component", {
  epidata_call <- pub_flusurv(
    locations = "ca",
    epiweeks = 202001,
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
  # see generate_test_data.R
  mock_df <- as.data.frame(readr::read_rds(testthat::test_path("data/flusurv-epiweeks.rds")))

  # Int fields are returned as double
  result <- parse_data_frame(epidata_call, mock_df)
  expect_type(result$lag, "double")

  # Replace int fields with decimal
  mock_df$lag <- 4.3

  # Warning when int values have a decimal component
  expect_warning(
    parse_data_frame(epidata_call, mock_df),
    class = "epidatr__int_nonzero_decimal_digits"
  )
})

test_that("parse_value can handle NA/NULL values in an int field", {
  info <- create_epidata_field_info(
    name = "test",
    type = "int"
  )
  expect_warning(
    parse_value(info, c(1, 1.5, NA, NULL)),
    class = "epidatr__int_nonzero_decimal_digits"
  )
})

test_that("parse_api_date accepts str and int input", {
  expect_identical(parse_api_date("20200101"), as.Date("2020-01-01"))
  expect_identical(parse_api_date(20200101), as.Date("2020-01-01"))
})

test_that("parse_api_date accepts YYYYMMDD and YYYY-MM-DD", {
  expect_identical(parse_api_date(20200101), as.Date("2020-01-01"))
  expect_identical(parse_api_date("2020-01-01"), as.Date("2020-01-01"))
})

test_that("parse_api_date handles missing values appropriately", {
  expect_identical(parse_api_date(NA), as.Date(NA))
})

test_that("date_to_epiweek accepts str and int input", {
  expect_identical(date_to_epiweek("20200101"), 202001)
  expect_identical(date_to_epiweek(20200101), 202001)
})

test_that("date_to_epiweek accepts single and double-digit weeks", {
  expect_identical(date_to_epiweek(20201101), 202045)
  expect_identical(date_to_epiweek(20200109), 202002)
})

test_that("reformat_epirange works in basic cases", {
  # Week to week
  result <- reformat_epirange(epirange(202002, 202013), "week")
  expect_identical(result, epirange(202002, 202013))

  result <- reformat_epirange(epirange("202002", "202013"), "week")
  expect_identical(result, epirange("202002", "202013"))

  # Week to day
  # Across year boundary
  result <- reformat_epirange(epirange(202001, 202013), "day")
  expect_identical(result, epirange(20191229, 20200322))

  result <- reformat_epirange(epirange(202002, 202013), "day")
  expect_identical(result, epirange(20200105, 20200322))

  result <- reformat_epirange(epirange("202002", "202013"), "day")
  expect_identical(result, epirange(20200105, 20200322))

  # Day to week
  result <- reformat_epirange(epirange(20200201, 20201031), "week")
  expect_identical(result, epirange(202005, 202044))

  result <- reformat_epirange(epirange("20200201", "20201031"), "week")
  expect_identical(result, epirange(202005, 202044))

  # Day to day
  result <- reformat_epirange(epirange(20200201, 20201031), "day")
  expect_identical(result, epirange(20200201, 20201031))

  result <- reformat_epirange(epirange("20200201", "20201031"), "day")
  expect_identical(result, epirange("20200201", "20201031"))
})
