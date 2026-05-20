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

test_that("parse_data_frame handles NA values and missing meta", {
  meta <- list(
    create_epidata_field_info("val", "int"),
    create_epidata_field_info("flag", "bool")
  )
  epidata_call <- structure(list(meta = meta), class = "epidata_call")
  mock_df <- data.frame(val = NA_integer_, flag = TRUE)

  expect_no_error(res <- parse_data_frame(epidata_call, mock_df))
  expect_true(is.na(res$val))
  expect_true(is.numeric(res$val))
  expect_true(res$flag)

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
  meta <- list(create_epidata_field_info("val", "int"))
  epidata_call <- structure(list(meta = meta), class = "epidata_call")

  mock_df <- data.frame(val = 1L)
  expect_no_warning(parse_data_frame(epidata_call, mock_df))

  mock_df_extra <- data.frame(val = 1L, extra = 5)
  expect_snapshot(parse_data_frame(epidata_call, mock_df_extra), cran = TRUE)

  # Success when meta contains extra fields that aren't in the data
  meta_extra <- list(
    create_epidata_field_info("val", "int"),
    create_epidata_field_info("other", "int")
  )
  epidata_call$meta <- meta_extra
  expect_no_warning(parse_data_frame(epidata_call, mock_df))
})

test_that("parse_data_frame warns when df contains int values with decimal component", {
  meta <- list(create_epidata_field_info("val", "int"))
  epidata_call <- structure(list(meta = meta), class = "epidata_call")
  mock_df <- data.frame(val = 4.3)

  expect_snapshot(parse_data_frame(epidata_call, mock_df), cran = TRUE)
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

test_that("parse_api_date", {
  # accepts str
  expect_identical(parse_api_date("20200101"), as.Date("2020-01-01"))
  # accepts YYYYMMDD and YYYY-MM-DD (int and str)
  expect_identical(parse_api_date(20200101), as.Date("2020-01-01"))
  expect_identical(parse_api_date("2020-01-01"), as.Date("2020-01-01"))
  # handles missing values appropriately
  expect_identical(parse_api_date(NA), as.Date(NA))
  expect_equal(parse_api_date(c(NA, NA)), as.Date(c(NA, NA)))
  # parse_api_date works on mixed formats
  expect_equal(
    parse_api_date(c("20210101", "2021-01-02", "01032021", NA)),
    as.Date(c("2021-01-01", "2021-01-02", "2021-01-03", NA))
  )
  # handles invalid dates gracefully
  expect_equal(
    parse_api_date(c("invalid", "20210101")),
    as.Date(c(NA, "2021-01-01"))
  )
})

test_that("parse_api_timestamp_to_datetime works on timestamps", {
  val <- 1592707979
  # 1592707979 is 2020-06-20 19:52:59 PDT
  expected <- as.POSIXct(1592707979, origin = "1970-01-01")
  expect_equal(parse_api_timestamp_to_datetime(val), expected)
  expect_identical(parse_api_timestamp_to_datetime(as.character(val)), expected)

  # Check vectorization
  expect_equal(
    parse_api_timestamp_to_datetime(c(1000000000, 1592707979)),
    as.POSIXct(c(1000000000, 1592707979), origin = "1970-01-01")
  )
  # Missing values
  expect_identical(parse_api_timestamp_to_datetime(NA), as.POSIXct(NA))
})

test_that("parse_api_week returns the expected day of the week", {
  expect_identical(parse_api_week(202005) %>% weekdays(), "Sunday")
  expect_identical(parse_api_week(202005, 4) %>% weekdays(), "Wednesday")
  expect_identical(parse_api_week(202005, 7) %>% weekdays(), "Saturday")
})

test_that("date_to_epiweek accepts str and int input", {
  expect_identical(date_to_epiweek("20200101"), 202001)
  expect_identical(date_to_epiweek(20200101), 202001)
  # Single and double-digit weeks
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
