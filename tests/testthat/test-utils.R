test_that("format_item", {
  expect_identical(format_item(5), "5")
  expect_identical(format_item("5"), "5")
  expect_identical(format_item("*"), "*")
  expect_identical(format_item(as.Date("2020-01-01")), "20200101")
  expect_identical(format_item(as.Date("2020-01-01") + 0:1), "20200101,20200102")
  expect_identical(format_item(epirange(201501, 201601)), "201501-201601")
})

test_that("format_list", {
  expect_identical(format_list(5), "5")
  expect_identical(format_list("5"), "5")
  expect_identical(format_list("*"), "*")
  expect_identical(format_list(as.Date("2020-01-01")), "20200101")
  expect_identical(format_list(as.Date("2020-01-01") + 0:1), "20200101,20200102")
  expect_identical(format_list(c(5, 6)), "5,6")
  expect_identical(format_list(c("5", "6")), "5,6")
  expect_identical(format_list(c("*", "*")), "*,*")
  expect_identical(format_list(list(5, 6)), "5,6")
  expect_identical(format_list(list("5", "6")), "5,6")
  expect_identical(format_list(list("*", "*")), "*,*")
})

test_that("check_is_cachable can handle both str and date inputs of various lengths", {
  epidata_call <- list(
    params = list(as_of = NULL, issues = NULL)
  )
  fetch_args <- fetch_args_list()

  expect_no_error(check_is_cachable(epidata_call, fetch_args))

  # as_of string
  epidata_call$params$as_of <- "2022-01-01"
  epidata_call$params$issues <- NULL
  expect_no_error(check_is_cachable(epidata_call, fetch_args))

  # as_of date
  epidata_call$params$as_of <- as.Date("2022-01-01")
  epidata_call$params$issues <- NULL
  expect_no_error(check_is_cachable(epidata_call, fetch_args))

  # issues single string
  epidata_call$params$as_of <- NULL
  epidata_call$params$issues <- "2022-01-01"
  expect_no_error(check_is_cachable(epidata_call, fetch_args))

  # issues string vector
  epidata_call$params$as_of <- NULL
  epidata_call$params$issues <- c("2022-01-01", "2022-02-01")
  expect_no_error(check_is_cachable(epidata_call, fetch_args))

  # issues single date
  epidata_call$params$as_of <- NULL
  epidata_call$params$issues <- as.Date("2022-01-01")
  expect_no_error(check_is_cachable(epidata_call, fetch_args))

  # issues date vector
  epidata_call$params$as_of <- NULL
  epidata_call$params$issues <- c(as.Date("2022-01-01"), as.Date("2022-02-01"))
  expect_no_error(check_is_cachable(epidata_call, fetch_args))

  # issues epirange
  epidata_call$params$as_of <- NULL
  epidata_call$params$issues <- epirange(as.Date("2022-01-01"), as.Date("2022-02-01"))
  expect_no_error(check_is_cachable(epidata_call, fetch_args))
})

test_that("check_is_recent can handle both str and date inputs of various lengths", {
  # NULL
  as_of <- NULL
  expect_no_error(result <- check_is_recent(as_of, 10))
  expect_identical(result, FALSE)

  # as_of single string
  as_of <- "2022-01-01"
  expect_no_error(result <- check_is_recent(as_of, 10))
  expect_identical(result, FALSE)

  # as_of string vector
  as_of <- c("2022-01-01", "3000-01-02", "3000-01-03")
  expect_no_error(result <- check_is_recent(as_of, 10))
  expect_identical(result, TRUE)

  # as_of single date
  as_of <- as.Date("2022-01-01")
  expect_no_error(result <- check_is_recent(as_of, 10))
  expect_identical(result, FALSE)

  # as_of date vector
  as_of <- as.Date(c("2022-01-01", "3000-01-02", "3000-01-03"))
  expect_no_error(result <- check_is_recent(as_of, 10))
  expect_identical(result, TRUE)
})

test_that("get_wildcard_equivalent_dates works in basic cases", {
  # Week date
  result <- get_wildcard_equivalent_dates(epirange(202002, 202013), "week")
  expect_identical(result, epirange(202002, 202013))

  result <- get_wildcard_equivalent_dates(epirange("202002", "202013"), "week")
  expect_identical(result, epirange("202002", "202013"))

  # Week wildcard
  result <- get_wildcard_equivalent_dates("*", "week")
  expect_identical(result, epirange(100001, 300001))

  # Day date
  result <- get_wildcard_equivalent_dates(epirange(20200201, 20201031), "day")
  expect_identical(result, epirange(20200201, 20201031))

  result <- get_wildcard_equivalent_dates(epirange("20200201", "20201031"), "day")
  expect_identical(result, epirange("20200201", "20201031"))

  # Day wildcard
  result <- get_wildcard_equivalent_dates("*", "day")
  expect_identical(result, epirange(10000101, 30000101))
})
