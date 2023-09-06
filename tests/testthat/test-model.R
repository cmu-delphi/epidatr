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
  mock_df[[metadata[[1]]$name]][1] <- list(NULL)
  mock_df[[metadata[[2]]$name]] <- c(TRUE)
  epidata_call$meta[[2]]$type <- "bool"
  res <- parse_data_frame(epidata_call, mock_df) %>% as_tibble()
  expect_true(res$location)

  # if the call has no metadata, return the whole frame as is
  epidata_call$meta <- NULL
  expect_identical(parse_data_frame(epidata_call, mock_df), mock_df)
})

test_that("parse invalid time", {
  vale <- list(3)
  vale$class <- "my nonexistant class"
  expect_error(parse_timeset_input(vale))
})
