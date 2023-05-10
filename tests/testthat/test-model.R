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
