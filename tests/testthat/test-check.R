test_that("assert_character_param", {
  expect_no_error(assert_character_param("name", "value", len = 1))
  expect_error(assert_character_param("name", c("value", "value"), len = 1))
  expect_no_error(assert_character_param("name", c("value1", "value2")))
  expect_error(assert_character_param("name", 5))
  expect_error(assert_character_param("name", NULL))
  expect_no_error(assert_character_param("name", NULL, required = FALSE))
  expect_error(assert_character_param("name", list("value1", 5)))
})

test_that("assert_integerish_param", {
  expect_no_error(assert_integerish_param("name", 5, len = 1))
  expect_error(assert_integerish_param("name", c(5, 6), len = 1))
  expect_no_error(assert_integerish_param("name", c(5, 6)))
  expect_error(assert_integerish_param("name", "value", len = 1))
  expect_error(assert_integerish_param("name", NULL, len = 1))
  expect_no_error(assert_integerish_param("name", NULL, len = 1, required = FALSE))
  expect_error(assert_integerish_param("name", list(5, 6)))
})

test_that("assert_date_param", {
  expect_no_error(assert_date_param("name", "2020-01-01", len = 1))
  expect_error(assert_date_param("name", c("2020-01-01", "2021-01-02"), len = 1))
  expect_no_error(assert_date_param("name", c("2020-01-01", "2021-01-02")))
  expect_no_error(assert_date_param("name", c(20200101, 20200101)))
  expect_no_error(assert_date_param("name", c(as.Date("2020-01-01"), as.Date("2020-01-02"))))
  expect_error(assert_date_param("name", NULL))
  expect_no_error(assert_date_param("name", NULL, required = FALSE))
  expect_error(assert_date_param("name", list(20200101, 20200101)))
  # We'd like an error when using `c` instead of `list` for heterogeneous
  # things. Without `lubridate` loaded, we already get (a confusing) one, at
  # least when things are ordered in this particular way. With `lubridate`
  # loaded, we wouldn't get an error but would get the wrong input.
  # expect_error(assert_date_param("name", c(as.Date("2020-01-01"), 20200102, "20200103")))
  expect_no_error(assert_date_param("name", "*"))
})

test_that("assert_timeset_param", {
  expect_no_error(assert_timeset_param("name", "2020-01-01", len = 1))
  expect_error(assert_timeset_param("name", c("2020-01-01", "2021-01-02"), len = 1))
  expect_no_error(assert_timeset_param("name", c("2020-01-01", "2021-01-02")))
  expect_no_error(assert_timeset_param("name", c(20200101, 20200101)))
  expect_no_error(assert_timeset_param("name", c(as.Date("2020-01-01"), as.Date("2020-01-02"))))
  expect_no_error(assert_timeset_param("name", epirange(20200101, 20200102)))
  # The next error message is suboptimal, complaining about names rather than
  # the underlying problem of using `c` on `EpiRange`s. We might try to fix
  # (e.g., impl with `vctrs`) / forbid `c` on `EpiRange`s to improve it:
  expect_error(assert_timeset_param("name", c(epirange(20200101, 20200102), epirange(20200103, 20200104))))
  expect_error(assert_timeset_param("name", NULL))
  expect_no_error(assert_timeset_param("name", NULL, required = FALSE))
  expect_no_error(assert_timeset_param("name", list(epirange(20200101, 20200102), epirange(20200101, 20200102))))
  expect_no_error(assert_date_param("name", "*"))
})
