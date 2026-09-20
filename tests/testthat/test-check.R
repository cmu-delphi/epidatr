test_that("assert_character_param", {
  expect_no_error(assert_character_param("name", "*"))
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
  expect_no_error(assert_date_param("name", "*"))
  expect_no_error(assert_date_param("name", "2020-01-01", len = 1))
  expect_error(assert_date_param("name", c("2020-01-01", "2021-01-02"), len = 1))
  expect_no_error(assert_date_param("name", c("2020-01-01", "2021-01-02")))
  expect_no_error(assert_date_param("name", c(20200101, 20200101)))
  expect_no_error(assert_date_param("name", c(as.Date("2020-01-01"), as.Date("2020-01-02"))))
  expect_error(assert_date_param("name", NULL))
  expect_no_error(assert_date_param("name", NULL, required = FALSE))
  expect_error(assert_date_param("name", list(20200101, 20200101)))
})

test_that("assert_report_time_param", {
  ok <- list(
    "*",
    "2020-01-01",
    "2020-01-01T13:45:00Z",
    as.POSIXct("2020-01-01 13:45:00", tz = "UTC")
  )
  for (value in ok) expect_no_error(assert_report_time_param("name", value, len = 1))

  expect_error(assert_report_time_param("name", NULL))
  expect_no_error(assert_report_time_param("name", NULL, required = FALSE))
  # POSIXt length is enforced even though check_class lacks a len param
  expect_error(
    assert_report_time_param(
      "name",
      as.POSIXct(c("2020-01-01", "2020-01-02"), tz = "UTC"),
      len = 1
    )
  )
})

test_that("format_report_time_bound", {
  cases <- list(
    # Already-formatted UTC timestamps pass through unchanged, including the
    # cast-API's optional seconds and optional fractional seconds
    list("2025-10-16T13:45:00Z", "2025-10-16T13:45:00Z"),
    list("2025-10-16T13:45Z", "2025-10-16T13:45Z"),
    list("2025-10-16T13:45:00.123456Z", "2025-10-16T13:45:00.123456Z"),
    # POSIXt is rendered as a UTC timestamp
    list(as.POSIXct("2025-10-16 13:45:00", tz = "UTC"), "2025-10-16T13:45:00Z"),
    # Everything else falls back to a bare date
    list("2025-10-16", "2025-10-16"),
    list(as.Date("2025-10-16"), "2025-10-16"),
    list(20251016, "2025-10-16"),
    # A malformed timestamp attempt is NA, not silently truncated to its date
    # prefix (as.Date() would otherwise ignore everything after "%Y-%m-%d")
    list("2025-01-01T00:00:00", NA_character_), # naive, no Z
    list("2025-01-01T00:00:00+00:00", NA_character_), # offset
    list("2025-01-01T00:00:00-05:00", NA_character_) # offset
  )
  for (case in cases) expect_equal(format_report_time_bound(case[[1]]), case[[2]])
})

test_that("assert_timeset_param", {
  # Make sure to keep in sync with test-model.R parse_timeset_input checks
  expect_no_error(assert_timeset_param("name", "*"))
  expect_no_error(assert_timeset_param("name", "2020-01-01", len = 1))
  expect_error(assert_timeset_param("name", c("2020-01-01", "2021-01-02"), len = 1))
  expect_no_error(assert_timeset_param("name", c("2020-01-01", "2021-01-02")))
  expect_no_error(assert_timeset_param("name", c(20200101, 20200101)))
  expect_no_error(assert_timeset_param("name", c(as.Date("2020-01-01"), as.Date("2020-01-02"))))
  expect_no_error(assert_timeset_param("name", epirange(20200101, 20200102)))
  expect_error(assert_timeset_param("name", c(epirange(20200101, 20200102), epirange(20200103, 20200104))))
  expect_error(assert_timeset_param("name", NULL))
  expect_no_error(assert_timeset_param("name", NULL, required = FALSE))
  expect_error(assert_timeset_param("name", list(epirange(20200101, 20200102), epirange(20200101, 20200102))))
  # Non-EpiRange-class epiranges are no longer allowed:
  expect_error(assert_timeset_param("name", list(from = "2020-01-01", to = "2021-01-02")))
  expect_error(assert_timeset_param("name", c(from = "2020-01-01", to = "2021-01-02")))
})

test_that("validate_version_query", {
  # Comparison operators are preserved
  expect_equal(validate_version_query("<2024-01-01"), "<2024-01-01")
  expect_equal(validate_version_query(">2024-01-01"), ">2024-01-01")
  expect_equal(validate_version_query("<=2024-01-01"), "<=2024-01-01")
  expect_equal(validate_version_query(">=2024-01-01"), ">=2024-01-01")

  # EpiRange maps to inclusive server-side range
  expect_equal(validate_version_query(epirange("2024-01-01", "2024-01-05")), "2024-01-01:2024-01-05")

  # UTC timestamp bounds are preserved as-is
  expect_equal(validate_version_query("<=2024-01-01T13:45:00Z"), "<=2024-01-01T13:45:00Z")
  expect_equal(validate_version_query(">2024-01-01T13:45:00Z"), ">2024-01-01T13:45:00Z")

  # Naive timestamps and timezone offsets are rejected, not silently
  # truncated to a bare date (matches the cast-API's own rejection of these)
  expect_error(validate_version_query("<2024-01-01T13:45:00"), class = "epidatr__invalid_version_query")
  expect_error(validate_version_query("<2024-01-01T13:45:00+00:00"), class = "epidatr__invalid_version_query")
  expect_error(validate_version_query("<2024-01-01T13:45:00-05:00"), class = "epidatr__invalid_version_query")

  # Bare dates are rejected — use snapshot_date for point-in-time
  expect_error(validate_version_query("2024-01-01"), class = "epidatr__invalid_version_query")
  expect_error(validate_version_query("20240101"), class = "epidatr__invalid_version_query")
  expect_error(validate_version_query(20240101), class = "epidatr__invalid_version_query")
  expect_error(validate_version_query(as.Date("2024-01-01")), class = "epidatr__invalid_version_query")

  # Explicit '=' is also rejected
  expect_error(validate_version_query("=2024-01-01"), class = "epidatr__invalid_version_query")

  # Other invalid inputs
  expect_error(validate_version_query("not-a-date"), class = "epidatr__invalid_version_query")
  expect_error(validate_version_query("<not-a-date"), class = "epidatr__invalid_version_query")
})
