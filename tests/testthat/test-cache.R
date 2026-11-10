new_temp_dir <- tempdir()
test_set_cache <- function(
  cache_dir = new_temp_dir,
  days = 1,
  max_size = 1,
  logfile = "logfile.txt",
  confirm = FALSE
) {
  set_cache(
    cache_dir = cache_dir,
    days = days,
    max_size = max_size,
    logfile = logfile,
    confirm = confirm
  )
}

test_that("basic cache setup", {
  # Should be off at the start
  expect_true(!is_cache_enabled())
})

test_that("cache set as expected", {
  # Setup a new cache
  expect_message(test_set_cache())
  # Delete cache files after the test
  withr::defer(clear_cache(disable = TRUE))

  if (grepl("/", as.character(new_temp_dir))) {
    # this is what check produces
    expect_equal(cache_info()$dir, normalizePath(new_temp_dir))
  } else {
    # this is what test produces directly
    tmp <- strsplit(cache_info()$dir, "/")[[1]]
    expect_equal(tmp[length(tmp)], normalizePath(new_temp_dir))
  }
  # set_cache converts max_size (MB) and days to cachem's bytes and seconds
  expect_equal(cache_info()$max_size, 1024^2)
  expect_equal(cache_info()$max_age, 24 * 60 * 60)
  expect_equal(cache_info()$logfile, file.path(new_temp_dir, "logfile.txt"))
})

# use an existing example to save, then load and compare the values
test_that("cache saves & loads", {
  # Setup a new cache
  expect_message(test_set_cache())
  # Delete cache files after the test
  withr::defer(clear_cache(disable = TRUE))

  epidata_call <- pub_covidcast(
    source = "jhu-csse",
    signals = "confirmed_7dav_incidence_prop",
    time_type = "day",
    geo_type = "state",
    time_values = epirange("2020-06-01", "2020-08-01"),
    geo_values = "ca,fl",
    as_of = "2022-01-01",
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
  mock_response <- list(
    epidata = list(list(
      source = "jhu-csse",
      signal = "confirmed_7dav_incidence_prop",
      geo_type = "state",
      time_type = "day",
      geo_value = "ca",
      time_value = 20200601L,
      issue = 20200602L,
      lag = 1L,
      value = 1.5,
      stderr = 0.1,
      sample_size = 100.0,
      direction = 1.0,
      missing_value = 0L,
      missing_stderr = 0L,
      missing_sample_size = 0L
    )),
    result = 1,
    message = "success"
  )
  perform_count <- 0
  fixture <- to_httr2_response(as.character(jsonlite::toJSON(
    mock_response,
    auto_unbox = TRUE
  )))
  local_mocked_bindings(
    req_perform = function(req, ...) {
      perform_count <<- perform_count + 1
      fixture
    },
    .package = "httr2"
  )

  first_call <- epidata_call %>% fetch()
  cache_call <- epidata_call %>% fetch()
  expect_equal(perform_count, 1)
  expect_equal(first_call, cache_call)

  # and compare directly with the file saved
  # the request url hashed here is "https://api.delphi.cmu.edu/epidata/covidcast/?data_source=jhu-csse&signals=
  # confirmed_7dav_incidence_prop&geo_type=state&time_type=day&geo_values=ca%2Cfl&time_values=20200601-20200801
  # &as_of=20220101"
  request_hash <- paste0(
    openssl::md5(cache_key(epidata_call, fetch_args_list())),
    ".rds"
  )
  direct_from_cache <- readRDS(file.path(new_temp_dir, request_hash))
  expect_equal(first_call, direct_from_cache[[1]])

  # Test the empty return branch
  expect_message(clear_cache())
  empty_fixture <- to_httr2_response(
    '{"epidata":[],"result":-2,"message":"no results"}'
  )
  local_mocked_bindings(
    req_perform = function(req, ...) empty_fixture,
    .package = "httr2"
  )
  expect_warning(empty_call <- epidata_call %>% fetch())
  expect_equal(empty_call, tibble())
})

test_that("check_is_cachable", {
  # Setup a new cache
  expect_message(test_set_cache())
  # Delete cache files after the test
  withr::defer(clear_cache(disable = TRUE))

  check_fun <- function(..., fetch_args = fetch_args_list(), expected_result) {
    epidata_call <- pub_covidcast(
      source = "jhu-csse",
      signals = "confirmed_7dav_incidence_prop",
      time_type = "day",
      geo_type = "state",
      time_values = epirange("2020-06-01", "2020-08-01"),
      geo_values = "ca,fl",
      ...,
      fetch_args = fetch_args_list(dry_run = TRUE)
    )
    if (expected_result) {
      expect_true(check_is_cachable(epidata_call, fetch_args))
    } else {
      expect_false(check_is_cachable(epidata_call, fetch_args))
    }
  }
  check_fun(expected_result = FALSE) # doesn't specify issues or as_of
  check_fun(as_of = "2020-01-01", expected_result = TRUE) # valid as_of
  check_fun(issues = "2020-01-01", expected_result = TRUE) # valid issues
  check_fun(
    issues = epirange("2020-01-01", "2020-03-01"),
    expected_result = TRUE
  ) # valid issues
  check_fun(as_of = "*", expected_result = FALSE) # invalid as_of
  check_fun(issues = "*", expected_result = FALSE) # invalid issues
  # refresh_cache works
  check_fun(
    as_of = "2020-01-01",
    fetch_args = fetch_args_list(refresh_cache = TRUE),
    expected_result = FALSE
  )

  # any odd fetch args mean don't use the cache
  check_fun(
    as_of = "2020-01-01",
    fetch_args = fetch_args_list(fields = c("time_value", "value")),
    expected_result = FALSE
  )
  check_fun(
    as_of = "2020-01-01",
    fetch_args = fetch_args_list(disable_date_parsing = TRUE),
    expected_result = FALSE
  )
  check_fun(
    as_of = "2020-01-01",
    fetch_args = fetch_args_list(disable_data_frame_parsing = TRUE),
    expected_result = FALSE
  )
  # return_empty is fine
  # timeout_seconds is fine
  check_fun(
    as_of = "2020-01-01",
    fetch_args = fetch_args_list(base_url = "foo.bar"),
    expected_result = FALSE
  )
  check_fun(
    as_of = "2020-01-01",
    fetch_args = fetch_args_list(dry_run = TRUE),
    expected_result = FALSE
  )

  # cases where the cache isn't active
  disable_cache()
  check_fun(as_of = "2020-01-01", expected_result = FALSE)
  expect_message(test_set_cache())
  cache_environ$epidatr_cache <- NULL
  check_fun(as_of = "2020-01-01", expected_result = FALSE)
  expect_message(test_set_cache())
  check_fun(as_of = "2020-01-01", expected_result = TRUE)
})

test_that("set_cache validates days and max_size (#396)", {
  withr::defer(disable_cache())
  dir <- withr::local_tempdir()
  # A fraction like "3/24" used to become NA, pass validation, and then make
  # every cache read error inside cachem.
  withr::local_envvar(EPIDATR_CACHE_MAX_AGE_DAYS = "3/24")
  expect_error(
    set_cache(cache_dir = dir, confirm = FALSE),
    class = "epidatr__invalid_cache_setting"
  )
  withr::local_envvar(EPIDATR_CACHE_MAX_AGE_DAYS = "0.125")
  expect_message(set_cache(cache_dir = dir, confirm = FALSE))
  expect_equal(cache_info()$max_age, 0.125 * 24 * 60 * 60)

  expect_error(
    set_cache(cache_dir = dir, days = -1, confirm = FALSE),
    class = "epidatr__invalid_cache_setting"
  )
  expect_error(
    set_cache(cache_dir = dir, max_size = NA, confirm = FALSE),
    class = "epidatr__invalid_cache_setting"
  )
  withr::local_envvar(EPIDATR_CACHE_MAX_SIZE_MB = "big")
  expect_error(
    set_cache(cache_dir = dir, confirm = FALSE),
    class = "epidatr__invalid_cache_setting"
  )
  # the hint about fractions of a day is only for days
  size_error <- tryCatch(set_cache(cache_dir = dir, confirm = FALSE), error = identity)
  expect_false(grepl("days is 3 hours", conditionMessage(size_error), fixed = TRUE))
  withr::local_envvar(EPIDATR_CACHE_MAX_SIZE_MB = "1024", EPIDATR_CACHE_MAX_AGE_DAYS = "3/24")
  days_error <- tryCatch(set_cache(cache_dir = dir, confirm = FALSE), error = identity)
  expect_true(grepl("days is 3 hours", conditionMessage(days_error), fixed = TRUE))
})

test_that("set_cache never prompts non-interactively and reports honestly (#401)", {
  withr::defer(disable_cache())
  missing_dir <- file.path(withr::local_tempdir(), "not-yet")
  # confirm = TRUE can't prompt here, so the directory is not created, the
  # cache stays off, and the message says so (it used to say "being used").
  rlang::local_interactive(FALSE)
  expect_message(
    set_cache(cache_dir = missing_dir, confirm = TRUE),
    "Could not set the epidatr cache"
  )
  expect_false(dir.exists(missing_dir))
  expect_false(is_cache_enabled())

  # confirm = FALSE (what .onLoad uses) creates the directory.
  expect_message(
    set_cache(cache_dir = missing_dir, confirm = FALSE),
    "cache is being used"
  )
  expect_true(dir.exists(missing_dir))
  expect_true(is_cache_enabled())
})

test_that("a bad cache setting doesn't stop the package loading (#396, #401)", {
  old_use_cache <- cache_environ$use_cache
  withr::defer(cache_environ$use_cache <- old_use_cache)
  withr::defer(disable_cache())
  withr::local_envvar(
    EPIDATR_USE_CACHE = "TRUE",
    EPIDATR_CACHE_DIR = withr::local_tempdir(),
    EPIDATR_CACHE_MAX_AGE_DAYS = "3/24"
  )
  expect_message(.onLoad(NULL, "epidatr"), "epidatr cache is off")
  expect_false(is_cache_enabled())
  # braces in the bad value must not break the message itself
  withr::local_envvar(EPIDATR_CACHE_MAX_AGE_DAYS = "{x}")
  expect_message(.onLoad(NULL, "epidatr"), "epidatr cache is off")
})

covidcast_call_as_of <- function(as_of) {
  pub_covidcast(
    source = "jhu-csse",
    signals = "confirmed_7dav_incidence_prop",
    time_type = "day",
    geo_type = "state",
    time_values = epirange("2020-06-01", "2020-08-01"),
    geo_values = "ca",
    as_of = as_of,
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
}

test_that("the stale-cache warning fires for recent versions (#385)", {
  # the warning is rate-limited per session; show it every time here
  withr::local_options(rlib_warning_verbosity = "verbose")
  # It used to read epidata_call$params, which doesn't exist, so it never fired.
  expect_warning(
    check_for_cache_warnings(covidcast_call_as_of(Sys.Date()), fetch_args_list()),
    class = "cache_recent_data"
  )
  expect_no_warning(
    check_for_cache_warnings(covidcast_call_as_of("2022-01-01"), fetch_args_list())
  )
})

test_that("latest_version_date reads dates, ranges, and epiweeks", {
  expect_equal(latest_version_date("20220101"), as.Date("2022-01-01"))
  expect_equal(latest_version_date("20220101-20220105"), as.Date("2022-01-05"))
  expect_equal(latest_version_date("2024-12-01:2025-01-01"), as.Date("2025-01-01"))
  expect_equal(latest_version_date("<2025-01-01"), as.Date("2025-01-01"))
  expect_equal(latest_version_date("202001-202004"), as.Date("2020-01-25"))
  expect_null(latest_version_date(NULL))
  expect_null(latest_version_date("*"))
  # Impossible dates are ignored instead of raising an error.
  expect_null(latest_version_date("2024-02-30"))
  expect_null(latest_version_date("202099"))
  expect_equal(latest_version_date("20241301,20230101"), as.Date("2023-01-01"))
  expect_equal(latest_version_date("2025-10-16T13:45:00Z"), as.Date("2025-10-16"))
  expect_equal(
    latest_version_date("2025-10-01T00:00:00Z:2025-10-16T13:45:00Z"),
    as.Date("2025-10-16")
  )
})

test_that("refresh_cache only writes entries a default fetch may read (#388)", {
  expect_message(test_set_cache(cache_dir = withr::local_tempdir()))
  withr::defer(disable_cache())
  call <- covidcast_call_as_of("2022-01-01")

  # refresh_cache with a parse-changing setting must not write an entry that a
  # later default fetch would read back.
  epidata <- data.frame(
    source = "jhu-csse", signal = "confirmed_7dav_incidence_prop",
    geo_type = "state", time_type = "day", geo_value = "ca",
    time_value = 20200601L, issue = 20200602L, lag = 1L, value = 1.5
  )
  requests <- 0
  local_mocked_bindings(request_epidata = function(...) {
    requests <<- requests + 1
    epidata
  })
  fetch(call, fetch_args_list(refresh_cache = TRUE, disable_date_parsing = TRUE))
  expect_length(cache_environ$epidatr_cache$keys(), 0)

  # A default fetch caches, and a refresh rewrites the same entry.
  first <- suppressWarnings(fetch(call))
  expect_length(cache_environ$epidatr_cache$keys(), 1)
  suppressWarnings(fetch(call))
  expect_equal(requests, 2)
  suppressWarnings(fetch(call, fetch_args_list(refresh_cache = TRUE)))
  expect_equal(requests, 3)
  expect_length(cache_environ$epidatr_cache$keys(), 1)
  expect_s3_class(first$time_value, "Date")
})

test_that("an empty result allowed by return_empty isn't cached (#388)", {
  expect_message(test_set_cache(cache_dir = withr::local_tempdir()))
  withr::defer(disable_cache())
  local_mocked_bindings(request_epidata = function(...) list())
  result <- suppressWarnings(
    fetch(covidcast_call_as_of("2022-01-01"), fetch_args_list(return_empty = TRUE))
  )
  expect_equal(nrow(result), 0)
  expect_length(cache_environ$epidatr_cache$keys(), 0)
})

test_that("the cache key includes a non-default reference_week_day (#388)", {
  call <- covidcast_call_as_of("2022-01-01")
  default_key <- cache_key(call, fetch_args_list())
  expect_equal(
    default_key,
    paste0("epidatr", utils::packageVersion("epidatr"), ":", request_url(call, "json"))
  )
  expect_false(default_key == cache_key(call, fetch_args_list(reference_week_day = 7)))

  # and fetch() uses it: a different reference_week_day doesn't hit the cache
  expect_message(test_set_cache(cache_dir = withr::local_tempdir()))
  withr::defer(disable_cache())
  requests <- 0
  local_mocked_bindings(request_epidata = function(...) {
    requests <<- requests + 1
    data.frame(
      source = "jhu-csse", signal = "confirmed_7dav_incidence_prop",
      geo_type = "state", time_type = "day", geo_value = "ca",
      time_value = 20200601L, issue = 20200602L, lag = 1L, value = 1.5
    )
  })
  suppressWarnings(fetch(call))
  suppressWarnings(fetch(call, fetch_args_list(reference_week_day = 7)))
  suppressWarnings(fetch(call, fetch_args_list(reference_week_day = 7)))
  expect_equal(requests, 2)
})

test_that("V5 calls for a fixed past version are cachable (A2)", {
  expect_message(test_set_cache(cache_dir = withr::local_tempdir()))
  withr::defer(disable_cache())
  dry <- fetch_args_list(dry_run = TRUE)
  snap <- function(...) epidata_snapshot("nssp", "pct_ed_visits_influenza", "state", ..., fetch_args = dry)
  arch <- function(...) epidata_archive("nssp", "pct_ed_visits_influenza", "state", ..., fetch_args = dry)
  cachable <- function(call, fetch_args = fetch_args_list()) check_is_cachable(call, fetch_args)
  two_days_ago <- format(utc_today() - 2)
  yesterday <- format(utc_today() - 1)
  today <- format(utc_today())

  # fixed past versions
  expect_true(cachable(snap(snapshot_date = "2025-01-01")))
  expect_true(cachable(snap(snapshot_date = two_days_ago)))
  expect_true(cachable(arch(report_time = epirange("2024-12-01", "2025-01-01"))))
  expect_true(cachable(arch(report_time = "<2025-01-01")))
  expect_true(cachable(epidata_aux("nwss", report_time = "<2026-08-01", fetch_args = dry)))

  # versions that can still change
  expect_false(cachable(snap())) # latest
  expect_false(cachable(snap(snapshot_date = today)))
  expect_false(cachable(snap(snapshot_date = yesterday))) # one day of margin
  expect_false(cachable(arch())) # every version, including future ones
  expect_false(cachable(arch(report_time = "*")))
  expect_false(cachable(arch(report_time = ">2025-01-01")))
  expect_false(cachable(arch(report_time = epirange("2024-12-01", today))))
  # comma-joined conditions are OR'd, so one open-ended part makes it open
  expect_false(cachable(arch(report_time = c("<2024-01-01", ">2025-01-01"))))
  expect_true(cachable(arch(report_time = c("<2024-01-01", "<=2024-06-01"))))

  # a row limit returns an unstable subset
  limited <- epidata_snapshot(
    "nssp", "pct_ed_visits_influenza", "state",
    snapshot_date = "2025-01-01", fetch_args = fetch_args_list(dry_run = TRUE, limit = 5)
  )
  expect_false(cachable(limited))
})

test_that("V5 cachability edge cases", {
  local_mocked_bindings(utc_today = function() as.Date("2026-11-08"))
  v5 <- function(...) check_is_cachable_v5(list(...))
  # the cutoff is the UTC date, with a day of margin for late loads
  expect_true(v5(snapshot_date = "2026-11-06"))
  expect_false(v5(snapshot_date = "2026-11-07"))
  expect_false(v5(snapshot_date = "2026-11-08"))
  expect_false(v5(snapshot_date = "2026-11-08T01:00:00Z"))
  # every OR'd condition needs an upper bound
  expect_false(v5(report_time_query = "<2024-01-01,>2025-01-01"))
  expect_false(v5(report_time_query = "2024-01-01:2024-02-01,>=2025-01-01"))
  expect_true(v5(report_time_query = "<2024-01-01,2024-03-01:2024-04-01"))
  # a range needs both ends, and dates must be real
  expect_false(v5(report_time_query = "2024-01-01:"))
  expect_false(v5(report_time_query = "<2024-02-30"))
  expect_false(v5(report_time_query = "<2024-02-30,<2023-01-01"))
  expect_true(v5(report_time_query = "2025-10-01T00:00:00Z:2025-10-16T13:45:00Z"))
  # fractional seconds aren't an epiweek
  expect_true(v5(snapshot_date = "2024-01-01T00:00:00.123456Z"))
  expect_false(v5(report_time_query = "*"))
  expect_false(v5())
})

test_that("a repeated V5 snapshot is served from the cache", {
  expect_message(test_set_cache(cache_dir = withr::local_tempdir()))
  withr::defer(disable_cache())
  csv <- data.frame(
    signal = "pct_ed_visits_influenza", report_time = "2024-12-27T12:00:00Z",
    geo_type = "state", geo_value = c("ca", "pa"), fill_method = "source",
    reference_time = "2024-12-21", value = c("1.5", "2.5")
  )
  requests <- 0
  local_mocked_bindings(request_epidata = function(...) {
    requests <<- requests + 1
    csv
  })
  get <- function() {
    epidata_snapshot("nssp", "pct_ed_visits_influenza", "state", snapshot_date = "2025-01-01")
  }
  first <- get()
  second <- get()
  expect_equal(requests, 1)
  expect_identical(first, second) # including the cast_source/cast_kind attributes
})

test_that("the stale-cache warning covers V5 versions", {
  # the warning is rate-limited per session; show it every time here
  withr::local_options(rlib_warning_verbosity = "verbose")
  recent <- format(Sys.Date() - 2)
  expect_warning(
    check_for_cache_warnings(
      epidata_snapshot("nssp", "pct_ed_visits_influenza", "state",
        snapshot_date = recent, fetch_args = fetch_args_list(dry_run = TRUE)
      ),
      fetch_args_list()
    ),
    class = "cache_recent_data"
  )
  expect_no_warning(check_for_cache_warnings(
    epidata_archive("nssp", "pct_ed_visits_influenza", "state",
      report_time = epirange("2024-12-01", "2025-01-01"), fetch_args = fetch_args_list(dry_run = TRUE)
    ),
    fetch_args_list()
  ))
})
