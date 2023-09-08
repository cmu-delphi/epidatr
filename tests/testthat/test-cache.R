test_that("basic cache setup", {
  expect_true(is.null(cache_environ$epidatr_cache))
})

new_temp_dir <- tempdir()
test_set_cache <- function(cache_dir = new_temp_dir,
                           days = 1,
                           max_size = 1,
                           logfile = "logfile.txt",
                           confirm = FALSE) {
  set_cache(
    cache_dir = cache_dir,
    days = days,
    max_size = max_size,
    logfile = logfile,
    confirm = confirm
  )
}

test_that("cache set as expected", {
  test_set_cache()
  if (grepl("/", as.character(new_temp_dir))) {
    # this is what check produces
    expect_equal(cache_info()$dir, normalizePath(new_temp_dir))
  } else {
    # this is what test produces directly
    tmp <- strsplit(cache_info()$dir, "/")[[1]]
    expect_equal(tmp[length(tmp)], normalizePath(new_temp_dir))
  }
  expect_equal(cache_info()$max_size, 1024^2)
  expect_equal(cache_info()$max_age, 24 * 60 * 60)
  expect_equal(cache_info()$prune_rate, 20)
  expect_equal(cache_info()$logfile, file.path(new_temp_dir, "logfile.txt"))
  expect_equal(cache_info()$evict, "lru")
  expect_equal(cache_info()$max_n, Inf)
  disable_cache()
})

# use an existing example to save, then load and compare the values
test_that("cache saves & loads", {
  test_set_cache()
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
  local_mocked_bindings(
    request_impl = function(...) NULL,
    .package = "epidatr"
  )
  local_mocked_bindings(
    # see generate_test_data.R
    content = function(...) readRDS(testthat::test_path("data/test-classic.rds")),
    .package = "httr"
  )
  # testing the cache_info

  first_call <- epidata_call %>% fetch()
  # compare cached call w/non cached (and make sure it's fetching from the cache)
  rlang::reset_warning_verbosity("using the cache")
  expect_warning(cache_call <- epidata_call %>% fetch(), class = "cache_access")
  rlang::reset_warning_verbosity("using the cache")
  expect_equal(first_call, cache_call)
  # and compare directly with the file saved
  # the request url hashed here is "https://api.delphi.cmu.edu/epidata/covidcast/?data_source=jhu-csse&signals=
  # confirmed_7dav_incidence_prop&geo_type=state&time_type=day&geo_values=ca%2Cfl&time_values=20200601-20200801
  # &as_of=20220101"
  request_hash <- "01479468989102176d7cb70374f18f1f.rds"
  direct_from_cache <- readRDS(file.path(new_temp_dir, request_hash))
  expect_equal(first_call, direct_from_cache[[1]])
})

test_that("check_is_recent", {
  expect_true(check_is_recent(format(Sys.Date() - 7, format = "%Y%m%d"), 7))
  expect_true(check_is_recent(format(Sys.Date(), format = "%Y%m%d"), 7))
  expect_true(check_is_recent(format(Sys.Date() + 12, format = "%Y%m%d"), 7))
  expect_false(check_is_recent(format(Sys.Date() - 12, format = "%Y%m%d"), 7))
  expect_false(check_is_recent(
    epirange(
      format(Sys.Date() - 12, format = "%Y%m%d"),
      format(Sys.Date() - 9, format = "%Y%m%d")
    ),
    7
  ))
  expect_true(check_is_recent(
    epirange(
      format(Sys.Date() - 12, format = "%Y%m%d"),
      format(Sys.Date() - 2, format = "%Y%m%d")
    ),
    7
  ))
  expect_true(check_is_recent(
    epirange(
      format(Sys.Date() - 2, format = "%Y%m%d"),
      format(Sys.Date(), format = "%Y%m%d")
    ),
    7
  ))
  expect_true(check_is_recent(
    epirange(
      format(Sys.Date(), format = "%Y%m%d"),
      format(Sys.Date() + 5, format = "%Y%m%d")
    ),
    7
  ))
})

test_that("check_is_cachable", {
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
  test_set_cache()
  check_fun(expected_result = FALSE) # doesn't specify issues or as_of
  check_fun(as_of = "2020-01-01", expected_result = TRUE) # valid as_of
  check_fun(issues = "2020-01-01", expected_result = TRUE) # valid issues
  check_fun(issues = epirange("2020-01-01", "2020-03-01"), expected_result = TRUE) # valid issues
  check_fun(as_of = "*", expected_result = FALSE) # invalid as_of
  check_fun(issues = "*", expected_result = FALSE) # invalid issues

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
  check_fun(
    as_of = "2020-01-01",
    fetch_args = fetch_args_list(debug = TRUE),
    expected_result = FALSE
  )
  check_fun(
    as_of = "2020-01-01",
    fetch_args = fetch_args_list(format_type = "csv"),
    expected_result = FALSE
  )

  # cases where the cache isn't active
  disable_cache()
  check_fun(as_of = "2020-01-01", expected_result = FALSE)
  test_set_cache()
  cache_environ$epidatr_cache <- NULL
  check_fun(as_of = "2020-01-01", expected_result = FALSE)
  test_set_cache()
  check_fun(as_of = "2020-01-01", expected_result = TRUE)
})

test_set_cache()
cache_environ$epidatr_cache$prune()
clear_cache(disable = TRUE)
rm(new_temp_dir)

test_that("cache teardown", {
  expect_true(is.null(cache_environ$epidatr_cache))
})
