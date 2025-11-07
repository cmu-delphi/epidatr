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
  expect_equal(cache_info()$max_size, 1024^2)
  expect_equal(cache_info()$max_age, 24 * 60 * 60)
  expect_equal(cache_info()$prune_rate, 20)
  expect_equal(cache_info()$logfile, file.path(new_temp_dir, "logfile.txt"))
  expect_equal(cache_info()$evict, "lru")
  expect_equal(cache_info()$max_n, Inf)
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
  httr_content_called_count <- 0
  local_mocked_bindings(
    request_impl = function(...) NULL,
    .package = "epidatr"
  )
  local_mocked_bindings(
    # see generate_test_data.R
    content = function(...) {
      httr_content_called_count <<- httr_content_called_count + 1
      readRDS(testthat::test_path("data/test-classic.rds"))
    },
    .package = "httr"
  )

  first_call <- epidata_call %>% fetch()
  cache_call <- epidata_call %>% fetch()
  expect_equal(httr_content_called_count, 1)
  expect_equal(first_call, cache_call)

  # and compare directly with the file saved
  # the request url hashed here is "https://api.delphi.cmu.edu/epidata/covidcast/?data_source=jhu-csse&signals=
  # confirmed_7dav_incidence_prop&geo_type=state&time_type=day&geo_values=ca%2Cfl&time_values=20200601-20200801
  # &as_of=20220101"
  request_hash <- "01479468989102176d7cb70374f18f1f.rds"
  direct_from_cache <- readRDS(file.path(new_temp_dir, request_hash))
  expect_equal(first_call, direct_from_cache[[1]])

  # Test the empty return branch
  expect_message(clear_cache())
  local_mocked_bindings(
    # see generate_test_data.R
    content = function(...) {
      httr_content_called_count <<- httr_content_called_count + 1
      '{"epidata":[],"result":-2,"message":"no results"}'
    },
    .package = "httr"
  )
  expect_warning(empty_call <- epidata_call %>% fetch())
  expect_equal(empty_call, tibble())
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
  check_fun(issues = epirange("2020-01-01", "2020-03-01"), expected_result = TRUE) # valid issues
  check_fun(as_of = "*", expected_result = FALSE) # invalid as_of
  check_fun(issues = "*", expected_result = FALSE) # invalid issues
  # refresh_cache works
  check_fun(as_of = "2020-01-01", fetch_args = fetch_args_list(refresh_cache = TRUE), expected_result = FALSE)

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
  expect_message(test_set_cache())
  cache_environ$epidatr_cache <- NULL
  check_fun(as_of = "2020-01-01", expected_result = FALSE)
  expect_message(test_set_cache())
  check_fun(as_of = "2020-01-01", expected_result = TRUE)
})
