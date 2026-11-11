# Live check that V5 queries for a fixed past version are cached and that a
# cached read matches a fresh fetch. Runs under `make test-live-cast`.
# NHSN at state level is small enough to fetch without a row limit.

local_live_cache <- function(env = parent.frame()) {
  previous <- cache_environ$epidatr_cache
  previous_args <- cache_environ$cache_args
  dir <- withr::local_tempdir(.local_envir = env)
  suppressMessages(set_cache(cache_dir = dir, confirm = FALSE))
  withr::defer(
    {
      cache_environ$epidatr_cache <- previous
      cache_environ$cache_args <- previous_args
    },
    envir = env
  )
  invisible(dir)
}

# Counts real requests while still sending them to the server.
count_requests <- function(env = parent.frame()) {
  counter <- new.env()
  counter$n <- 0L
  real <- request_epidata
  testthat::local_mocked_bindings(
    request_epidata = function(...) {
      counter$n <- counter$n + 1L
      real(...)
    },
    .env = env
  )
  counter
}

skip_if_custom_cast_url <- function() {
  # A custom base_url is never cached, so there is nothing to test.
  if (nzchar(Sys.getenv("EPIDATR_CAST_BASE_URL"))) {
    testthat::skip("EPIDATR_CAST_BASE_URL set; custom base URLs bypass the cache")
  }
}

fetch_snapshot <- function(...) {
  epidata_snapshot(
    source = "nhsn",
    signals = "confirmed_admissions_flu_ew",
    geo_type = "state",
    snapshot_date = "2025-06-01",
    fetch_args = fetch_args_list(limit = -1L, ...)
  )
}

fetch_archive <- function(...) {
  epidata_archive(
    source = "nhsn",
    signals = "confirmed_admissions_flu_ew",
    geo_type = "state",
    report_time = "<2025-06-01",
    fetch_args = fetch_args_list(limit = -1L, ...)
  )
}

test_that("a past snapshot_date query is cached and read back unchanged", {
  skip_unless_live()
  skip_if_custom_cast_url()
  local_live_cache()
  requests <- count_requests()

  first <- fetch_snapshot()
  expect_gt(nrow(first), 0)
  expect_length(cache_environ$epidatr_cache$keys(), 1)
  expect_equal(requests$n, 1L)

  second <- fetch_snapshot()
  expect_equal(requests$n, 1L)
  expect_identical(second, first)

  # refresh_cache goes back to the server and rewrites the same entry.
  refreshed <- fetch_snapshot(refresh_cache = TRUE)
  expect_equal(requests$n, 2L)
  expect_length(cache_environ$epidatr_cache$keys(), 1)
  expect_equal(refreshed, first)
})

test_that("a past report_time archive query is cached and read back unchanged", {
  skip_unless_live()
  skip_if_custom_cast_url()
  local_live_cache()
  requests <- count_requests()

  first <- fetch_archive()
  expect_gt(nrow(first), 0)
  expect_length(cache_environ$epidatr_cache$keys(), 1)

  second <- fetch_archive()
  expect_equal(requests$n, 1L)
  expect_identical(second, first)
})

test_that("different fetch settings and query dates get separate cache entries", {
  skip_unless_live()
  skip_if_custom_cast_url()
  local_live_cache()

  snap <- fetch_snapshot()
  fetch_archive()
  expect_length(cache_environ$epidatr_cache$keys(), 2)

  # Unparsed output is not cachable and must not poison the cached tibble.
  fetch_snapshot(disable_data_frame_parsing = TRUE)
  expect_length(cache_environ$epidatr_cache$keys(), 2)
  expect_identical(fetch_snapshot(), snap)
})

test_that("queries without a fixed past version or with a row limit are not cached", {
  skip_unless_live()
  skip_if_custom_cast_url()
  local_live_cache()

  epidata_snapshot(
    source = "nhsn",
    signals = "confirmed_admissions_flu_ew",
    geo_type = "state",
    geo_values = "ca",
    fetch_args = fetch_args_list(limit = -1L)
  )
  fetch_snapshot_limited <- epidata_snapshot(
    source = "nhsn",
    signals = "confirmed_admissions_flu_ew",
    geo_type = "state",
    snapshot_date = "2025-06-01",
    fetch_args = fetch_args_list(limit = 10L)
  )
  expect_lte(nrow(fetch_snapshot_limited), 10)
  expect_length(cache_environ$epidatr_cache$keys(), 0)
})
