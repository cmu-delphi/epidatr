current_cache <- cache_environ$epidatr_cache
disable_cache()
withr::defer(cache_environ$epidatr_cache <- current_cache, teardown_env())

# The package's own V4-sunset nudge and frozen-endpoint note (warn_v4_sunset()
# and note_frozen_endpoint() in R/utils.R) fire whenever a test calls a
# deprecated/frozen V4 endpoint (e.g. the endpoint-urls snapshot test, which
# calls all of them). That's expected here, not a signal something's broken,
# so mute both for the whole test run.
testthat::local_mocked_bindings(
  warn_v4_sunset = function(fn_name) invisible(NULL),
  note_frozen_endpoint = function(fn_name) invisible(NULL),
  .env = teardown_env()
)
