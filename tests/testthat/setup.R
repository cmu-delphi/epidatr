current_cache <- cache_environ$epidatr_cache
disable_cache()
withr::defer(cache_environ$epidatr_cache <- current_cache, teardown_env())
