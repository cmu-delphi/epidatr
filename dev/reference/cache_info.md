# Describe current cache

Report where the cache lives and how big it is. The result includes
cachem's `info()` fields (such as `dir`, `max_size` and `max_age`) plus
`enabled`, `n_entries` and `size_mb`. Entries written by older epidatr
versions share the directory and are counted; use
[`clear_cache`](https://cmu-delphi.github.io/epidatr/dev/reference/clear_cache.md)
to remove them. When the cache is off, the directory it would use is
inspected instead.

## Usage

``` r
cache_info()
```

## Value

a list of class `epidatr_cache_info`; printing it also shows how to
clear the cache

## See also

[`set_cache`](https://cmu-delphi.github.io/epidatr/dev/reference/set_cache.md)
to start a new cache (and general caching info),
[`clear_cache`](https://cmu-delphi.github.io/epidatr/dev/reference/clear_cache.md)
to delete the cache and set a new one, and
[`disable_cache`](https://cmu-delphi.github.io/epidatr/dev/reference/disable_cache.md)
to disable without deleting
