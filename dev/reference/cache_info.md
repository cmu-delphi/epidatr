# Describe current cache

Print out the information about the cache (as would be returned by
cachem's `info()` method).

## Usage

``` r
cache_info()
```

## Value

[`list`](https://rdrr.io/r/base/list.html) containing the info result as
created by cachem

## See also

[`set_cache`](https://cmu-delphi.github.io/epidatr/dev/reference/set_cache.md)
to start a new cache (and general caching info),
[`clear_cache`](https://cmu-delphi.github.io/epidatr/dev/reference/clear_cache.md)
to delete the cache and set a new one, and
[`disable_cache`](https://cmu-delphi.github.io/epidatr/dev/reference/disable_cache.md)
to disable without deleting
