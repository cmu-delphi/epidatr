# Turn off the caching for this session

Disable caching until you call `set_cache` or restart R. The files
defining the cache are untouched. If you are looking to disable the
caching more permanently, set `EPIDATR_USE_CACHE=FALSE` as environmental
variable in your `.Renviron`.

## Usage

``` r
disable_cache()
```

## Value

[`NULL`](https://rdrr.io/r/base/NULL.html) no return value, all effects
are stored in the package environment

## See also

[`set_cache`](https://cmu-delphi.github.io/epidatr/reference/set_cache.md)
to start a new cache (and general caching info),
[`clear_cache`](https://cmu-delphi.github.io/epidatr/reference/clear_cache.md)
to delete the cache and set a new one, and
[`cache_info`](https://cmu-delphi.github.io/epidatr/reference/cache_info.md)
