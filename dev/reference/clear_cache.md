# Manually reset the cache, deleting all currently saved data and starting afresh

Deletes the current cache and resets a new cache. Deletes local data! If
you are using a session unique cache, the previous settings will be
reused. If you pass in new `set_cache` arguments, they will take
precedence over the previous settings.

## Usage

``` r
clear_cache(..., disable = FALSE)
```

## Arguments

- ...:

  Arguments passed on to
  [`set_cache`](https://cmu-delphi.github.io/epidatr/dev/reference/set_cache.md)

  `cache_dir`

  :   the directory in which the cache is stored. By default, this is
      `rappdirs::user_cache_dir("R", version = "epidatr")`. The path can
      be either relative or absolute. The environmental variable is
      `EPIDATR_CACHE_DIR`.

  `days`

  :   the maximum length of time in days to keep any particular cached
      call. By default this is `1`. The environmental variable is
      `EPIDATR_CACHE_MAX_AGE_DAYS`.

  `max_size`

  :   the size of the entire cache, in MB, at which to start pruning
      entries. By default this is `1024`, or 1GB. The environmental
      variable is `EPIDATR_CACHE_MAX_SIZE_MB`.

  `logfile`

  :   where cachem's log of transactions is stored, relative to the
      cache directory. By default, it is `"logfile.txt"`. The
      environmental variable is `EPIDATR_CACHE_LOGFILE`.

  `confirm`

  :   whether to confirm directory creation. default is `TRUE`; should
      only be set in non-interactive scripts

  `startup`

  :   indicates whether the function is being called on startup. Affects
      suppressability of the messages. Default is `FALSE`.

- disable:

  instead of setting a new cache, disable caching entirely; defaults to
  `FALSE`

## Value

[`NULL`](https://rdrr.io/r/base/NULL.html) no return value, all effects
are stored in the package environment

## See also

[`set_cache`](https://cmu-delphi.github.io/epidatr/dev/reference/set_cache.md)
to start a new cache (and general caching info),
[`disable_cache`](https://cmu-delphi.github.io/epidatr/dev/reference/disable_cache.md)
to only disable without deleting, and
[`cache_info`](https://cmu-delphi.github.io/epidatr/dev/reference/cache_info.md)
