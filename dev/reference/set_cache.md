# Create or renew a cache for this session

By default, epidatr re-requests data from the API on every call of
`fetch`. In case you find yourself repeatedly calling the same data, you
can enable the cache using either this function for a given session, or
environmental variables for a persistent cache. The typical recommended
workflow for using the cache is to set the environmental variables
`EPIDATR_USE_CACHE=TRUE` and
`EPIDATR_CACHE_DIRECTORY="/your/directory/here"`in your `.Renviron`, for
example by calling
[`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html).
See the parameters below for some more configurables if you're so
inclined.

`set_cache` (re)defines the cache to use in a particular R session. This
does not clear existing data at any previous location, but instead
creates a handle to the new cache using
[cachem](https://cachem.r-lib.org/index.html) that seamlessly handles
caching for you. Say your cache is normally stored in some default
directory, but for the current session you want to save your results in
`~/my/temporary/savedirectory`, then you would call
`set_cache(dir = "~/my/temporary/savedirectory")`. Or if you know the
data from 2 days ago is wrong, you could call `set_cache(days = 1)` to
clear older data whenever the cache is referenced. In both cases, these
changes would only last for a single session (though the deleted data
would be gone permanently!).

An important feature of the caching in this package is that only calls
which specify either `issues` before a certain date, or `as_of` before a
certain date will actually cache. For example the call

    pub_covidcast(
      source = "jhu-csse",
      signals = "confirmed_7dav_incidence_prop",
      geo_type = "state",
      time_type = "day",
      geo_values = "ca,fl",
      time_values = epirange(20200601, 20230801)
    )

*won't* cache, since it is possible for the cache to be invalidated by
new releases with no warning. On the other hand, the call

    pub_covidcast(
      source = "jhu-csse",
      signals = "confirmed_7dav_incidence_prop",
      geo_type = "state",
      time_type = "day",
      geo_values = "ca,fl",
      time_values = epirange(20200601, 20230801),
      as_of = "2023-08-01"
    )

*will* cache, since normal new versions of data can't invalidate it
(since they would be `as_of` a later date). It is still possible that
Delphi may patch such data, but the frequency is on the order of months
rather than days. We are working on creating a public channel to
communicate such updates. While specifying `issues` will usually cache,
a call with `issues="*"` won't cache, since its subject to cache
invalidation by normal versioning.

On the backend, the cache uses cachem, with filenames generated using an
md5 encoding of the call url. Each file corresponds to a unique
epidata-API call.

## Usage

``` r
set_cache(
  cache_dir = NULL,
  days = NULL,
  max_size = NULL,
  logfile = NULL,
  confirm = TRUE,
  startup = FALSE
)
```

## Arguments

- cache_dir:

  the directory in which the cache is stored. By default, this is
  `rappdirs::user_cache_dir("R", version = "epidatr")`. The path can be
  either relative or absolute. The environmental variable is
  `EPIDATR_CACHE_DIR`.

- days:

  the maximum length of time in days to keep any particular cached call.
  By default this is `1`. The environmental variable is
  `EPIDATR_CACHE_MAX_AGE_DAYS`.

- max_size:

  the size of the entire cache, in MB, at which to start pruning
  entries. By default this is `1024`, or 1GB. The environmental variable
  is `EPIDATR_CACHE_MAX_SIZE_MB`.

- logfile:

  where cachem's log of transactions is stored, relative to the cache
  directory. By default, it is `"logfile.txt"`. The environmental
  variable is `EPIDATR_CACHE_LOGFILE`.

- confirm:

  whether to confirm directory creation. default is `TRUE`; should only
  be set in non-interactive scripts

- startup:

  indicates whether the function is being called on startup. Affects
  suppressability of the messages. Default is `FALSE`.

## Value

[`NULL`](https://rdrr.io/r/base/NULL.html) no return value, all effects
are stored in the package environment

## See also

[`clear_cache`](https://cmu-delphi.github.io/epidatr/dev/reference/clear_cache.md)
to delete the old cache while making a new one,
[`disable_cache`](https://cmu-delphi.github.io/epidatr/dev/reference/disable_cache.md)
to disable without deleting, and
[`cache_info`](https://cmu-delphi.github.io/epidatr/dev/reference/cache_info.md)

## Examples

``` r
set_cache(
  cache_dir = tempdir(),
  days = 14,
  max_size = 512,
  logfile = "logs.txt"
)
#> ! epidatr cache is being used (set env var EPIDATR_USE_CACHE=FALSE if not
#>   intended).
#> ℹ The cache directory is /tmp/RtmpOzvujW.
#> ℹ The cache will be cleared after 14 days and will be pruned if it exceeds 512
#>   MB.
#> ℹ The log of cache transactions is stored at /tmp/RtmpOzvujW/logs.txt.
```
