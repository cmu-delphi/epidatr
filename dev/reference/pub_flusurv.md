# CDC FluSurv flu hospitalizations

This is a V4 endpoint. Starting in October 2026, it is tentatively
deprecated in favor of the V5 API. The new API can be accessed via the
[`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md),
[`epidata_archive()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md),
and
[`epidata_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_meta.md)
functions. For more details on the changes, refer to
[`vignette("migration-guide")`](https://cmu-delphi.github.io/epidatr/dev/articles/migration-guide.md),
and visit the [V5 signals
documentation](https://cmu-delphi.github.io/delphi-epidata/api/v5_signals.html)
to see which sources are currently available.

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/flusurv.html>.

Obtain information on influenza hospitalization rates from the Center of
Disease Control.

See also <https://gis.cdc.gov/GRASP/Fluview/FluHospRates.html>.

## Usage

``` r
pub_flusurv(
  locations,
  epiweeks = "*",
  ...,
  issues = NULL,
  lag = NULL,
  fetch_args = fetch_args_list()
)
```

## Arguments

- locations:

  character. List of locations to fetch. See [geographic
  codes](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#flusurv-locations)
  for details.

- epiweeks:

  [`timeset`](https://cmu-delphi.github.io/epidatr/dev/reference/timeset.md).
  Epiweeks to fetch. Supports
  [`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md)
  and defaults to all ("\*") dates. Format as
  `epirange(startweek, endweek)`, where startweek and endweek are of the
  form YYYYWW (string or numeric).

- ...:

  not used for values, forces later arguments to bind by name

- issues:

  [`timeset`](https://cmu-delphi.github.io/epidatr/dev/reference/timeset.md).
  Optionally, the issue(s) of the data to fetch. See the "Data
  Versioning" section for details.

- lag:

  integer. Optionally, the lag of the issues to fetch. See the "Data
  Versioning" section for details.

- fetch_args:

  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md).
  See
  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md)
  for details.

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## Details

The list of location argument can be found in
<https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/flusurv_locations.txt>.

## Data Versioning

Several endpoints support retrieving historical versions of the data.
The following parameters control this and are mutually exclusive (only
one can be provided at a time).

- `as_of`: (Date) Retrieve the data as it was on this date.

- `issues`:
  [`timeset`](https://cmu-delphi.github.io/epidatr/dev/reference/timeset.md)
  Retrieve data from a specific issue date or range of dates.

- `lag`: (integer) Retrieve data with a specific lag from its issue
  date.

If none of these is specified, the most recent version of the data is
returned.

See
[`vignette("versioned-data")`](https://cmu-delphi.github.io/epidatr/dev/articles/versioned-data.md)
for details and more ways to specify versioned data.

## See also

For example queries showing how to discover signals and build calls, see
[`vignette("signal-discovery", package = "epidatr")`](https://cmu-delphi.github.io/epidatr/dev/articles/signal-discovery.md).

## Examples

``` r

pub_flusurv(locations = "ca", epiweeks = epirange(201701, 201801))
#> Warning: `pub_flusurv()` uses the V4 Epidata API.
#> ℹ Starting in October 2026, V4 is tentatively deprecated in favor of the V5
#>   API.
#> ℹ See `vignette("migration-guide")` (or
#>   <https://cmu-delphi.github.io/epidatr/articles/migration-guide.html>) for the
#>   V5 endpoints and how to move to them. Old data will remain available for at
#>   least a year, but new ingestion will end.
#> This warning is displayed once every 8 hours.
#> # A tibble: 31 × 34
#>    release_date location season  issue      epiweek      lag rate_age_0
#>    <date>       <chr>    <chr>   <date>     <date>     <dbl>      <dbl>
#>  1 2025-11-03   CA       2016-17 2025-09-14 2017-01-01   454        3.4
#>  2 2025-11-03   CA       2016-17 2025-09-14 2017-01-08   453        2.9
#>  3 2025-11-03   CA       2016-17 2025-09-14 2017-01-15   452        3.4
#>  4 2025-11-03   CA       2016-17 2025-09-14 2017-01-22   451        2  
#>  5 2025-11-03   CA       2016-17 2025-09-14 2017-01-29   450        2  
#>  6 2025-11-03   CA       2016-17 2025-09-14 2017-02-05   449        2  
#>  7 2025-11-03   CA       2016-17 2025-09-14 2017-02-12   448        2  
#>  8 2025-11-03   CA       2016-17 2025-09-14 2017-02-19   447        0.5
#>  9 2025-11-03   CA       2016-17 2025-09-14 2017-02-26   446        2  
#> 10 2025-11-03   CA       2016-17 2025-09-14 2017-03-05   445        0.5
#> # ℹ 21 more rows
#> # ℹ 27 more variables: rate_age_1 <dbl>, rate_age_2 <dbl>, rate_age_3 <dbl>,
#> #   rate_age_4 <dbl>, rate_overall <dbl>, rate_age_5 <dbl>, rate_age_6 <dbl>,
#> #   rate_age_7 <dbl>, rate_age_18t29 <dbl>, rate_age_30t39 <dbl>,
#> #   rate_age_40t49 <dbl>, rate_age_5t11 <dbl>, rate_age_12t17 <dbl>,
#> #   rate_age_lt18 <dbl>, rate_age_gte18 <dbl>, rate_age_1t4 <dbl>,
#> #   rate_age_gte75 <dbl>, rate_age_0tlt1 <dbl>, rate_race_white <dbl>, …
```
