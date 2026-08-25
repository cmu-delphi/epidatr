# Various COVID and flu signals via the COVIDcast endpoint

This is a V4 endpoint. Starting in October 2026, it is tentatively
deprecated in favor of the V5 API. The new API can be accessed via the
[`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/reference/cast_api_queries.md),
[`epidata_archive()`](https://cmu-delphi.github.io/epidatr/reference/cast_api_queries.md),
and
[`epidata_meta()`](https://cmu-delphi.github.io/epidatr/reference/epidata_meta.md)
functions. For more details on the changes, refer to
[`vignette("migration-guide")`](https://cmu-delphi.github.io/epidatr/articles/migration-guide.md),
and visit the [V5 signals
documentation](https://cmu-delphi.github.io/delphi-epidata/api/v5_signals.html)
to see which sources are currently available.

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html>

The primary endpoint for fetching COVID-19 data, providing access to a
wide variety of signals from a wide variety of sources. See the API
documentation link above for more. Delphi's [COVIDcast public
dashboard](https://delphi.cmu.edu/covidcast/) is powered by this
endpoint.

## Usage

``` r
pub_covidcast(
  source,
  signals,
  geo_type,
  time_type,
  geo_values = "*",
  time_values = "*",
  ...,
  as_of = NULL,
  issues = NULL,
  lag = NULL,
  fetch_args = fetch_args_list()
)
```

## Arguments

- source:

  string. The data source to query (see:
  <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html>).

- signals:

  string. The signals to query from a specific source (see:
  <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html>).

- geo_type:

  string. The geographic resolution of the data (see:
  <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html>).

- time_type:

  string. The temporal resolution of the data (either "day" or "week",
  depending on signal).

- geo_values:

  character. The geographies to return. Defaults to all ("\*")
  geographies within requested geographic resolution (see:
  <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html>.).

- time_values:

  [`timeset`](https://cmu-delphi.github.io/epidatr/reference/timeset.md).
  Dates or epiweeks to fetch. Supports
  [`epirange()`](https://cmu-delphi.github.io/epidatr/reference/epirange.md)
  and defaults to all ("\*") dates.

- ...:

  not used for values, forces later arguments to bind by name

- as_of:

  Date. Optionally, the as-of date for the issues to fetch. See the
  "Data Versioning" section for details.

- issues:

  [`timeset`](https://cmu-delphi.github.io/epidatr/reference/timeset.md).
  Optionally, the issue(s) of the data to fetch. See the "Data
  Versioning" section for details.

- lag:

  integer. Optionally, the lag of the issues to fetch. See the "Data
  Versioning" section for details.

- fetch_args:

  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/reference/epidata_call.md).
  See
  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md)
  for details.

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## Data Versioning

Several endpoints support retrieving historical versions of the data.
The following parameters control this and are mutually exclusive (only
one can be provided at a time).

- `as_of`: (Date) Retrieve the data as it was on this date.

- `issues`:
  [`timeset`](https://cmu-delphi.github.io/epidatr/reference/timeset.md)
  Retrieve data from a specific issue date or range of dates.

- `lag`: (integer) Retrieve data with a specific lag from its issue
  date.

If none of these is specified, the most recent version of the data is
returned.

See
[`vignette("versioned-data")`](https://cmu-delphi.github.io/epidatr/articles/versioned-data.md)
for details and more ways to specify versioned data.

## See also

For example queries showing how to discover signals and build calls, see
[`vignette("signal-discovery", package = "epidatr")`](https://cmu-delphi.github.io/epidatr/articles/signal-discovery.md).

## See also

[`pub_covidcast_meta()`](https://cmu-delphi.github.io/epidatr/reference/pub_covidcast_meta.md),
[`covidcast_epidata()`](https://cmu-delphi.github.io/epidatr/reference/covidcast_epidata.md),
[`epirange()`](https://cmu-delphi.github.io/epidatr/reference/epirange.md)

## Examples

``` r

pub_covidcast(
  source = "jhu-csse",
  signals = "confirmed_7dav_incidence_prop",
  geo_type = "state",
  time_type = "day",
  geo_values = c("ca", "fl"),
  time_values = epirange(20200601, 20200801)
)
#> # A tibble: 124 × 15
#>    geo_value signal    source geo_type time_type time_value direction issue     
#>    <chr>     <chr>     <chr>  <fct>    <fct>     <date>         <dbl> <date>    
#>  1 ca        confirme… jhu-c… state    day       2020-06-01        NA 2023-03-10
#>  2 fl        confirme… jhu-c… state    day       2020-06-01        NA 2023-03-03
#>  3 ca        confirme… jhu-c… state    day       2020-06-02        NA 2023-03-10
#>  4 fl        confirme… jhu-c… state    day       2020-06-02        NA 2023-03-03
#>  5 ca        confirme… jhu-c… state    day       2020-06-03        NA 2023-03-10
#>  6 fl        confirme… jhu-c… state    day       2020-06-03        NA 2023-03-03
#>  7 ca        confirme… jhu-c… state    day       2020-06-04        NA 2023-03-10
#>  8 fl        confirme… jhu-c… state    day       2020-06-04        NA 2023-03-03
#>  9 ca        confirme… jhu-c… state    day       2020-06-05        NA 2023-03-10
#> 10 fl        confirme… jhu-c… state    day       2020-06-05        NA 2023-03-03
#> # ℹ 114 more rows
#> # ℹ 7 more variables: lag <dbl>, missing_value <dbl>, missing_stderr <dbl>,
#> #   missing_sample_size <dbl>, value <dbl>, stderr <dbl>, sample_size <dbl>
pub_covidcast(
  source = "jhu-csse",
  signals = "confirmed_7dav_incidence_prop",
  geo_type = "state",
  time_type = "day",
  geo_values = "*",
  time_values = epirange(20200601, 20200801)
)
#> # A tibble: 3,472 × 15
#>    geo_value signal    source geo_type time_type time_value direction issue     
#>    <chr>     <chr>     <chr>  <fct>    <fct>     <date>         <dbl> <date>    
#>  1 ak        confirme… jhu-c… state    day       2020-06-01        NA 2023-03-03
#>  2 al        confirme… jhu-c… state    day       2020-06-01        NA 2023-03-03
#>  3 ar        confirme… jhu-c… state    day       2020-06-01        NA 2023-03-03
#>  4 as        confirme… jhu-c… state    day       2020-06-01        NA 2023-03-03
#>  5 az        confirme… jhu-c… state    day       2020-06-01        NA 2023-03-03
#>  6 ca        confirme… jhu-c… state    day       2020-06-01        NA 2023-03-10
#>  7 co        confirme… jhu-c… state    day       2020-06-01        NA 2023-03-03
#>  8 ct        confirme… jhu-c… state    day       2020-06-01        NA 2023-03-03
#>  9 dc        confirme… jhu-c… state    day       2020-06-01        NA 2023-03-03
#> 10 de        confirme… jhu-c… state    day       2020-06-01        NA 2023-03-03
#> # ℹ 3,462 more rows
#> # ℹ 7 more variables: lag <dbl>, missing_value <dbl>, missing_stderr <dbl>,
#> #   missing_sample_size <dbl>, value <dbl>, stderr <dbl>, sample_size <dbl>
```
