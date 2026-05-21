# CDC FluView flu tests from clinical labs

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/fluview_clinical.html>

## Usage

``` r
pub_fluview_clinical(
  regions,
  epiweeks = "*",
  ...,
  issues = NULL,
  lag = NULL,
  fetch_args = fetch_args_list()
)
```

## Arguments

- regions:

  character. Vector of location IDs to fetch. Can be "nat" for national,
  "hhs1"–"hhs10" for HHS Regions, "cen1"–"cen9" for census divisions,
  lowercase two-letter state or territory abbreviations for most states
  and territories,"jfk" for New York City, or "ny_minus_jfk" for upstate
  New York. Full list of locations is available
  [here](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#us-regions-and-states)
  and
  [here](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#fluview-cities).

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

pub_fluview_clinical(regions = "nat", epiweeks = epirange(201601, 201701))
#> # A tibble: 14 × 11
#>    release_date region issue      epiweek      lag total_specimens total_a
#>    <date>       <chr>  <date>     <date>     <dbl>           <dbl>   <dbl>
#>  1 2018-10-08   nat    2018-09-23 2016-10-02   103           13380     120
#>  2 2018-10-08   nat    2018-09-23 2016-10-09   102           14053     108
#>  3 2018-10-08   nat    2018-09-23 2016-10-16   101           15110     115
#>  4 2018-10-08   nat    2018-09-23 2016-10-23   100           15312     143
#>  5 2018-10-08   nat    2018-09-23 2016-10-30    99           16652     201
#>  6 2018-10-08   nat    2018-09-23 2016-11-06    98           17811     271
#>  7 2018-10-08   nat    2018-09-23 2016-11-13    97           18948     347
#>  8 2018-10-08   nat    2018-09-23 2016-11-20    96           18520     451
#>  9 2018-10-08   nat    2018-09-23 2016-11-27    95           21542     532
#> 10 2018-10-08   nat    2018-09-23 2016-12-04    94           20564     696
#> 11 2018-10-08   nat    2018-09-23 2016-12-11    93           23476    1359
#> 12 2018-10-08   nat    2018-09-23 2016-12-18    92           26775    2556
#> 13 2018-10-08   nat    2018-09-23 2016-12-25    91           33273    4270
#> 14 2018-10-08   nat    2018-09-23 2017-01-01    90           35028    4288
#> # ℹ 4 more variables: total_b <dbl>, percent_positive <dbl>, percent_a <dbl>,
#> #   percent_b <dbl>
```
