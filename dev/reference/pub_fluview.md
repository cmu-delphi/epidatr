# CDC FluView ILINet outpatient doctor visits

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/fluview.html>. For

Obtains information on outpatient inluenza-like-illness (ILI) from U.S.
Outpatient Influenza-like Illness Surveillance Network (ILINet).

more information on ILINet, see
<https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html>.

## Usage

``` r
pub_fluview(
  regions,
  epiweeks = "*",
  ...,
  issues = NULL,
  lag = NULL,
  auth = NULL,
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

- auth:

  string. Your restricted access key (not the same as API key).

- fetch_args:

  [`fetch_args`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md).
  See
  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md)
  for details.

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## Details

The full list of location inputs can be accessed at
<https://github.com/cmu-delphi/delphi-epidata/blob/main/src/acquisition/fluview/fluview_locations.py>.

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

## Examples

``` r
pub_fluview(regions = "nat", epiweeks = epirange(201201, 202005))
#> # A tibble: 422 × 16
#>    release_date region issue      epiweek      lag num_ili num_patients
#>    <date>       <chr>  <date>     <date>     <dbl>   <dbl>        <dbl>
#>  1 2017-10-24   nat    2017-10-01 2012-01-01   300   11992       678631
#>  2 2017-10-24   nat    2017-10-01 2012-01-08   299   11543       747894
#>  3 2017-10-24   nat    2017-10-01 2012-01-15   298   11939       724623
#>  4 2017-10-24   nat    2017-10-01 2012-01-22   297   13209       784244
#>  5 2017-10-24   nat    2017-10-01 2012-01-29   296   14448       775298
#>  6 2017-10-24   nat    2017-10-01 2012-02-05   295   14624       784516
#>  7 2017-10-24   nat    2017-10-01 2012-02-12   294   15929       789193
#>  8 2017-10-24   nat    2017-10-01 2012-02-19   293   16137       767022
#>  9 2017-10-24   nat    2017-10-01 2012-02-26   292   16518       788242
#> 10 2017-10-24   nat    2017-10-01 2012-03-04   291   15763       749198
#> # ℹ 412 more rows
#> # ℹ 9 more variables: num_providers <dbl>, num_age_0 <dbl>, num_age_1 <dbl>,
#> #   num_age_2 <dbl>, num_age_3 <dbl>, num_age_4 <dbl>, num_age_5 <dbl>,
#> #   wili <dbl>, ili <dbl>
```
