# NIDSS flu doctor visits (Taiwan)

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/nidss_flu.html>

Obtains information on outpatient inluenza-like-illness from Taiwan
National Infectious Disease Statistical System.

## Usage

``` r
pub_nidss_flu(
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

  character. List of regions to fetch. See [Taiwan's geographic
  codes](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#nidss)
  for details.

- epiweeks:

  [`timeset`](https://cmu-delphi.github.io/epidatr/reference/timeset.md).
  Epiweeks to fetch. Supports
  [`epirange()`](https://cmu-delphi.github.io/epidatr/reference/epirange.md)
  and defaults to all ("\*") dates. Format as
  `epirange(startweek, endweek)`, where startweek and endweek are of the
  form YYYYWW (string or numeric).

- ...:

  not used for values, forces later arguments to bind by name

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

## Examples

``` r

pub_nidss_flu(regions = "taipei", epiweeks = epirange(201501, 201601))
#> # A tibble: 53 × 7
#>    release_date region issue      epiweek      lag visits   ili
#>    <date>       <chr>  <date>     <date>     <dbl>  <dbl> <dbl>
#>  1 2016-01-05   Taipei 2015-12-27 2015-01-04    51  16698  0.96
#>  2 2016-01-12   Taipei 2016-01-03 2015-01-11    51  17146  1.03
#>  3 2016-01-19   Taipei 2016-01-10 2015-01-18    51  17817  1.07
#>  4 2016-01-26   Taipei 2016-01-17 2015-01-25    51  18168  1.13
#>  5 2016-02-02   Taipei 2016-01-24 2015-02-01    51  18321  1.11
#>  6 2016-02-02   Taipei 2016-01-24 2015-02-08    50  19799  1.08
#>  7 2016-02-02   Taipei 2016-01-24 2015-02-15    49  11879  1.25
#>  8 2016-02-02   Taipei 2016-01-24 2015-02-22    48  17397  1.19
#>  9 2016-02-02   Taipei 2016-01-24 2015-03-01    47  17849  1.07
#> 10 2016-02-02   Taipei 2016-01-24 2015-03-08    46  17441  1.08
#> # ℹ 43 more rows
```
