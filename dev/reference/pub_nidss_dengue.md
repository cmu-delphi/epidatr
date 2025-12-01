# NIDSS dengue cases (Taiwan)

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/nidss_dengue.html>

Obtains counts of confirmed dengue cases in Taiwan from Taiwan National
Infectious Disease Statistical System.

## Usage

``` r
pub_nidss_dengue(locations, epiweeks = "*", fetch_args = fetch_args_list())
```

## Arguments

- locations:

  character. List of locations to fetch.

- epiweeks:

  [`timeset`](https://cmu-delphi.github.io/epidatr/dev/reference/timeset.md).
  Epiweeks to fetch. Supports
  [`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md)
  and defaults to all ("\*") dates. Format as
  `epirange(startweek, endweek)`, where startweek and endweek are of the
  form YYYYWW (string or numeric).

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

Possible location inputs can be found in
<https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/nidss_regions.txt>
and
<https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/nidss_locations.txt>.

## Examples

``` r
pub_nidss_dengue(locations = "taipei", epiweeks = epirange(201201, 201301))
#> # A tibble: 53 × 3
#>    location epiweek    count
#>    <chr>    <date>     <dbl>
#>  1 Taipei   2012-01-01     1
#>  2 Taipei   2012-01-08     1
#>  3 Taipei   2012-01-15     1
#>  4 Taipei   2012-01-22     2
#>  5 Taipei   2012-01-29     4
#>  6 Taipei   2012-02-05     0
#>  7 Taipei   2012-02-12     2
#>  8 Taipei   2012-02-19     0
#>  9 Taipei   2012-02-26     0
#> 10 Taipei   2012-03-04     0
#> # ℹ 43 more rows
```
