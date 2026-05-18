# Google Flu Trends flu search volume

API docs: <https://cmu-delphi.github.io/delphi-epidata/api/gft.html>

Obtains estimates of inluenza activity based on volume of certain search
queries from Google.

## Usage

``` r
pub_gft(locations, epiweeks = "*", fetch_args = fetch_args_list())
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

  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md).
  See
  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md)
  for details.

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## Details

Google has discontinued Flu Trends and this is now a static endpoint.
Possibile input for locations can be found in
<https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#hhs-regions>,
<https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#us-states>,
and
<https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#selected-us-cities>.

## Examples

``` r

pub_gft(locations = "hhs1", epiweeks = epirange(201201, 202001))
#> # A tibble: 189 × 3
#>    location epiweek      num
#>    <chr>    <date>     <dbl>
#>  1 hhs1     2012-01-01  1567
#>  2 hhs1     2012-01-08  1339
#>  3 hhs1     2012-01-15  1220
#>  4 hhs1     2012-01-22  1271
#>  5 hhs1     2012-01-29  1181
#>  6 hhs1     2012-02-05   994
#>  7 hhs1     2012-02-12  1091
#>  8 hhs1     2012-02-19  1030
#>  9 hhs1     2012-02-26   856
#> 10 hhs1     2012-03-04   799
#> # ℹ 179 more rows
```
