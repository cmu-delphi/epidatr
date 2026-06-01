# Delphi's ILI Nearby nowcasts

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/nowcast.html>.

Obtains information on outpatient inluenza-like-illness (ILI) from
Delphi's

## Usage

``` r
pub_nowcast(locations, epiweeks = "*", fetch_args = fetch_args_list())
```

## Arguments

- locations:

  character. List of locations to fetch.

- epiweeks:

  [`timeset`](https://cmu-delphi.github.io/epidatr/reference/timeset.md).
  Epiweeks to fetch. Supports
  [`epirange()`](https://cmu-delphi.github.io/epidatr/reference/epirange.md)
  and defaults to all ("\*") dates. Format as
  `epirange(startweek, endweek)`, where startweek and endweek are of the
  form YYYYWW (string or numeric).

- fetch_args:

  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/reference/epidata_call.md).
  See
  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md)
  for details.

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## Details

The full list of location inputs can be accessed at
<https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#us-regions-and-states>
and
<https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#fluview-cities>.

## See also

For example queries showing how to discover signals and build calls, see
[`vignette("signal-discovery", package = "epidatr")`](https://cmu-delphi.github.io/epidatr/articles/signal-discovery.md).

## Examples

``` r

pub_nowcast(locations = "ca", epiweeks = epirange(201201, 201301))
#> # A tibble: 53 × 4
#>    location epiweek    value   std
#>    <chr>    <date>     <dbl> <dbl>
#>  1 ca       2012-01-01  2.92 0.269
#>  2 ca       2012-01-08  3.63 0.271
#>  3 ca       2012-01-15  3.34 0.277
#>  4 ca       2012-01-22  3.15 0.221
#>  5 ca       2012-01-29  3.58 0.225
#>  6 ca       2012-02-05  3.84 0.229
#>  7 ca       2012-02-12  3.34 0.229
#>  8 ca       2012-02-19  3.37 0.227
#>  9 ca       2012-02-26  3.83 0.234
#> 10 ca       2012-03-04  3.51 0.232
#> # ℹ 43 more rows
```
