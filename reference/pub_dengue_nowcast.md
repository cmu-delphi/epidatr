# Delphi's PAHO dengue nowcasts (North and South America)

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/dengue_nowcast.html>

## Usage

``` r
pub_dengue_nowcast(locations, epiweeks = "*", fetch_args = fetch_args_list())
```

## Arguments

- locations:

  character. List of locations to fetch. See the [codes for countries
  and territories in the
  Americas](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#countries-and-territories-in-the-americas).
  \# nolint

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

## See also

For example queries showing how to discover signals and build calls, see
[`vignette("signal-discovery", package = "epidatr")`](https://cmu-delphi.github.io/epidatr/articles/signal-discovery.md).

## Examples

``` r

pub_dengue_nowcast(
  locations = "pr",
  epiweeks = epirange(201401, 202301)
)
#> # A tibble: 320 × 4
#>    location epiweek    value   std
#>    <chr>    <date>     <dbl> <dbl>
#>  1 pr       2014-02-23  92.7  547.
#>  2 pr       2014-03-02  96.8  601.
#>  3 pr       2014-03-09  93.5  655.
#>  4 pr       2014-03-16  87.8  707.
#>  5 pr       2014-03-23  94.0  758.
#>  6 pr       2014-03-30  88.1  807.
#>  7 pr       2014-04-06  93.7  858.
#>  8 pr       2014-04-13 135.   740.
#>  9 pr       2014-04-20 145.   767.
#> 10 pr       2014-04-27 102.   793.
#> # ℹ 310 more rows
```
