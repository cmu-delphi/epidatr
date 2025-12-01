# Metadata for the COVIDcast endpoint

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/covidcast_meta.html>.

Fetch a summary of metadata for all sources and signals that are
available in the API, along with basic summary statistics such as the
dates they are available, the geographic levels at which they are
reported, and etc.

## Usage

``` r
pub_covidcast_meta(fetch_args = fetch_args_list())
```

## Arguments

- fetch_args:

  [`fetch_args`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md).
  See
  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md)
  for details.

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## See also

[`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md),[`covidcast_epidata()`](https://cmu-delphi.github.io/epidatr/dev/reference/covidcast_epidata.md)

## Examples

``` r
pub_covidcast_meta()
#> # A tibble: 2,666 × 15
#>    data_source         signal time_type geo_type min_time max_time num_locations
#>    <chr>               <chr>  <fct>     <fct>       <dbl>    <dbl>         <dbl>
#>  1 beta_google_sympto… s01_r… day       county   20250919 20251029          2718
#>  2 beta_google_sympto… s01_r… day       state    20250919 20251029            51
#>  3 beta_google_sympto… s01_r… day       hrr      20250919 20251029           299
#>  4 beta_google_sympto… s01_r… day       msa      20250919 20251029           380
#>  5 beta_google_sympto… s01_s… day       county   20250925 20251029          2718
#>  6 beta_google_sympto… s01_s… day       state    20250925 20251029            51
#>  7 beta_google_sympto… s01_s… day       hrr      20250925 20251029           299
#>  8 beta_google_sympto… s01_s… day       msa      20250925 20251029           380
#>  9 beta_google_sympto… s02_r… day       county   20250919 20251029          2718
#> 10 beta_google_sympto… s02_r… day       state    20250919 20251029            51
#> # ℹ 2,656 more rows
#> # ℹ 8 more variables: min_value <dbl>, max_value <dbl>, mean_value <dbl>,
#> #   stdev_value <dbl>, last_update <dbl>, max_issue <dbl>, min_lag <dbl>,
#> #   max_lag <dbl>
```
