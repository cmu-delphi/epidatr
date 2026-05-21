# Metadata for the COVIDcast endpoint

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/covidcast_meta.html>.

Fetch a summary of metadata for all sources and signals that are
available in the API, along with basic summary statistics such as the
dates they are available, the geographic levels at which they are
reported, and etc.

The result can be filtered server-side by passing `signals`,
`time_type`, and/or `geo_type`. Omitted filters (the default) return
metadata for everything.

## Usage

``` r
pub_covidcast_meta(
  signals = NULL,
  time_type = NULL,
  geo_type = NULL,
  fetch_args = fetch_args_list()
)
```

## Arguments

- signals:

  character. Optionally, the signals to return metadata for, each
  formatted as `"source:signal"` (e.g. `"fb-survey:smoothed_cli"`).
  Defaults to all signals.

- time_type:

  string. The temporal resolution of the data (either "day" or "week",
  depending on signal).

- geo_type:

  string. Optionally, a single geographic resolution to return metadata
  for (see:
  <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html>).
  Defaults to all geographic resolutions.

- fetch_args:

  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md).
  See
  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md)
  for details.

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## See also

For example queries showing how to discover signals and build calls, see
[`vignette("signal-discovery", package = "epidatr")`](https://cmu-delphi.github.io/epidatr/dev/articles/signal-discovery.md).

## See also

[`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md),[`covidcast_epidata()`](https://cmu-delphi.github.io/epidatr/dev/reference/covidcast_epidata.md)

## Examples

``` r

pub_covidcast_meta()
#> # A tibble: 2,964 × 15
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
#> # ℹ 2,954 more rows
#> # ℹ 8 more variables: min_value <dbl>, max_value <dbl>, mean_value <dbl>,
#> #   stdev_value <dbl>, last_update <dttm>, max_issue <dbl>, min_lag <dbl>,
#> #   max_lag <dbl>
# All signals from the Facebook survey data source
pub_covidcast_meta(
  signals = "fb-survey:*"
)
#> # A tibble: 1,753 × 15
#>    data_source signal         time_type geo_type min_time max_time num_locations
#>    <chr>       <chr>          <fct>     <fct>       <dbl>    <dbl>         <dbl>
#>  1 fb-survey   raw_cli        day       county   20200406 20220625           642
#>  2 fb-survey   raw_cli        day       hrr      20200406 20220625           306
#>  3 fb-survey   raw_cli        day       msa      20200406 20220625           342
#>  4 fb-survey   raw_cli        day       nation   20200406 20220626             1
#>  5 fb-survey   raw_cli        day       state    20200406 20220625            51
#>  6 fb-survey   raw_hh_cmnty_… day       county   20200415 20220625           351
#>  7 fb-survey   raw_hh_cmnty_… day       hrr      20200415 20220625           288
#>  8 fb-survey   raw_hh_cmnty_… day       msa      20200415 20220625           215
#>  9 fb-survey   raw_hh_cmnty_… day       nation   20200415 20220626             1
#> 10 fb-survey   raw_hh_cmnty_… day       state    20200415 20220625            51
#> # ℹ 1,743 more rows
#> # ℹ 8 more variables: min_value <dbl>, max_value <dbl>, mean_value <dbl>,
#> #   stdev_value <dbl>, last_update <dttm>, max_issue <dbl>, min_lag <dbl>,
#> #   max_lag <dbl>
# All signals with time_type "day".
pub_covidcast_meta(
  time_type = "day",
)
#> # A tibble: 2,474 × 15
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
#> # ℹ 2,464 more rows
#> # ℹ 8 more variables: min_value <dbl>, max_value <dbl>, mean_value <dbl>,
#> #   stdev_value <dbl>, last_update <dttm>, max_issue <dbl>, min_lag <dbl>,
#> #   max_lag <dbl>
# All signals with geo_type "state".
pub_covidcast_meta(
  geo_type = "state",
)
#> # A tibble: 622 × 15
#>    data_source         signal time_type geo_type min_time max_time num_locations
#>    <chr>               <chr>  <fct>     <fct>       <dbl>    <dbl>         <dbl>
#>  1 beta_google_sympto… s01_r… day       state    20250919 20251029            51
#>  2 beta_google_sympto… s01_s… day       state    20250925 20251029            51
#>  3 beta_google_sympto… s02_r… day       state    20250919 20251029            51
#>  4 beta_google_sympto… s02_s… day       state    20250925 20251029            51
#>  5 beta_google_sympto… s03_r… day       state    20250919 20251029            51
#>  6 beta_google_sympto… s03_s… day       state    20250925 20251029            51
#>  7 beta_google_sympto… s04_r… day       state    20250919 20251029            51
#>  8 beta_google_sympto… s04_s… day       state    20250925 20251029            51
#>  9 beta_google_sympto… s05_r… day       state    20250919 20251029            51
#> 10 beta_google_sympto… s05_s… day       state    20250925 20251029            51
#> # ℹ 612 more rows
#> # ℹ 8 more variables: min_value <dbl>, max_value <dbl>, mean_value <dbl>,
#> #   stdev_value <dbl>, last_update <dttm>, max_issue <dbl>, min_lag <dbl>,
#> #   max_lag <dbl>
```
