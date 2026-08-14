# Metadata for the FluView endpoint

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/fluview_meta.html>

## Usage

``` r
pub_fluview_meta(fetch_args = fetch_args_list())
```

## Arguments

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

[`pub_fluview()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview.md)

## Examples

``` r

pub_fluview_meta()
#> # A tibble: 1 × 3
#>   latest_update latest_issue table_rows
#>   <date>        <date>            <dbl>
#> 1 2026-08-07    2026-07-26      2830379
```
