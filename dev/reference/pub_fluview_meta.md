# Metadata for the FluView endpoint

**\[deprecated\]**

This is a V3/V4 endpoint. **As of September 22, 2026, the V3 and V4 APIs
no longer receive new data.** They still serve the historical data they
already have, but current data is only added to the V5 API, accessed via
the
[`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md),
[`epidata_archive()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md),
and
[`epidata_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_meta.md)
functions. For more details on the changes, refer to
[`vignette("migration-guide")`](https://cmu-delphi.github.io/epidatr/dev/articles/migration-guide.md),
and visit the [V5 signals
documentation](https://cmu-delphi.github.io/delphi-epidata/api/v5_signals.html)
to see which sources are currently available.

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

A [`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)
with one row per observation: one location at one time (and, for
versioned data, one issue). The columns are the fields the endpoint
returns; the API documentation linked at the top of the page describes
what each one contains. The `fetch_args` argument controls the parsing,
e.g. `fields` restricts the columns.

## See also

[`pub_fluview()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview.md),
[`vignette("signal-discovery")`](https://cmu-delphi.github.io/epidatr/dev/articles/signal-discovery.md)

## Examples

``` r

pub_fluview_meta()
#> Warning: `pub_fluview_meta()` uses the V4 Epidata API.
#> ℹ As of September 22, 2026, V4 no longer receives new data. It still serves the
#>   historical data it already has, but for current data you must use the V5
#>   endpoints (`epidata_snapshot()`, `epidata_archive()`, `epidata_meta()`) with
#>   an up-to-date epidatr (and epiprocess, if you use it).
#> ℹ See `vignette("migration-guide")` (or
#>   <https://cmu-delphi.github.io/epidatr/articles/migration-guide.html>) for the
#>   endpoint, argument, and column mapping.
#> This warning is displayed once every 8 hours.
#> # A tibble: 1 × 3
#>   latest_update latest_issue table_rows
#>   <date>        <date>            <dbl>
#> 1 2026-11-06    2026-10-25      2908849
```
