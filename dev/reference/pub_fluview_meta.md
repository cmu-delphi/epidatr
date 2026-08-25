# Metadata for the FluView endpoint

This is a V4 endpoint. Starting in October 2026, it is tentatively
deprecated in favor of the V5 API. The new API can be accessed via the
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

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## See also

For example queries showing how to discover signals and build calls, see
[`vignette("signal-discovery", package = "epidatr")`](https://cmu-delphi.github.io/epidatr/dev/articles/signal-discovery.md).

## See also

[`pub_fluview()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview.md)

## Examples

``` r

pub_fluview_meta()
#> Warning: `pub_fluview_meta()` uses the V4 Epidata API.
#> ℹ Starting in October 2026, V4 is tentatively deprecated in favor of the V5
#>   API.
#> ℹ See `vignette("migration-guide")` (or
#>   <https://cmu-delphi.github.io/epidatr/articles/migration-guide.html>) for the
#>   V5 endpoints and how to move to them. Old data will remain available for at
#>   least a year, but new ingestion will end.
#> This warning is displayed once every 8 hours.
#> # A tibble: 1 × 3
#>   latest_update latest_issue table_rows
#>   <date>        <date>            <dbl>
#> 1 2026-08-21    2026-08-09      2844081
```
