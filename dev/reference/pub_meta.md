# Metadata for the Delphi Epidata API

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

API docs: <https://cmu-delphi.github.io/delphi-epidata/api/meta.html>

## Usage

``` r
pub_meta(fetch_args = fetch_args_list())
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

[`list`](https://rdrr.io/r/base/list.html)

## See also

For example queries showing how to discover signals and build calls, see
[`vignette("signal-discovery", package = "epidatr")`](https://cmu-delphi.github.io/epidatr/dev/articles/signal-discovery.md).
