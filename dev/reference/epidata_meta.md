# Get cast-API source metadata

`epidata_meta` returns source-level metadata from the cast-API,
including `report_time` ranges, `reference_time` ranges, and lists of
available signals and geo types.

## Usage

``` r
epidata_meta(source = NULL, fetch_args = fetch_args_list())
```

## Arguments

- source:

  string. The data source to query. If `NULL` (default), returns
  metadata for all available sources. If specified, returns metadata for
  the given source.

- fetch_args:

  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md).
  See
  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md)
  for details.

## Value

list. If `source` is `NULL`, a named list of source metadata objects. If
`source` is specified, the metadata list for that source.

## See also

For example queries showing how to discover signals and build calls, see
[`vignette("signal-discovery", package = "epidatr")`](https://cmu-delphi.github.io/epidatr/dev/articles/signal-discovery.md).

## See also

[`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md),
[`epidata_archive()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md),
[`epidata()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md),
[`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md)
