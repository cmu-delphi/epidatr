# Get cast-API source metadata

`epidata_meta` returns source-level metadata from the cast-API,
including version ranges, time value ranges, and lists of available
signals and geo types.

## Usage

``` r
epidata_meta(source, fetch_args = fetch_args_list())
```

## Arguments

- source:

  string. The data source to query.

- fetch_args:

  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md).
  See
  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md)
  for details.

## Value

list

## See also

[`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md),
[`epidata_archive()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md),
[`epidata()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md),
[`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md)
