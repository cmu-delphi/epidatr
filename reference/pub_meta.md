# Metadata for the Delphi Epidata API

API docs: <https://cmu-delphi.github.io/delphi-epidata/api/meta.html>

## Usage

``` r
pub_meta(fetch_args = fetch_args_list())
```

## Arguments

- fetch_args:

  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/reference/epidata_call.md).
  See
  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md)
  for details.

## Value

[`list`](https://rdrr.io/r/base/list.html)

## See also

For example queries showing how to discover signals and build calls, see
[`vignette("signal-discovery", package = "epidatr")`](https://cmu-delphi.github.io/epidatr/articles/signal-discovery.md).
