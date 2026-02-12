# Metadata for the NoroSTAT endpoint

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/meta_norostat.html>

## Usage

``` r
pvt_meta_norostat(auth, fetch_args = fetch_args_list())
```

## Arguments

- auth:

  string. Your restricted access key (not the same as API key).

- fetch_args:

  [`fetch_args`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md).
  See
  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md)
  for details.

## Value

[`list`](https://rdrr.io/r/base/list.html)

## See also

[`pvt_norostat()`](https://cmu-delphi.github.io/epidatr/dev/reference/pvt_norostat.md)

## Examples

``` r
if (FALSE) { # \dontrun{
pvt_meta_norostat(auth = Sys.getenv("DELPHI_EPIDATA_KEY"),)
} # }
```
