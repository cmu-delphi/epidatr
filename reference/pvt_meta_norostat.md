# Metadata for the NoroSTAT endpoint

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/meta_norostat.html>

## Usage

``` r
pvt_meta_norostat(auth, fetch_args = fetch_args_list())
```

## Arguments

- auth:

  string. Restricted access key (not the same as API key).

- fetch_args:

  [`fetch_args`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/reference/epidata_call.md).

## Value

[`list`](https://rdrr.io/r/base/list.html)

## See also

[`pvt_norostat()`](https://cmu-delphi.github.io/epidatr/reference/pvt_norostat.md)

## Examples

``` r
if (FALSE) { # \dontrun{
pvt_meta_norostat(auth = Sys.getenv("SECRET_API_AUTH_NOROSTAT"))
} # }
```
