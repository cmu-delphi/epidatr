# Delphi's ILINet outpatient doctor visits forecasts

API docs: <https://cmu-delphi.github.io/delphi-epidata/api/delphi.html>

## Usage

``` r
pub_delphi(system, epiweek, fetch_args = fetch_args_list())
```

## Arguments

- system:

  character. System name to fetch.

- epiweek:

  [`timeset`](https://cmu-delphi.github.io/epidatr/dev/reference/timeset.md).
  Epiweek to fetch. Does not support multiple dates. Make separate calls
  to fetch data for multiple epiweeks.

- fetch_args:

  [`fetch_args`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md).
  See
  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md)
  for details.

## Value

[`list`](https://rdrr.io/r/base/list.html)

## Examples

``` r
if (FALSE) { # \dontrun{
pub_delphi(system = "ec", epiweek = 201501)
} # }
```
