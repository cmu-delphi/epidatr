# Quidel COVID-19 and influenza testing data

API docs: <https://cmu-delphi.github.io/delphi-epidata/api/quidel.html>

Data provided by Quidel Corp., which contains flu lab test results.

## Usage

``` r
pvt_quidel(auth, locations, epiweeks = "*", fetch_args = fetch_args_list())
```

## Arguments

- auth:

  string. Restricted access key (not the same as API key).

- locations:

  character. Locations to fetch.

- epiweeks:

  [`timeset`](https://cmu-delphi.github.io/epidatr/dev/reference/timeset.md).
  Epiweeks to fetch. Defaults to all ("\*") dates.

- fetch_args:

  [`fetch_args`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md).

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## Examples

``` r
if (FALSE) { # \dontrun{
pvt_quidel(
  auth = Sys.getenv("SECRET_API_AUTH_QUIDEL"),
  epiweeks = epirange(201201, 202001),
  locations = "hhs1"
)
} # }
```
