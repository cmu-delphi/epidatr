# Quidel COVID-19 and influenza testing data

API docs: <https://cmu-delphi.github.io/delphi-epidata/api/quidel.html>

Data provided by Quidel Corp., which contains flu lab test results.

## Usage

``` r
pvt_quidel(auth, locations, epiweeks = "*", fetch_args = fetch_args_list())
```

## Arguments

- auth:

  string. Your restricted access key (not the same as API key).

- locations:

  character. List of locations to fetch. See [HHS regions'
  codes](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#hhs-regions)
  for details.

- epiweeks:

  [`timeset`](https://cmu-delphi.github.io/epidatr/dev/reference/timeset.md).
  Epiweeks to fetch. Supports
  [`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md)
  and defaults to all ("\*") dates. Format as
  `epirange(startweek, endweek)`, where startweek and endweek are of the
  form YYYYWW (string or numeric).

- fetch_args:

  [`fetch_args`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md).
  See
  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md)
  for details.

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## Examples

``` r
if (FALSE) { # curl::has_internet() && Sys.getenv("SECRET_API_AUTH_QUIDEL") != ""

pvt_quidel(
  auth = Sys.getenv("SECRET_API_AUTH_QUIDEL"),
  epiweeks = epirange(201201, 202001),
  locations = "hhs1"
)
}
```
