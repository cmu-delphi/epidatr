# CDC FluSurv flu hospitalizations

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/flusurv.html>.

Obtain information on influenza hospitalization rates from the Center of
Disease Control.

See also <https://gis.cdc.gov/GRASP/Fluview/FluHospRates.html>.

## Usage

``` r
pub_flusurv(
  locations,
  epiweeks = "*",
  ...,
  issues = NULL,
  lag = NULL,
  fetch_args = fetch_args_list()
)
```

## Arguments

- locations:

  character. Character vector indicating location.

- epiweeks:

  [`timeset`](https://cmu-delphi.github.io/epidatr/reference/timeset.md).
  Epiweeks to fetch. Defaults to all ("\*") dates.

- ...:

  not used for values, forces later arguments to bind by name

- issues:

  [`timeset`](https://cmu-delphi.github.io/epidatr/reference/timeset.md).
  Optionally, the issues to fetch. If not set, the most recent issue is
  returned. Mutually exclusive with `lag`.

- lag:

  integer. Optionally, the lag of the issues to fetch. If not set, the
  most recent issue is returned. Mutually exclusive with `issues`.

- fetch_args:

  [`fetch_args`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/reference/epidata_call.md).

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## Details

The list of location argument can be found in
<https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/flusurv_locations.txt>.

## Examples

``` r
if (FALSE) { # \dontrun{
pub_flusurv(locations = "CA", epiweeks = epirange(201701, 201801))
} # }
```
