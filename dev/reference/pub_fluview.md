# CDC FluView ILINet outpatient doctor visits

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/fluview.html>. For

Obtains information on outpatient inluenza-like-illness (ILI) from U.S.
Outpatient Influenza-like Illness Surveillance Network (ILINet).

more information on ILINet, see
<https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html>.

## Usage

``` r
pub_fluview(
  regions,
  epiweeks = "*",
  ...,
  issues = NULL,
  lag = NULL,
  auth = NULL,
  fetch_args = fetch_args_list()
)
```

## Arguments

- regions:

  character. Locations to fetch. Can be any string IDs in national, HHS
  region, census division, most states and territories, and so on. Full
  list link below.

- epiweeks:

  [`timeset`](https://cmu-delphi.github.io/epidatr/dev/reference/timeset.md).
  Epiweeks to fetch in the form `epirange(startweek, endweek)`, where
  startweek and endweek are of the form YYYYWW (string or numeric).
  Defaults to all ("\*") dates.

- ...:

  not used for values, forces later arguments to bind by name

- issues:

  [`timeset`](https://cmu-delphi.github.io/epidatr/dev/reference/timeset.md).
  Optionally, the issues to fetch. If not set, the most recent issue is
  returned. Mutually exclusive with `lag`.

- lag:

  integer. Optionally, the lag of the issues to fetch. If not set, the
  most recent issue is returned. Mutually exclusive with `issues`.

- auth:

  string. Optionally, restricted access key (not the same as API key).

- fetch_args:

  [`fetch_args`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md).

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## Details

The full list of location inputs can be accessed at
<https://github.com/cmu-delphi/delphi-epidata/blob/main/src/acquisition/fluview/fluview_locations.py>.

## Examples

``` r
if (FALSE) { # \dontrun{
pub_fluview(regions = "nat", epiweeks = epirange(201201, 202005))
} # }
```
