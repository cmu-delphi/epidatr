# CDC FluView flu tests from clinical labs

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/fluview_clinical.html>

## Usage

``` r
pub_fluview_clinical(
  regions,
  epiweeks = "*",
  ...,
  issues = NULL,
  lag = NULL,
  fetch_args = fetch_args_list()
)
```

## Arguments

- regions:

  character. Regions to fetch.

- epiweeks:

  [`timeset`](https://cmu-delphi.github.io/epidatr/reference/timeset.md).
  Epiweeks to fetch in the form epirange(startweek,endweek), where
  startweek and endweek are of the form YYYYWW (string or numeric).
  Defaults to all ("\*") dates.

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

## Examples

``` r
if (FALSE) { # \dontrun{
pub_fluview_clinical(regions = "nat", epiweeks = epirange(201601, 201701))
} # }
```
