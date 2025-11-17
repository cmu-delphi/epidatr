# ECDC ILI incidence (Europe)

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/ecdc_ili.html>.

Obtain information on influenza-like-illness from the European Centre
for Disease Prevention and Control.

## Usage

``` r
pub_ecdc_ili(
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
<https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/ecdc_regions.txt>.

## Examples

``` r
if (FALSE) { # \dontrun{
pub_ecdc_ili(regions = "austria", epiweeks = epirange(201901, 202001))
} # }
```
