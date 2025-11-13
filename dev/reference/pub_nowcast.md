# Delphi's ILI Nearby nowcasts

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/nowcast.html>.

Obtains information on outpatient inluenza-like-illness (ILI) from
Delphi's

## Usage

``` r
pub_nowcast(locations, epiweeks = "*", fetch_args = fetch_args_list())
```

## Arguments

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

## Details

The full list of location inputs can be accessed at
<https://github.com/cmu-delphi/delphi-epidata/blob/main/src/acquisition/fluview/fluview_locations.py>.

## Examples

``` r
if (FALSE) { # \dontrun{
pub_nowcast(locations = "ca", epiweeks = epirange(201201, 201301))
} # }
```
