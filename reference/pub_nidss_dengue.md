# NIDSS dengue cases (Taiwan)

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/nidss_dengue.html>

Obtains counts of confirmed dengue cases in Taiwan from Taiwan National
Infectious Disease Statistical System.

## Usage

``` r
pub_nidss_dengue(locations, epiweeks = "*", fetch_args = fetch_args_list())
```

## Arguments

- locations:

  character. Locations to fetch.

- epiweeks:

  [`timeset`](https://cmu-delphi.github.io/epidatr/reference/timeset.md).
  Epiweeks to fetch. Defaults to all ("\*") dates.

- fetch_args:

  [`fetch_args`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/reference/epidata_call.md).

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## Details

Possible location inputs can be found in
<https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/nidss_regions.txt>
and
<https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/nidss_locations.txt>.

## Examples

``` r
if (FALSE) { # \dontrun{
pub_nidss_dengue(locations = "taipei", epiweeks = epirange(201201, 201301))
} # }
```
