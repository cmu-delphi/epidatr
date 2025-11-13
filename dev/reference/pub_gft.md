# Google Flu Trends flu search volume

API docs: <https://cmu-delphi.github.io/delphi-epidata/api/gft.html>

Obtains estimates of inluenza activity based on volume of certain search
queries from Google.

## Usage

``` r
pub_gft(locations, epiweeks = "*", fetch_args = fetch_args_list())
```

## Arguments

- locations:

  character. Locations to fetch.

- epiweeks:

  [`timeset`](https://cmu-delphi.github.io/epidatr/dev/reference/timeset.md)
  Epiweeks to fetch. Defaults to all ("\*") dates.

- fetch_args:

  [`fetch_args`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md).

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## Details

Google has discontinued Flu Trends and this is now a static endpoint.
Possibile input for locations can be found in
<https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/regions.txt>,
<https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/states.txt>,
and
<https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/cities.txt>.

## Examples

``` r
if (FALSE) { # \dontrun{
pub_gft(locations = "hhs1", epiweeks = epirange(201201, 202001))
} # }
```
