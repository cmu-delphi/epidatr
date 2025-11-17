# PAHO dengue digital surveillance sensors (North and South America)

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/dengue_sensors.html>

## Usage

``` r
pvt_dengue_sensors(
  auth,
  names,
  locations,
  epiweeks = "*",
  fetch_args = fetch_args_list()
)
```

## Arguments

- auth:

  string. Restricted access key (not the same as API key).

- names:

  character. Names to fetch.

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

## Examples

``` r
if (FALSE) { # \dontrun{
pvt_dengue_sensors(
  auth = Sys.getenv("SECRET_API_AUTH_SENSORS"),
  names = "ght",
  locations = "ag",
  epiweeks = epirange(201501, 202001)
)
} # }
```
