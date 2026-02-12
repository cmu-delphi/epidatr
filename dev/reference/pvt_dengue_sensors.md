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

  string. Your restricted access key (not the same as API key).

- names:

  character. List of sensor names to fetch. See the [available
  sensors](https://cmu-delphi.github.io/delphi-epidata/api/dengue_sensors.html#available-sensors).

- locations:

  character. List of locations to fetch. See the [codes for countries
  and territories in the
  Americas](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#countries-and-territories-in-the-americas).
  \# nolint

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
if (FALSE) { # \dontrun{
pvt_dengue_sensors(
  auth = Sys.getenv("DELPHI_EPIDATA_KEY"),
  names = "ght",
  locations = "ag",
  epiweeks = epirange(201501, 202001)
)
} # }
```
