# Influenza and dengue digital surveillance sensors

API docs: <https://cmu-delphi.github.io/delphi-epidata/api/sensors.html>

This is the documentation of the API for accessing the Digital
Surveillance Sensors endpoint of the Delphi’s epidemiological. Note:
this repository was built to support modeling and forecasting efforts
surrounding seasonal influenza (and dengue). In the current COVID-19
pandemic, syndromic surveillance data, like ILI data (influenza-like
illness) through FluView, will likely prove very useful. However, we
urge caution to users examining the digital surveillance sensors, like
ILI Nearby, Google Flu Trends, etc., during the COVID-19 pandemic,
because these were designed to track ILI as driven by seasonal
influenza, and were NOT designed to track ILI during the COVID-19
pandemic.

## Usage

``` r
pvt_sensors(
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

  character. Sensor names to fetch.

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
pvt_sensors(
  auth = Sys.getenv("SECRET_API_AUTH_SENSORS"),
  names = "sar3",
  locations = "nat",
  epiweeks = epirange(201501, 202001)
)
} # }
```
