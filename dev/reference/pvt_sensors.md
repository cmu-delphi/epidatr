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

  string. Your restricted access key (not the same as API key).

- names:

  character. List of sensor names to fetch. See the [data sources
  available](https://cmu-delphi.github.io/delphi-epidata/api/sensors.html#data-sources)
  for details.

- locations:

  character. List of locations to fetch. See the codes of the [US
  regions and
  states](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#us-regions-and-states)
  \# nolint for details.

- epiweeks:

  [`timeset`](https://cmu-delphi.github.io/epidatr/dev/reference/timeset.md).
  Epiweeks to fetch. Supports
  [`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md)
  and defaults to all ("\*") dates. Format as
  `epirange(startweek, endweek)`, where startweek and endweek are of the
  form YYYYWW (string or numeric).

- fetch_args:

  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md).
  See
  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md)
  for details.

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## See also

For example queries showing how to discover signals and build calls, see
[`vignette("signal-discovery", package = "epidatr")`](https://cmu-delphi.github.io/epidatr/dev/articles/signal-discovery.md).

## Examples

``` r
if (FALSE) { # \dontrun{
pvt_sensors(
  auth = Sys.getenv("DELPHI_EPIDATA_KEY"),
  names = "sar3",
  locations = "nat",
  epiweeks = epirange(201501, 202001)
)
} # }
```
