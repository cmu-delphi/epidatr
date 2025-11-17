# Various COVID and flu signals via the COVIDcast endpoint

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html>

The primary endpoint for fetching COVID-19 data, providing access to a
wide variety of signals from a wide variety of sources. See the API
documentation link above for more. Delphi's [COVIDcast public
dashboard](https://delphi.cmu.edu/covidcast/) is powered by this
endpoint.

## Usage

``` r
pub_covidcast(
  source,
  signals,
  geo_type,
  time_type,
  geo_values = "*",
  time_values = "*",
  ...,
  as_of = NULL,
  issues = NULL,
  lag = NULL,
  fetch_args = fetch_args_list()
)
```

## Arguments

- source:

  string. The data source to query (see:
  <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html>).

- signals:

  string. The signals to query from a specific source (see:
  <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html>).

- geo_type:

  string. The geographic resolution of the data (see:
  <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html>).

- time_type:

  string. The temporal resolution of the data (either "day" or "week",
  depending on signal).

- geo_values:

  character. The geographies to return. Defaults to all ("\*")
  geographies within requested geographic resolution (see:
  <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html>.).

- time_values:

  [`timeset`](https://cmu-delphi.github.io/epidatr/reference/timeset.md).
  Dates to fetch. Defaults to all ("\*") dates.

- ...:

  not used for values, forces later arguments to bind by name

- as_of:

  Date. Optionally, the as of date for the issues to fetch. If not
  specified, the most recent data is returned. Mutually exclusive with
  `issues` or `lag`.

- issues:

  [`timeset`](https://cmu-delphi.github.io/epidatr/reference/timeset.md).
  Optionally, the issue of the data to fetch. If not specified, the most
  recent issue is returned. Mutually exclusive with `as_of` or `lag`.

- lag:

  integer. Optionally, the lag of the issues to fetch. If not set, the
  most recent issue is returned. Mutually exclusive with `as_of` or
  `issues`.

- fetch_args:

  [`fetch_args`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/reference/epidata_call.md).

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## See also

[`pub_covidcast_meta()`](https://cmu-delphi.github.io/epidatr/reference/pub_covidcast_meta.md),
[`covidcast_epidata()`](https://cmu-delphi.github.io/epidatr/reference/covidcast_epidata.md),
[`epirange()`](https://cmu-delphi.github.io/epidatr/reference/epirange.md)

## Examples

``` r
if (FALSE) { # \dontrun{
pub_covidcast(
  source = "jhu-csse",
  signals = "confirmed_7dav_incidence_prop",
  geo_type = "state",
  time_type = "day",
  geo_values = c("ca", "fl"),
  time_values = epirange(20200601, 20200801)
)
pub_covidcast(
  source = "jhu-csse",
  signals = "confirmed_7dav_incidence_prop",
  geo_type = "state",
  time_type = "day",
  geo_values = "*",
  time_values = epirange(20200601, 20200801)
)
} # }
```
