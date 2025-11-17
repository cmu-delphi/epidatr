# COVID hospitalizations by state

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/covid_hosp.html>.

Obtains the COVID-19 reported patient impact and hospital capacity data
by state. This dataset is provided by the US Department of Health &
Human Services.

## Usage

``` r
pub_covid_hosp_state_timeseries(
  states,
  dates = "*",
  ...,
  as_of = NULL,
  issues = NULL,
  fetch_args = fetch_args_list()
)
```

## Arguments

- states:

  character. Two letter state abbreviations.

- dates:

  [`timeset`](https://cmu-delphi.github.io/epidatr/reference/timeset.md).
  Dates to fetch. Defaults to all ("\*") dates.

- ...:

  not used for values, forces later arguments to bind by name

- as_of:

  Date. Optionally, the as of date for the issues to fetch. If not
  specified, the most recent data is returned. Mutually exclusive with
  `issues`.

- issues:

  [`timeset`](https://cmu-delphi.github.io/epidatr/reference/timeset.md).
  Optionally, the issue of the data to fetch. If not specified, the most
  recent issue is returned. Mutually exclusive with `as_of` or `lag`.

- fetch_args:

  [`fetch_args`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/reference/epidata_call.md).

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## Details

Starting October 1, 2022, some facilities are only required to report
annually.

## Examples

``` r
if (FALSE) { # \dontrun{
pub_covid_hosp_state_timeseries(
  states = "fl",
  dates = epirange(20200101, 20200501)
)
} # }
```
