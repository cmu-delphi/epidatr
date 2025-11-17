# COVID hospitalizations by facility

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/covid_hosp_facility.html>

Obtains the COVID-19 reported patient impact and hospital capacity data
by facility. This dataset is provided by the US Department of Health &
Human Services. The companion function
[`pub_covid_hosp_facility_lookup()`](https://cmu-delphi.github.io/epidatr/reference/pub_covid_hosp_facility_lookup.md)
can be used to look up facility identifiers in a variety of ways.

## Usage

``` r
pub_covid_hosp_facility(
  hospital_pks,
  collection_weeks = "*",
  ...,
  publication_dates = NULL,
  fetch_args = fetch_args_list()
)
```

## Arguments

- hospital_pks:

  character. Facility identifiers.

- collection_weeks:

  [`timeset`](https://cmu-delphi.github.io/epidatr/reference/timeset.md).
  Dates (corresponding to epiweeks) to fetch. Defaults to all ("\*")
  dates.

- ...:

  not used for values, forces later arguments to bind by name

- publication_dates:

  [`timeset`](https://cmu-delphi.github.io/epidatr/reference/timeset.md).
  Publication dates to fetch.

- fetch_args:

  [`fetch_args`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/reference/epidata_call.md).

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## Details

Starting October 1, 2022, some facilities are only required to report
annually.

## See also

`pub_covid_hosp_facility()`,
[`epirange()`](https://cmu-delphi.github.io/epidatr/reference/epirange.md)

## Examples

``` r
if (FALSE) { # \dontrun{
pub_covid_hosp_facility(
  hospital_pks = "100075",
  collection_weeks = epirange(20200101, 20200501)
)

pub_covid_hosp_facility(
  hospital_pks = "050063",
  collection_weeks = epirange(20240101, 20240301)
)
} # }
```
