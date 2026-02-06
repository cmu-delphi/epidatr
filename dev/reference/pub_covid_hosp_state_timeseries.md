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

  character. Two-letter state abbreviations. See [US states
  codes](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#us-states)
  for details.

- dates:

  [`timeset`](https://cmu-delphi.github.io/epidatr/dev/reference/timeset.md).
  Dates to fetch. Supports
  [`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md)
  and defaults to all ("\*") dates.

- ...:

  not used for values, forces later arguments to bind by name

- as_of:

  Date. Optionally, the as-of date for the issues to fetch. See the
  "Data Versioning" section for details.

- issues:

  [`timeset`](https://cmu-delphi.github.io/epidatr/dev/reference/timeset.md).
  Optionally, the issue(s) of the data to fetch. See the "Data
  Versioning" section for details.

- fetch_args:

  [`fetch_args`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md).
  See
  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md)
  for details.

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## Details

Starting October 1, 2022, some facilities are only required to report
annually.

## Data Versioning

Several endpoints support retrieving historical versions of the data.
The following parameters control this and are mutually exclusive (only
one can be provided at a time).

- `as_of`: (Date) Retrieve the data as it was on this date.

- `issues`:
  [`timeset`](https://cmu-delphi.github.io/epidatr/dev/reference/timeset.md)
  Retrieve data from a specific issue date or range of dates.

- `lag`: (integer) Retrieve data with a specific lag from its issue
  date.

If none of these is specified, the most recent version of the data is
returned.

See
[`vignette("versioned-data")`](https://cmu-delphi.github.io/epidatr/dev/articles/versioned-data.md)
for details and more ways to specify versioned data.

## Examples

``` r
pub_covid_hosp_state_timeseries(
  states = "fl",
  dates = epirange(20200101, 20200501)
)
#> # A tibble: 51 × 118
#>    state geocoded_state issue      date       critical_staffing_shortage_today…¹
#>    <chr> <lgl>          <date>     <date>     <lgl>                             
#>  1 FL    NA             2024-05-03 2020-03-12 FALSE                             
#>  2 FL    NA             2024-05-03 2020-03-13 FALSE                             
#>  3 FL    NA             2024-05-03 2020-03-14 FALSE                             
#>  4 FL    NA             2024-05-03 2020-03-15 FALSE                             
#>  5 FL    NA             2024-05-03 2020-03-16 FALSE                             
#>  6 FL    NA             2024-05-03 2020-03-17 FALSE                             
#>  7 FL    NA             2024-05-03 2020-03-18 FALSE                             
#>  8 FL    NA             2024-05-03 2020-03-19 FALSE                             
#>  9 FL    NA             2024-05-03 2020-03-20 FALSE                             
#> 10 FL    NA             2024-05-03 2020-03-21 FALSE                             
#> # ℹ 41 more rows
#> # ℹ abbreviated name: ¹​critical_staffing_shortage_today_yes
#> # ℹ 113 more variables: critical_staffing_shortage_today_no <lgl>,
#> #   critical_staffing_shortage_today_not_reported <lgl>,
#> #   critical_staffing_shortage_anticipated_within_week_yes <lgl>,
#> #   critical_staffing_shortage_anticipated_within_week_no <lgl>,
#> #   critical_staffing_shortage_anticipated_within_week_not_reported <lgl>, …
```
