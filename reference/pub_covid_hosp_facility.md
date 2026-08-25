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

  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/reference/epidata_call.md).
  See
  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md)
  for details.

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## Details

Starting October 1, 2022, some facilities are only required to report
annually.

## See also

For example queries showing how to discover signals and build calls, see
[`vignette("signal-discovery", package = "epidatr")`](https://cmu-delphi.github.io/epidatr/articles/signal-discovery.md).

## See also

`pub_covid_hosp_facility()`,
[`epirange()`](https://cmu-delphi.github.io/epidatr/reference/epirange.md)

## Examples

``` r

pub_covid_hosp_facility(
  hospital_pks = "100075",
  collection_weeks = epirange(20200101, 20200501)
)
#> `pub_covid_hosp_facility()` covers a data source that is no longer updated.
#> ℹ Historical data remains available, but no new data is being ingested.
#> ℹ See the "Endpoints kept for historical reference" section of
#>   `vignette("migration-guide")` (or
#>   <https://cmu-delphi.github.io/epidatr/articles/migration-guide.html#endpoints-kept-for-historical-reference>)
#>   for details.
#> This message is displayed once per session.
#> # A tibble: 12 × 113
#>    hospital_pk state ccn    hospital_name   address city  zip   hospital_subtype
#>    <chr>       <chr> <chr>  <chr>           <chr>   <chr> <chr> <chr>           
#>  1 100075      FL    100075 ST JOSEPHS HOS… 3001 W… TAMPA 33677 Short Term      
#>  2 100075      FL    100075 ST JOSEPHS HOS… 3001 W… TAMPA 33677 Short Term      
#>  3 100075      FL    100075 ST JOSEPHS HOS… 3001 W… TAMPA 33677 Short Term      
#>  4 100075      FL    100075 ST JOSEPHS HOS… 3001 W… TAMPA 33677 Short Term      
#>  5 100075      FL    100075 ST JOSEPHS HOS… 3001 W… TAMPA 33677 Short Term      
#>  6 100075      FL    100075 ST JOSEPHS HOS… 3001 W… TAMPA 33677 Short Term      
#>  7 100075      FL    100075 ST JOSEPHS HOS… 3001 W… TAMPA 33677 Short Term      
#>  8 100075      FL    100075 ST JOSEPHS HOS… 3001 W… TAMPA 33677 Short Term      
#>  9 100075      FL    100075 ST JOSEPHS HOS… 3001 W… TAMPA 33677 Short Term      
#> 10 100075      FL    100075 ST JOSEPHS HOS… 3001 W… TAMPA 33677 Short Term      
#> 11 100075      FL    100075 ST JOSEPHS HOS… 3001 W… TAMPA 33677 Short Term      
#> 12 100075      FL    100075 ST JOSEPHS HOS… 3001 W… TAMPA 33677 Short Term      
#> # ℹ 105 more variables: fips_code <chr>, geocoded_hospital_address <chr>,
#> #   hhs_ids <chr>, publication_date <date>, collection_week <date>,
#> #   is_metro_micro <lgl>, total_beds_7_day_sum <dbl>,
#> #   all_adult_hospital_beds_7_day_sum <dbl>,
#> #   all_adult_hospital_inpatient_beds_7_day_sum <dbl>,
#> #   inpatient_beds_used_7_day_sum <dbl>,
#> #   all_adult_hospital_inpatient_bed_occupied_7_day_sum <dbl>, …

pub_covid_hosp_facility(
  hospital_pks = "050063",
  collection_weeks = epirange(20240101, 20240301)
)
#> # A tibble: 7 × 113
#>   hospital_pk state ccn    hospital_name    address city  zip   hospital_subtype
#>   <chr>       <chr> <chr>  <chr>            <chr>   <chr> <chr> <chr>           
#> 1 050063      CA    050063 HOLLYWOOD PRESB… 1300 N… LOS … 90027 Short Term      
#> 2 050063      CA    050063 HOLLYWOOD PRESB… 1300 N… LOS … 90027 Short Term      
#> 3 050063      CA    050063 HOLLYWOOD PRESB… 1300 N… LOS … 90027 Short Term      
#> 4 050063      CA    050063 HOLLYWOOD PRESB… 1300 N… LOS … 90027 Short Term      
#> 5 050063      CA    050063 HOLLYWOOD PRESB… 1300 N… LOS … 90027 Short Term      
#> 6 050063      CA    050063 HOLLYWOOD PRESB… 1300 N… LOS … 90027 Short Term      
#> 7 050063      CA    050063 HOLLYWOOD PRESB… 1300 N… LOS … 90027 Short Term      
#> # ℹ 105 more variables: fips_code <chr>, geocoded_hospital_address <chr>,
#> #   hhs_ids <chr>, publication_date <date>, collection_week <date>,
#> #   is_metro_micro <lgl>, total_beds_7_day_sum <dbl>,
#> #   all_adult_hospital_beds_7_day_sum <dbl>,
#> #   all_adult_hospital_inpatient_beds_7_day_sum <dbl>,
#> #   inpatient_beds_used_7_day_sum <dbl>,
#> #   all_adult_hospital_inpatient_bed_occupied_7_day_sum <dbl>, …
```
