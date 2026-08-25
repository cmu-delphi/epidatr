# Helper for finding COVID hospitalization facilities

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/covid_hosp_facility_lookup.html>

Obtains unique identifiers and other metadata for COVID hospitalization
facilities of interest. This is a companion endpoint to the
[`pub_covid_hosp_facility()`](https://cmu-delphi.github.io/epidatr/reference/pub_covid_hosp_facility.md)
endpoint.

## Usage

``` r
pub_covid_hosp_facility_lookup(
  ...,
  state = NULL,
  ccn = NULL,
  city = NULL,
  zip = NULL,
  fips_code = NULL,
  fetch_args = fetch_args_list()
)
```

## Arguments

- ...:

  not used for values, forces later arguments to bind by name

- state:

  string. A two-letter character state abbreviation. See [US states
  codes](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#us-states)
  for details.

- ccn:

  string. A facility CMS certification number.

- city:

  string. A city name.

- zip:

  string. A 5-digit zip code.

- fips_code:

  string. A 5-digit fips county code, zero-padded.

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

Only one location argument needs to be specified. Combinations of the
arguments are not currently supported.

## See also

For example queries showing how to discover signals and build calls, see
[`vignette("signal-discovery", package = "epidatr")`](https://cmu-delphi.github.io/epidatr/articles/signal-discovery.md).

## See also

[`pub_covid_hosp_facility()`](https://cmu-delphi.github.io/epidatr/reference/pub_covid_hosp_facility.md)

## Examples

``` r

pub_covid_hosp_facility_lookup(state = "fl")
#> `pub_covid_hosp_facility_lookup()` covers a data source that is no longer
#> updated.
#> ℹ Historical data remains available, but no new data is being ingested.
#> ℹ See the "Endpoints kept for historical reference" section of
#>   `vignette("migration-guide")` (or
#>   <https://cmu-delphi.github.io/epidatr/articles/migration-guide.html#endpoints-kept-for-historical-reference>)
#>   for details.
#> This message is displayed once per session.
#> # A tibble: 236 × 10
#>    hospital_pk state ccn    hospital_name   address city  zip   hospital_subtype
#>    <chr>       <chr> <chr>  <chr>           <chr>   <chr> <chr> <chr>           
#>  1 100001      FL    100001 UF HEALTH JACK… 655 W … JACK… 32209 Short Term      
#>  2 100002      FL    100002 BETHESDA HOSPI… 2815 S… BOYN… 33435 Short Term      
#>  3 100006      FL    100006 ORLANDO HEALTH… 52 W U… ORLA… 32806 Short Term      
#>  4 100007      FL    100007 ADVENTHEALTH O… 601 E … ORLA… 32803 Short Term      
#>  5 100008      FL    100008 BAPTIST HOSPIT… 8900 N… MIAMI 33176 Short Term      
#>  6 100012      FL    100012 LEE MEMORIAL H… 2776 C… FORT… 33901 Short Term      
#>  7 100014      FL    100014 ADVENTHEALTH N… 401 PA… NEW … 32170 Short Term      
#>  8 100017      FL    100017 HALIFAX HEALTH… 303 N … DAYT… 32114 Short Term      
#>  9 100018      FL    100018 NAPLES COMMUNI… 350 7T… NAPL… 34102 Short Term      
#> 10 100019      FL    100019 HOLMES REGIONA… 1350 S… MELB… 32901 Short Term      
#> # ℹ 226 more rows
#> # ℹ 2 more variables: fips_code <chr>, is_metro_micro <dbl>
pub_covid_hosp_facility_lookup(city = "southlake")
#> # A tibble: 2 × 10
#>   hospital_pk state ccn    hospital_name    address city  zip   hospital_subtype
#>   <chr>       <chr> <chr>  <chr>            <chr>   <chr> <chr> <chr>           
#> 1 450888      TX    450888 TEXAS HEALTH HA… 1545 E… SOUT… 76092 Short Term      
#> 2 670132      TX    670132 METHODIST SOUTH… 421 E … SOUT… 76092 Short Term      
#> # ℹ 2 more variables: fips_code <chr>, is_metro_micro <dbl>
```
