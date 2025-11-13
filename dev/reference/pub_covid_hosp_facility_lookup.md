# Helper for finding COVID hospitalization facilities

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/covid_hosp_facility_lookup.html>

Obtains unique identifiers and other metadata for COVID hospitalization
facilities of interest. This is a companion endpoint to the
[`pub_covid_hosp_facility()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covid_hosp_facility.md)
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

  string. A two-letter character state abbreviation.

- ccn:

  string. A facility CMS certification number.

- city:

  string. A city name.

- zip:

  string. A 5-digit zip code.

- fips_code:

  string. A 5-digit fips county code, zero-padded.

- fetch_args:

  [`fetch_args`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md).

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## Details

Only one location argument needs to be specified. Combinations of the
arguments are not currently supported.

## See also

[`pub_covid_hosp_facility()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covid_hosp_facility.md)

## Examples

``` r
if (FALSE) { # \dontrun{
pub_covid_hosp_facility_lookup(state = "fl")
pub_covid_hosp_facility_lookup(city = "southlake")
} # }
```
