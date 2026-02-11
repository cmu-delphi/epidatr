# ECDC ILI incidence (Europe)

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/ecdc_ili.html>.

Obtain information on influenza-like-illness from the European Centre
for Disease Prevention and Control.

## Usage

``` r
pub_ecdc_ili(
  regions,
  epiweeks = "*",
  ...,
  issues = NULL,
  lag = NULL,
  fetch_args = fetch_args_list()
)
```

## Arguments

- regions:

  character. List of regions to fetch. See the [codes for European
  countries](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#european-countries).
  \# nolint

- epiweeks:

  [`timeset`](https://cmu-delphi.github.io/epidatr/dev/reference/timeset.md).
  Epiweeks to fetch. Supports
  [`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md)
  and defaults to all ("\*") dates. Format as
  `epirange(startweek, endweek)`, where startweek and endweek are of the
  form YYYYWW (string or numeric).

- ...:

  not used for values, forces later arguments to bind by name

- issues:

  [`timeset`](https://cmu-delphi.github.io/epidatr/dev/reference/timeset.md).
  Optionally, the issue(s) of the data to fetch. See the "Data
  Versioning" section for details.

- lag:

  integer. Optionally, the lag of the issues to fetch. See the "Data
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

The list of location argument can be found in
<https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/ecdc_regions.txt>.

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
pub_ecdc_ili(regions = "austria", epiweeks = epirange(201901, 202001))
#> Waiting 3s for retry backoff ■■■■■■■■■■■                     
#> Waiting 3s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■         
#> Waiting 3s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  
#> # A tibble: 28 × 6
#>    release_date region  issue      epiweek      lag incidence_rate
#>    <date>       <chr>   <date>     <date>     <dbl>          <dbl>
#>  1 2020-03-26   Austria 2020-03-15 2019-01-06    62           787.
#>  2 2020-03-26   Austria 2020-03-15 2019-01-13    61           855.
#>  3 2020-03-26   Austria 2020-03-15 2019-01-20    60          1022.
#>  4 2020-03-26   Austria 2020-03-15 2019-01-27    59          1265.
#>  5 2020-03-26   Austria 2020-03-15 2019-02-03    58          1367.
#>  6 2020-03-26   Austria 2020-03-15 2019-02-10    57          1209.
#>  7 2020-03-26   Austria 2020-03-15 2019-02-17    56          1156.
#>  8 2020-03-26   Austria 2020-03-15 2019-02-24    55          1122.
#>  9 2020-03-26   Austria 2020-03-15 2019-03-03    54           948.
#> 10 2020-03-26   Austria 2020-03-15 2019-03-10    53           703.
#> # ℹ 18 more rows
```
