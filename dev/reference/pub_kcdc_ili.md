# KCDC ILI incidence (Korea)

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/kcdc_ili.html>

## Usage

``` r
pub_kcdc_ili(
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

  character. List of regions to fetch. See [South Korea's geographic
  codes](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#republic-of-korea)
  \# nolint for details.

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
pub_kcdc_ili(regions = "ROK", epiweeks = 200436)
#> Waiting 3s for retry backoff ■■■■■■■■■■■■■                   
#> Waiting 3s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 
#> # A tibble: 1 × 6
#>   release_date region issue      epiweek      lag   ili
#>   <date>       <chr>  <date>     <date>     <dbl> <dbl>
#> 1 2020-11-03   ROK    2020-11-01 2004-09-05   843   0.6
```
