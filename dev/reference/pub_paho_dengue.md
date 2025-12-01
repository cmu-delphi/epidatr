# PAHO dengue data (North and South America)

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/paho_dengue.html>

## Usage

``` r
pub_paho_dengue(
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

  character. List of regions to fetch.

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
pub_paho_dengue(regions = "ca", epiweeks = epirange(201401, 201501))
#> # A tibble: 54 × 11
#>    release_date region serotype issue      epiweek      lag total_pop num_dengue
#>    <date>       <chr>  <chr>    <date>     <date>     <dbl>     <dbl>      <dbl>
#>  1 2020-08-07   CA     "  "     2020-08-02 2013-12-29   344         0          0
#>  2 2020-08-07   CA     "  "     2020-08-02 2014-01-05   343         0          0
#>  3 2020-08-07   CA     "  "     2020-08-02 2014-01-12   342         0          0
#>  4 2020-08-07   CA     "  "     2020-08-02 2014-01-19   341         0          0
#>  5 2020-08-07   CA     "  "     2020-08-02 2014-01-26   340         0          0
#>  6 2020-08-07   CA     "  "     2020-08-02 2014-02-02   339         0          0
#>  7 2020-08-07   CA     "  "     2020-08-02 2014-02-09   338         0          0
#>  8 2020-08-07   CA     "  "     2020-08-02 2014-02-16   337         0          0
#>  9 2020-08-07   CA     "  "     2020-08-02 2014-02-23   336         0          0
#> 10 2020-08-07   CA     "  "     2020-08-02 2014-03-02   335         0          0
#> # ℹ 44 more rows
#> # ℹ 3 more variables: num_severe <dbl>, num_deaths <dbl>, incidence_rate <dbl>
```
