# Specify a range of days or weeks for API requests

Specify a date range (in days or epiweeks) for an API request.

## Usage

``` r
epirange(from, to)
```

## Arguments

- from:

  The first date to request. Can be specified as a `Date` or as an
  integer or integer-like string in the format YYYYMMDD for dates or
  YYYYWW for epiweeks.

- to:

  The final date to request (inclusive), specified the same way as
  `from`.

## Value

An `EpiRange` object.

## Details

Epiweeks, also known as MMWR weeks number the weeks of the year from 1
to 53, each week spanning from Sunday to Saturday. The numbering is
[defined by the
CDC](https://ndc.services.cdc.gov/wp-content/uploads/MMWR_Week_overview.pdf).

## Examples

``` r
# Represents 2021-01-01 to 2021-01-07, inclusive
epirange(20210101, 20210107)
#> 
#> ── <EpiRange> object: ──────────────────────────────────────────────────────────
#> Days from 2021-01-01 to 2021-01-07

# The same, but using Date objects
epirange(as.Date("2021-01-01"), as.Date("2021-01-07"))
#> 
#> ── <EpiRange> object: ──────────────────────────────────────────────────────────
#> Days from 2021-01-01 to 2021-01-07

# Represents epiweeks 2 through 4 of 2022, inclusive
epirange(202202, 202204)
#> 
#> ── <EpiRange> object: ──────────────────────────────────────────────────────────
#> Epiweeks from 2022w02 to 2022w04
```
