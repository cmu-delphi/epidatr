# Latest date mentioned in versioning query parameters

Extracts every date in `values` (`YYYY-MM-DD`, `YYYYMMDD`, or `YYYYWW`
epiweeks, alone, inside ranges like `20220101-20220105`, or at the start
of timestamps like `2025-10-16T13:45:00Z`) and returns the latest as a
`Date`, or `NULL` if there are none.

## Usage

``` r
latest_version_date(values)
```
