# Latest date mentioned in versioning query parameters

Extracts every date in `values` (`YYYY-MM-DD`, `YYYYMMDD`, or `YYYYWW`
epiweeks, alone or inside ranges like `20220101-20220105`) and returns
the latest as a `Date`, or `NULL` if there are none.

## Usage

``` r
latest_version_date(values)
```
