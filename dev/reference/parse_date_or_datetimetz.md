# Parses a cast-API report time, which is either a UTC timestamp or a bare date

Timestamps go through
[`parse_api_datetimetz()`](https://cmu-delphi.github.io/epidatr/dev/reference/parse_api_datetimetz.md)
to a UTC `POSIXct`. Anything else (e.g. "2025-10-16" or "20251016") goes
through `parse_api_date()` to a `Date`. The first non-missing value
decides the parser for the whole vector.

## Usage

``` r
parse_date_or_datetimetz(value)
```
