# Helper to format the 'version' argument for the CAST API version_query.

Helper to format the 'version' argument for the CAST API version_query.

## Usage

``` r
validate_version_query(version)
```

## Arguments

- version:

  A comparison string (e.g. `"<2025-10-16"`, `">=2025-10-16"`) or an
  [`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md).

## Value

A formatted `report_time_query` string: a comparison like
`"<2025-10-16"`, or an inclusive range like `"2024-01-01:2024-03-31"`.
