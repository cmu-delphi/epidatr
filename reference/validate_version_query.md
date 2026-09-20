# Helper to format the 'version' argument for the CAST API version_query.

Helper to format the 'version' argument for the CAST API version_query.

## Usage

``` r
validate_version_query(version)
```

## Arguments

- version:

  A comparison string (e.g. `"<2025-10-16"`, `">=2025-10-16"`, or
  `"<=2025-10-16T13:45:00Z"` for a UTC timestamp bound) or an
  [`epirange()`](https://cmu-delphi.github.io/epidatr/reference/epirange.md)
  (dates only).

## Value

A formatted `report_time_query` string: a comparison like
`"<2025-10-16"` or `"<=2025-10-16T13:45:00Z"`, or an inclusive range
like `"2024-01-01:2024-03-31"`.
