# Format a cast-API time filter (`reference_times` or `report_time_query`).

Accepts `"*"` or `NULL` (no filter), an
[`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md),
plain dates or epiweeks, and filter expressions (`">=2024-01-01"`,
`"2024-01-01:2024-03-31"`), which pass through for the API to validate.

## Usage

``` r
format_time_filter(value, name, bare_dates = TRUE)
```

## Arguments

- name:

  argument name for error messages.

- bare_dates:

  whether plain dates are allowed. `report_time` disallows them.
