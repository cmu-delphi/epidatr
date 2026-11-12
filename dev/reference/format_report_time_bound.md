# Format a report-time-family value the way the cast-API accepts it.

Aborts on a value that can't be formatted (such as a timestamp without a
`Z`, or `"latest"`), rather than sending `NA` to the API.

## Usage

``` r
format_report_time_bound(value, name = "snapshot_date")
```

## Arguments

- name:

  argument name for error messages.
