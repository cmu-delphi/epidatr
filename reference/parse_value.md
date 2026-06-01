# Parse data returned by the API

This function uses the metadata annotations to determine how to parse
the incoming data. The data annotation refers to the received API type,
rather than the target R type.

## Usage

``` r
parse_value(info, value, disable_date_parsing = FALSE, reference_week_day = 1)
```

## Arguments

- info:

  The `EpidataFieldInfo` object for the field.

- value:

  The value to parse.

- disable_date_parsing:

  If TRUE, disable automatic date parsing.

- reference_week_day:

  The reference day for epiweek conversion (default 1).

## Value

The parsed value.
