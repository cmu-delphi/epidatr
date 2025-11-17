# Returns the full request url for the given epidata_call

Returns the full request url for the given epidata_call

## Usage

``` r
request_url(epidata_call, format_type = "classic", fields = NULL)
```

## Arguments

- epidata_call:

  an instance of `epidata_call`

- format_type:

  format to return one of classic,json,csv

- fields:

  a list of epidata fields to return, or NULL to return all fields
  (default) e.g. c("time_value", "value") to return only the time_value
  and value fields or c("-direction") to return everything except the
  direction field

## Value

- For `request_url`: string containing the URL
