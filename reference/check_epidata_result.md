# Check an API response for epidata-level errors and warnings.

Check an API response for epidata-level errors and warnings.

## Usage

``` r
check_epidata_result(response_content, allow_empty = FALSE)
```

## Arguments

- response_content:

  parsed JSON response with `result` and `message` fields

- allow_empty:

  if TRUE, suppress errors for "no results" (result == -2)
