# Fetches the data.

Raises on errors from the API. Returns JSON.

## Usage

``` r
request_epidata(epidata_call, fetch_args = fetch_args_list(), simplify = TRUE)
```

## Arguments

- epidata_call:

  an instance of `epidata_call`

- fetch_args:

  a `fetch_args` object

## Value

- For `request_epidata`: a JSON-like list
