# Performs an API request and returns the response body as a string.

Handles authentication, retries, 414 URI Too Long fallback to POST, HTTP
errors, and API-level errors. The API returns errors as JSON regardless
of the requested format, so for non-classic formats we sniff the body
for a JSON error response.

## Usage

``` r
do_request(
  epidata_call,
  format_type,
  timeout_seconds,
  fields,
  http_method = c("GET", "POST")
)
```

## Arguments

- epidata_call:

  an instance of `epidata_call`

- format_type:

  format to request, one of "json", "csv", "classic"

- timeout_seconds:

  the maximum time to wait for a response

- fields:

  fields to include in the response, or NULL for all

- http_method:

  HTTP method to use

## Value

the response body as a string
