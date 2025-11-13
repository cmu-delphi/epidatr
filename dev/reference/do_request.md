# performs the request

You can test the authentication headers like so:

## Usage

``` r
do_request(url, params, timeout_seconds)
```

## Examples

``` r
if (FALSE) { # \dontrun{
response <- httr::RETRY(
  "GET", "https://httpbin.org/headers",
  httr::authenticate("epidata", "fake_key")
)
content(response)$headers$Authorization == paste0(
  "Basic ",
  base64enc::base64encode(charToRaw("epidata:fake_key"))
)
} # }
```
