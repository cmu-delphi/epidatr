# performs the request

You can test the authentication headers like so:

## Usage

``` r
do_request(url, params, timeout_seconds)
```

## Examples

``` r
response <- httr::RETRY(
  "GET", "https://httpbin.org/headers",
  httr::authenticate("epidata", "fake_key")
)
#> Request failed [503]. Retrying in 1 seconds...
#> Request failed [503]. Retrying in 1.9 seconds...
httr::content(response)$headers$Authorization == paste0(
  "Basic ",
  base64enc::base64encode(charToRaw("epidata:fake_key"))
)
#> No encoding supplied: defaulting to UTF-8.
#> logical(0)
```
