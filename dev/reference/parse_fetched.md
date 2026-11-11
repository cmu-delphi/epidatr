# Turn an API response into the tibble `fetch()` returns

Runs on both fresh and cached responses, so parse settings never need to
be part of the cache key.

## Usage

``` r
parse_fetched(epidata_call, response_content, fetch_args)
```
