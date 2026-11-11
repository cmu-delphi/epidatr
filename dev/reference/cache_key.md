# Cache key for an epidata call

The cache stores the unparsed response, so the key is the request URL
(which includes `fields`). The epidatr version and a format tag are a
prefix, so entries from another version, or in an older stored format,
are never read.

## Usage

``` r
cache_key(epidata_call, fetch_args)
```
