# Cache key for an epidata call

The cache stores parsed tibbles, so the key must cover every fetch
setting that changes parsing, not just the URL. `reference_week_day`
changes how epiweeks become dates; it is appended only when it differs
from the default so existing cache entries stay valid (#388).

## Usage

``` r
cache_key(epidata_call, fetch_args)
```
