# Serialize named key filters into the cast-API `key:value` term string.

The backend takes multiple filters per key as repeated `key:value` terms
in a single query param, so
`list(pcr_target = c("a", "b"), geo_value = "ca")` becomes
`"pcr_target:a,pcr_target:b,geo_value:ca"`. Returns `NULL` for no
filters.

## Usage

``` r
.serialize_key_filters(key_filters, max_vals = 10L)
```
