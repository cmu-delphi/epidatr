# Validate a numeric cache setting

Coerces `value` (an argument or environment variable string) to a single
positive number, aborting with an actionable message otherwise. A bare
[`as.numeric()`](https://rdrr.io/r/base/numeric.html) turns values like
`"3/24"` into `NA`, which passes
[`is.numeric()`](https://rdrr.io/r/base/numeric.html) and later breaks
every cache read inside cachem.

## Usage

``` r
as_cache_setting(value, arg, env_var)
```
