# Diagnose an empty (or partially empty) cast-API result.

On a partial result (some rows returned), warns about the
signals/geo_types that returned nothing, noting any that
[`epidata_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_meta.md)
says don't exist. On a fully empty result, errors on an invalid
`geo_type`/`signals` and warns generically otherwise. No-op when
`fetch_args$return_empty` is `TRUE`.

## Usage

``` r
.check_cast_empty(fetched, source, signals, geo_type, fetch_args)
```

## Arguments

- fetched:

  the combined server response (a data frame)

- source, signals, geo_type:

  the query parameters, for error/warning messages and for looking up
  [`epidata_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_meta.md)

- fetch_args:

  a `fetch_args` object
