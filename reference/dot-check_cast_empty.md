# Diagnose an empty (or partially empty) cast-API result.

On a partial result (some rows returned), warns about the
signals/geo_types that returned nothing, noting any that
[`epidata_meta()`](https://cmu-delphi.github.io/epidatr/reference/epidata_meta.md)
says don't exist. On a fully empty result, errors on an invalid
`geo_type`/`signals`, warns when the local `geo_values`/`reference_time`
filters dropped every row the server returned, and warns generically
otherwise. No-op when `fetch_args$return_empty` is `TRUE`.

## Usage

``` r
.check_cast_empty(result, fetched, source, signals, geo_type, fetch_args)
```

## Arguments

- result:

  the filtered result (a data frame)

- fetched:

  the combined server response before local filtering

- source, signals, geo_type:

  the query parameters, for error/warning messages and for looking up
  [`epidata_meta()`](https://cmu-delphi.github.io/epidatr/reference/epidata_meta.md)

- fetch_args:

  a `fetch_args` object
