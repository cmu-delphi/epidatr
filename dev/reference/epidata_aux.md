# Fetch V5 auxiliary data

Fetch auxiliary data associated with a cast signal.

You can pass a source string to fetch the auxiliary data directly.
Alternatively, you can pass the output of
[`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md)
or
[`epidata_archive()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md).
In this case, `epidata_aux` automatically retrieves the source from the
object, fetches the matching auxiliary data, and performs a
version-aware left join onto the base data.

For the auxiliary key columns and their allowed values, see the source's
API docs, e.g. NWSS:
<https://cmu-delphi.github.io/delphi-epidata/api/v5-signals/nwss.html>.

## Usage

``` r
epidata_aux(source, ...)

# Default S3 method
epidata_aux(
  source,
  ...,
  reference_time = "*",
  time_values = lifecycle::deprecated(),
  report_time = "*",
  issues = lifecycle::deprecated(),
  columns = NULL,
  fetch_args = fetch_args_list()
)

# S3 method for class 'data.frame'
epidata_aux(source, ..., columns = NULL, fetch_args = fetch_args_list())
```

## Arguments

- source:

  A source string to retrieve auxiliary data directly, or a tibble
  returned by
  [`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md)
  or
  [`epidata_archive()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md)
  to merge the data onto (its source is recovered automatically).

- ...:

  Named filters on the auxiliary key columns, such as
  `pcr_target = "sars-cov-2"` or `geo_value = c("ca", "ny")`. Each key
  accepts one or more values (matched as OR); they are serialized as
  repeated `key:value` terms server-side to keep the aux pull small.
  Passing more than 10 values for a key warns, since the request URL may
  get too long. In merge mode, when no filters are given, they are
  inferred from the base: each key it narrows to at most 10 distinct
  values is filtered to those.

- reference_time:

  [`timeset`](https://cmu-delphi.github.io/epidatr/dev/reference/timeset.md).
  Reference time to return (filters on the `reference_time` column).
  Supports individual dates or
  [`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md).
  Base-pull mode only (when `source` is a string).

- time_values:

  **\[deprecated\]** Use `reference_time` instead.

- report_time:

  A date, string, or
  [`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md)
  specifying the version of the auxiliary data to retrieve. Base-pull
  mode only (when `source` is a string).

- issues:

  **\[deprecated\]** Use `report_time` instead.

- columns:

  A character vector of columns to return. By default, all columns are
  returned.

- fetch_args:

  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md).
  See
  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md)
  for details.

## Value

A
[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html).

## See also

[`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md),
[`epidata_archive()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md),
[`epidata_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_meta.md)
