# cast-API snapshot and archive queries

- `epidata_snapshot` fetches a snapshot of signals as they appeared at a
  specific date (or the latest available if `as_of` is omitted).

- `epidata_archive` fetches the full version history of signals across
  all available issues.

- `epidata` is a wrapper that routes to one of the above based on which
  versioning argument is supplied.

## Usage

``` r
epidata_snapshot(
  source,
  signals,
  geo_type,
  geo_values = "*",
  time_values = "*",
  ...,
  fill_method = NULL,
  as_of = NULL,
  fetch_args = fetch_args_list()
)

epidata_archive(
  source,
  signals,
  geo_type,
  geo_values = "*",
  time_values = "*",
  ...,
  fill_method = NULL,
  version = "*",
  fetch_args = fetch_args_list()
)

epidata(
  source,
  signals,
  geo_type,
  geo_values = "*",
  time_values = "*",
  ...,
  fill_method = NULL,
  as_of = NULL,
  version = NULL,
  fetch_args = fetch_args_list()
)
```

## Arguments

- source:

  string. The data source to query (e.g., `"nssp"`, `"nhsn"`). Use
  [`epidata_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_meta.md)
  to discover available sources.

- signals:

  character vector. One or more signals to query for the given source.
  Use
  [`epidata_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_meta.md)
  to discover available signals.

- geo_type:

  string. The geography type to query (e.g., `"state"`, `"nation"`,
  `"county"`). Use
  [`epidata_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_meta.md)
  to discover available geo types for a given source and signal.

- geo_values:

  character. The geographies to return. Defaults to all ("\*")
  geographies within requested geographic resolution (see:
  <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html>.).

- time_values:

  [`timeset`](https://cmu-delphi.github.io/epidatr/dev/reference/timeset.md).
  Time values to return. Supports individual dates or
  [`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md).
  Defaults to all (`"*"`). Filtered locally after the API call.

- ...:

  not used for values, forces later arguments to bind by name

- fill_method:

  string. Optional filter to an imputation method. The API provides
  alternatives of the same signal differing in how nulls were handled
  during geographic aggregation: `"source"` means no imputation or
  aggregation (raw source data), `"fill_ave"` fills nulls with the
  average of neighboring values, and `"fill_zero"` fills nulls with
  zero. `NULL` (default) returns all fill methods.

- as_of:

  Date or `NULL`. The snapshot date; `NULL` returns the latest available
  version. Internally maps to the `snapshot_date` parameter.

- fetch_args:

  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md).
  See
  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md)
  for details.

- version:

  Date, string, or
  [`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md).
  A version query for the archive endpoint. Supports exact dates (e.g.,
  `"2025-10-16"`), operators (e.g., `"<2025-10-16"`), or an
  [`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md).
  Internally maps to the `version_query` parameter.

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## See also

[`epidata_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_meta.md),
[`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md)
