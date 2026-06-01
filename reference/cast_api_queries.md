# cast-API snapshot and archive queries

- `epidata_snapshot` fetches a snapshot of signals as they appeared at a
  specific date (or the latest available if `snapshot_date` is omitted).

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
  reference_time = "*",
  time_values = lifecycle::deprecated(),
  ...,
  fill_method = NULL,
  snapshot_date = NULL,
  as_of = lifecycle::deprecated(),
  fetch_args = fetch_args_list()
)

epidata_archive(
  source,
  signals,
  geo_type,
  geo_values = "*",
  reference_time = "*",
  time_values = lifecycle::deprecated(),
  ...,
  fill_method = NULL,
  report_time = "*",
  issues = lifecycle::deprecated(),
  fetch_args = fetch_args_list()
)

epidata(
  source,
  signals,
  geo_type,
  geo_values = "*",
  reference_time = "*",
  time_values = lifecycle::deprecated(),
  ...,
  fill_method = NULL,
  snapshot_date = NULL,
  as_of = lifecycle::deprecated(),
  report_time = NULL,
  issues = lifecycle::deprecated(),
  fetch_args = fetch_args_list()
)
```

## Arguments

- source:

  string. The data source to query (e.g., `"nssp"`, `"nhsn"`). Use
  [`epidata_meta()`](https://cmu-delphi.github.io/epidatr/reference/epidata_meta.md)
  to discover available sources.

- signals:

  character vector. One or more signals to query for the given source.
  Use
  [`epidata_meta()`](https://cmu-delphi.github.io/epidatr/reference/epidata_meta.md)
  to discover available signals.

- geo_type:

  string. The geography type to query (e.g., `"state"`, `"nation"`,
  `"county"`). Use
  [`epidata_meta()`](https://cmu-delphi.github.io/epidatr/reference/epidata_meta.md)
  to discover available geo types for a given source and signal.

- geo_values:

  character. The geographies to return. Defaults to all ("\*")
  geographies within requested geographic resolution (see:
  <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html>.).

- reference_time:

  [`timeset`](https://cmu-delphi.github.io/epidatr/reference/timeset.md).
  Reference time to return (filters on the `reference_time` column).
  Supports individual dates or
  [`epirange()`](https://cmu-delphi.github.io/epidatr/reference/epirange.md).
  Defaults to all (`"*"`). Filtered locally after the API call.

- time_values:

  **\[deprecated\]** Use `reference_time` instead.

- ...:

  not used for values, forces later arguments to bind by name

- fill_method:

  string. Optional filter to an imputation method. The API provides
  alternatives of the same signal differing in how nulls were handled
  during geographic aggregation: `"source"` means no imputation or
  aggregation (raw source data), `"fill_ave"` fills nulls with the
  average of neighboring values, and `"fill_zero"` fills nulls with
  zero. `NULL` (default) returns all fill methods.

- snapshot_date:

  Date or `NULL`. The snapshot date; `NULL` returns the latest available
  version.

- as_of:

  **\[deprecated\]** Use `snapshot_date` instead.

- fetch_args:

  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/reference/epidata_call.md).
  See
  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md)
  for details.

- report_time:

  Date, string, or
  [`epirange()`](https://cmu-delphi.github.io/epidatr/reference/epirange.md).
  A query on the `report_time` column for the archive endpoint. Supports
  exact dates (e.g., `"2025-10-16"`), operators (e.g., `"<2025-10-16"`),
  or an
  [`epirange()`](https://cmu-delphi.github.io/epidatr/reference/epirange.md).
  Internally maps to the `version_query` API parameter.

- issues:

  **\[deprecated\]** Use `report_time` instead.

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## Data Versioning

`epidata` supports two mutually exclusive versioning arguments. Pass
`snapshot_date` to retrieve data as it appeared on a specific date, or
`report_time` to query the archive by when data was reported. If neither
is supplied, `epidata` returns the latest available snapshot.

## See also

For example queries showing how to discover signals and build calls, see
[`vignette("signal-discovery", package = "epidatr")`](https://cmu-delphi.github.io/epidatr/articles/signal-discovery.md).

## See also

[`epidata_meta()`](https://cmu-delphi.github.io/epidatr/reference/epidata_meta.md),
[`epirange()`](https://cmu-delphi.github.io/epidatr/reference/epirange.md)
