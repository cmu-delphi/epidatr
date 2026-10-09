# V3/V4 to V5 Migration Guide

``` r

library(epidatr)
```

The Delphi Epidata API has transitioned from its V4 endpoint
([`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md))
and legacy V3 endpoints (such as
[`pub_fluview()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview.md),
[`pub_flusurv()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_flusurv.md),
and
[`pvt_quidel()`](https://cmu-delphi.github.io/epidatr/dev/reference/pvt_quidel.md))
to a new set of V5 endpoints, served by
[`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md),
[`epidata_archive()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md),
[`epidata_aux()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_aux.md),
and
[`epidata_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_meta.md).

**As of September 22, 2026, the V3 and V4 APIs are deprecated and no
longer receive new data.** They still serve historical data ingested
before that date, so existing queries will not break, but any
`{pub/pvt}_*` call that needs current data must use the V5 endpoints.
The V5 API is now live for all ongoing datasets. New analyses should use
the V5 functions.

For the current list of sources and indicators available on the new API,
see the [V5 signals
documentation](https://cmu-delphi.github.io/delphi-epidata/api/v5_signals.html).

This guide walks through the transition from
[`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md)
and other legacy endpoints. While
[`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md)
is the most widely used legacy endpoint, V3 endpoints differ in their
function names and parameter conventions. The tables below compare both
V4
([`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md))
and V3 (using
[`pub_fluview()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview.md)
as an example) to their V5 equivalents.

## Endpoint mapping

The legacy endpoints split into several purpose-built V5 routes
determined by query type. The “V3 (Other Endpoints)” column highlights
examples
([`pub_fluview()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview.md),
[`pub_flusurv()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_flusurv.md),
[`pub_wiki()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_wiki.md))
to illustrate differences across endpoints. Refer to each endpoint’s
documentation for specific behavior:

| Task | V4 (`pub_covidcast`) | V3 (Other Endpoints) | V5 Equivalent |
|----|----|----|----|
| Fetch latest data or snapshot as of a past date | [`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md) (default or with `as_of`) | Endpoint-specific ([`pub_fluview()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview.md) has no `as_of`) | [`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md) |
| Fetch full revision history for a signal | `pub_covidcast(issues = ...)` | Supported by some ([`pub_fluview()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview.md), [`pub_flusurv()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_flusurv.md) with `issues`) | [`epidata_archive()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md) |
| Discover sources, signals, geo types, and date ranges | [`pub_covidcast_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast_meta.md), [`covidcast_epidata()`](https://cmu-delphi.github.io/epidatr/dev/reference/covidcast_epidata.md) | Shared meta for some ([`pub_fluview_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview_meta.md)) | [`epidata_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_meta.md) |
| Access source-specific auxiliary tables | none | none | [`epidata_aux()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_aux.md) |
| Filter by publication lag | `pub_covidcast(lag = ...)` | Supported by some ([`pub_fluview()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview.md), [`pub_flusurv()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_flusurv.md)) | none (compute `report_time - reference_time`) |

[`epidata()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md)
is a convenience wrapper that routes to
[`epidata_archive()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md)
if you pass `report_time`, or to
[`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md)
if you pass `snapshot_date` (or neither).

## Argument changes

Most
[`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md)
arguments carry over to V5 with the same name, but some have been
renamed, dropped, or added. Historical V3 endpoints do not share
argument names with
[`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md).
Arguments for
[`pub_fluview()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview.md)
are shown below as an example, but consult each endpoint’s documentation
for details:

| V4 argument (`pub_covidcast`) | V3 (`pub_fluview`) | V5 argument | Notes |
|----|----|----|----|
| `source` (`data_source`) | not exposed (identified by function name [`pub_fluview()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview.md)) | `source` | Identifies the source dataset in V5 (replaces V4 `source` and V3 endpoint names). |
| `signals` | none (implicit from endpoint) | `signals` | Identifies the specific signal name within the source. |
| `geo_type` | not exposed ([`pub_fluview()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview.md) supports only `regions`) | `geo_type` | Specifies geographic resolution (e.g., `state`, `county`, `hhs`, `nation`). |
| `geo_values` | `regions` for [`pub_fluview()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview.md) | `geo_values` | Filtered server-side via the `geo_value` API parameter. |
| `time_type` | not exposed ([`pub_fluview()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview.md) is always `epiweeks`) | none | Dropped. All V5 endpoints use standard calendar dates (`Date`). |
| `time_values` | `epiweeks` for [`pub_fluview()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview.md) | `reference_time` | Filtered server-side via the `reference_times` API parameter. |
| `as_of` | none ([`pub_fluview()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview.md) has no `as_of`) | `snapshot_date` | In V5, used only in [`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md) to fetch data known as of a past date. `NULL` returns the latest data. |
| `issues` | `issues` (where supported) | `report_time` | In V5, used only in [`epidata_archive()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md). Accepts operators like `"<2025-10-16>"`, or [`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md). (For a single date, use [`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md)). |
| `lag` | `lag` (where supported) | none | Removed in V5. You can compute it yourself: fetch from [`epidata_archive()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md) and filter by `report_time - reference_time`. See [filtering by lag](#lag). |
| none | none | `fill_method` | New in V5. Selects the imputation method when aggregating sub-geographies (`"source"`, `"ave"`, or `"zero"`). See [below](#fill-method). |
| none | none | `...` | New in V5. Filters on source-specific dimensions (such as `age_group` or `nwss_source`). |

The new functions also add `fill_method`, which has no covidcast
equivalent. Some sources publish several variants of the same signal
that differ in how nulls were handled during geographic aggregation:

- `"source"` is the raw source data, with no imputation
- `"ave"` has null values filled with the average of neighboring values
- `"zero"` has null values filled with zero

The default `NULL` returns all variants, so filter on this column (or
pass a value to the argument) if you want exactly one time series per
location.

## Column changes

Response fields follow a similar pattern. In the table below,
[`pub_fluview()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview.md)
serves as an example of an endpoint with custom fields. Column names
vary across legacy endpoints (for example,
[`pub_wiki()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_wiki.md)
returns `article`, `count`, and `hour`):

| V4 column (`pub_covidcast`) | V3 (`pub_fluview`) | V5 column | Notes |
|----|----|----|----|
| `source` | not returned (implicit from endpoint) | dropped | Omitted in V5 responses because the source is already specified in the request. |
| `signal` | none (implicit from endpoint) | `signal` | Identifies the signal name in V5. |
| `value` | Endpoint-specific columns (e.g. `num_ili`, `wili`, `ili`) | `value` | Standardized metric value column across all V5 sources. |
| not returned | not returned (implicit from endpoint) | `geo_type` | Explicitly included in V5 responses to identify geographic resolution. |
| `geo_value` | `region` for [`pub_fluview()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview.md) | `geo_value` | Standardized location identifier across all V5 responses. |
| `time_value` | `epiweek` for [`pub_fluview()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview.md) | `reference_time` | Standardized date in `YYYY-MM-DD` format representing the observation period. |
| `issue` | `issue` (where returned) | `report_time` | Standardized date in `YYYY-MM-DD` format representing when the data point was published. Present in both snapshot and archive output. |
| `lag` | `lag` (where returned) | dropped | Omitted in V5 responses. You can compute it yourself as `report_time - reference_time`. See [calculating reporting lag](https://cmu-delphi.github.io/epidatr/articles/versioned-data.html#calculating-reporting-lag). |
| `direction` | none | dropped | Deprecated in V4 and dropped in V5. |
| `stderr`, `sample_size` | none | `ci_lower`, `ci_upper` | Expresses uncertainty as explicit confidence interval bounds on `value` when provided by the data source. See [Uncertainty columns](#uncertainty-columns) below. |
| `missing_value`, `missing_stderr`, `missing_sample_size` | none | dropped | Replaced in V5 by `fill_method` variants and plain `NA`s in `value`. |
| none | none | `fill_method` | Indicates which null-handling imputation method was applied (`"source"`, `"ave"`, or `"zero"`). See [above](#fill-method). |

Some sources also carry extra columns in the new API, for example
`age_group`
([pophive](https://cmu-delphi.github.io/delphi-epidata/api/v5-signals/epic-cosmos.html))
and `nwss_source`, `sample_index`, `pcr_target`
([nwss](https://cmu-delphi.github.io/delphi-epidata/api/v5-signals/nwss.html)).
For more information on whether the source you’re interested in provides
extra columns, please visit that source’s documentation page.

### Uncertainty columns

The covidcast columns `stderr` and `sample_size` have no fixed
replacement. The shared schema carries only `value`; a source that
quantifies uncertainty adds its own columns, such as `ci_lower` and
`ci_upper`. Use the metadata function or the
[documentation](https://cmu-delphi.github.io/delphi-epidata/api/v5_signals.html)
to see which value columns a source returns:

``` r

meta_sleepcycle <- epidata_meta(source = "sleepcycle")
meta_sleepcycle$value_columns
#> [1] "ci_lower" "ci_upper" "value"
```

## A query, before and after

### V4 query example: NSSP COVIDcast

Fetching NSSP influenza ED visit percentages for two states, as the data
looked on January 1, 2025:

``` r

old <- pub_covidcast(
  source = "nssp",
  signals = "pct_ed_visits_influenza",
  geo_type = "state",
  time_type = "week",
  geo_values = c("pa", "ca"),
  time_values = epirange(202440, 202501),
  as_of = 20250101
)
#> Warning: `pub_covidcast()` uses the V4 Epidata API.
#> ℹ As of September 22, 2026, V4 no longer receives new data. It still serves the
#>   historical data it already has, but for current data you must use the V5
#>   endpoints (`epidata_snapshot()`, `epidata_archive()`, `epidata_meta()`) with
#>   an up-to-date epidatr (and epiprocess, if you use it).
#> ℹ See `vignette("migration-guide")` (or
#>   <https://cmu-delphi.github.io/epidatr/articles/migration-guide.html>) for the
#>   endpoint, argument, and column mapping.
#> This warning is displayed once every 8 hours.
head(old)
#> # A tibble: 6 × 15
#>   geo_value signal     source geo_type time_type time_value direction issue     
#>   <chr>     <chr>      <chr>  <fct>    <fct>     <date>         <dbl> <date>    
#> 1 ca        pct_ed_vi… nssp   state    week      2024-09-29        NA 2026-09-20
#> 2 pa        pct_ed_vi… nssp   state    week      2024-09-29        NA 2026-09-20
#> 3 ca        pct_ed_vi… nssp   state    week      2024-10-06        NA 2026-09-20
#> 4 pa        pct_ed_vi… nssp   state    week      2024-10-06        NA 2026-09-20
#> 5 ca        pct_ed_vi… nssp   state    week      2024-10-13        NA 2026-09-20
#> 6 pa        pct_ed_vi… nssp   state    week      2024-10-13        NA 2026-09-20
#> # ℹ 7 more variables: lag <dbl>, missing_value <dbl>, missing_stderr <dbl>,
#> #   missing_sample_size <dbl>, value <dbl>, stderr <dbl>, sample_size <dbl>
```

``` r

new <- epidata_snapshot(
  source = "nssp",
  signals = "pct_ed_visits_influenza",
  geo_type = "state",
  geo_values = c("pa", "ca"),
  reference_time = epirange("2024-10-01", "2025-01-01"),
  snapshot_date = "2025-01-01"
)
head(new)
#> # A tibble: 6 × 7
#>   signal report_time         geo_type geo_value fill_method reference_time value
#>   <chr>  <dttm>              <chr>    <chr>     <chr>       <date>         <dbl>
#> 1 pct_e… 2024-11-08 00:00:00 state    ca        source      2024-10-05     0.140
#> 2 pct_e… 2024-11-08 00:00:00 state    ca        source      2024-10-12     0.140
#> 3 pct_e… 2024-11-23 00:00:00 state    ca        source      2024-10-19     0.160
#> 4 pct_e… 2024-11-08 00:00:00 state    ca        source      2024-10-26     0.200
#> 5 pct_e… 2024-12-03 00:00:00 state    ca        source      2024-11-02     0.25 
#> 6 pct_e… 2024-12-13 00:00:00 state    ca        source      2024-11-09     0.310
```

Both queries return the same signal, just with renamed and reshaped
columns:

``` r

names(old)
#>  [1] "geo_value"           "signal"              "source"             
#>  [4] "geo_type"            "time_type"           "time_value"         
#>  [7] "direction"           "issue"               "lag"                
#> [10] "missing_value"       "missing_stderr"      "missing_sample_size"
#> [13] "value"               "stderr"              "sample_size"
names(new)
#> [1] "signal"         "report_time"    "geo_type"       "geo_value"     
#> [5] "fill_method"    "reference_time" "value"
```

### V3 query example: FluView

For V3 endpoints like
[`pub_fluview()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview.md),
metric names that used to be separate columns (such as `num_ili`, `ili`,
`wili`) become individual signal names queried via `signals`, and
results are standardized into the single `value` column:

``` r

old_flu <- pub_fluview(
  regions = "nat",
  epiweeks = epirange(202440, 202445)
)
#> Warning: `pub_fluview()` uses the V4 Epidata API.
#> ℹ As of September 22, 2026, V4 no longer receives new data. It still serves the
#>   historical data it already has, but for current data you must use the V5
#>   endpoints (`epidata_snapshot()`, `epidata_archive()`, `epidata_meta()`) with
#>   an up-to-date epidatr (and epiprocess, if you use it).
#> ℹ See `vignette("migration-guide")` (or
#>   <https://cmu-delphi.github.io/epidatr/articles/migration-guide.html>) for the
#>   endpoint, argument, and column mapping.
#> This warning is displayed once every 8 hours.
head(old_flu[, c("release_date", "region", "epiweek", "wili", "ili")])
#> # A tibble: 6 × 5
#>   release_date region epiweek     wili   ili
#>   <date>       <chr>  <date>     <dbl> <dbl>
#> 1 2026-10-02   nat    2024-09-29  1.91  1.85
#> 2 2026-10-02   nat    2024-10-06  2.02  1.94
#> 3 2026-10-02   nat    2024-10-13  2.07  2.01
#> 4 2026-10-02   nat    2024-10-20  2.22  2.16
#> 5 2026-10-02   nat    2024-10-27  2.32  2.23
#> 6 2026-10-02   nat    2024-11-03  2.47  2.39
```

``` r

new_flu <- epidata_snapshot(
  source = "fluview_ilinet",
  signals = "wili",
  geo_type = "nation",
  geo_values = "us",
  reference_time = epirange("2024-10-01", "2024-11-15")
)
head(new_flu)
#> # A tibble: 6 × 8
#>   signal report_time         geo_type geo_value fill_method reference_time
#>   <chr>  <dttm>              <chr>    <chr>     <chr>       <date>        
#> 1 wili   2025-09-12 00:00:00 nation   us        source      2024-10-12    
#> 2 wili   2025-09-12 00:00:00 nation   us        source      2024-10-19    
#> 3 wili   2025-09-12 00:00:00 nation   us        source      2024-10-26    
#> 4 wili   2025-09-12 00:00:00 nation   us        source      2024-11-02    
#> 5 wili   2025-09-12 00:00:00 nation   us        source      2024-11-09    
#> 6 wili   2025-11-14 00:00:00 nation   us        source      2024-10-05    
#> # ℹ 2 more variables: age_group <chr>, value <dbl>
```

## Revision history queries

Where you pass `issues` to
[`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md),
use
[`epidata_archive()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md)
with `report_time`:

``` r

old_revisions <- pub_covidcast(
  source = "nssp",
  signals = "pct_ed_visits_influenza",
  geo_type = "state",
  time_type = "week",
  geo_values = "pa",
  time_values = epirange(202440, 202501),
  issues = epirange(202440, 202522)
)
head(old_revisions)
#> # A tibble: 6 × 15
#>   geo_value signal     source geo_type time_type time_value direction issue     
#>   <chr>     <chr>      <chr>  <fct>    <fct>     <date>         <dbl> <date>    
#> 1 pa        pct_ed_vi… nssp   state    week      2024-09-29        NA 2024-11-03
#> 2 pa        pct_ed_vi… nssp   state    week      2024-09-29        NA 2024-11-10
#> 3 pa        pct_ed_vi… nssp   state    week      2024-09-29        NA 2024-11-17
#> 4 pa        pct_ed_vi… nssp   state    week      2024-09-29        NA 2024-11-24
#> 5 pa        pct_ed_vi… nssp   state    week      2024-09-29        NA 2024-12-01
#> 6 pa        pct_ed_vi… nssp   state    week      2024-09-29        NA 2024-12-08
#> # ℹ 7 more variables: lag <dbl>, missing_value <dbl>, missing_stderr <dbl>,
#> #   missing_sample_size <dbl>, value <dbl>, stderr <dbl>, sample_size <dbl>
```

``` r

revisions <- epidata_archive(
  source = "nssp",
  signals = "pct_ed_visits_influenza",
  geo_type = "state",
  geo_values = "pa",
  reference_time = epirange("2024-10-01", "2025-01-01"),
  report_time = "<2025-06-01"
)
head(revisions)
#> # A tibble: 6 × 7
#>   signal       report_time         geo_type geo_value fill_method reference_time
#>   <chr>        <dttm>              <chr>    <chr>     <chr>       <date>        
#> 1 pct_ed_visi… 2024-11-08 00:00:00 state    pa        source      2024-10-05    
#> 2 pct_ed_visi… 2024-11-08 00:00:00 state    pa        source      2024-10-12    
#> 3 pct_ed_visi… 2024-11-08 00:00:00 state    pa        source      2024-10-19    
#> 4 pct_ed_visi… 2024-11-08 00:00:00 state    pa        source      2024-10-26    
#> 5 pct_ed_visi… 2024-11-23 00:00:00 state    pa        source      2024-11-02    
#> 6 pct_ed_visi… 2024-11-08 00:00:00 state    pa        source      2024-11-02    
#> # ℹ 1 more variable: value <dbl>
```

If you filtered by `lag`, fetch the archive with
[`epidata_archive()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md)
and filter afterwards:

``` r

# For an exact lag (e.g., 7 days):
revisions %>%
  filter(as.integer(report_time - reference_time) == 7)

# Or for maximum latency (e.g., at most 7 days of delay):
revisions %>%
  filter(as.integer(report_time - reference_time) <= 7)
```

## Inspecting a source in the new API

Use
[`epidata_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_meta.md)
to see what a source offers in the new API. It returns signals, geo
types, and the available `reference_time` and `report_time` ranges:

``` r

meta <- epidata_meta(source = "nssp")

# all the fields available for this source
names(meta)
#> [1] "report_time_range"    "reference_time_range" "signals"             
#> [4] "geo_types"            "key_columns"          "extra_key_columns"   
#> [7] "value_columns"        "column_types"

meta$signals # available signal names
#> [1] "pct_ed_visits_ari"                "pct_ed_visits_combined"          
#> [3] "pct_ed_visits_covid"              "pct_ed_visits_influenza"         
#> [5] "pct_ed_visits_rsv"                "smoothed_pct_ed_visits_combined" 
#> [7] "smoothed_pct_ed_visits_covid"     "smoothed_pct_ed_visits_influenza"
#> [9] "smoothed_pct_ed_visits_rsv"
meta$geo_types # supported geography levels
#> [1] "census_division" "census_region"   "county"          "hhs"            
#> [5] "hrr"             "hsa_nci"         "msa"             "nation"         
#> [9] "state"
meta$reference_time_range # earliest/latest reference_time available
#> $latest
#> [1] "2026-10-03"
#> 
#> $first
#> [1] "2022-10-01"
meta$report_time_range # earliest/latest report_time (publication date) available
#> $latest
#> [1] "2026-10-07T00:00:00Z"
#> 
#> $first
#> [1] "2024-04-18T00:00:00Z"
```

All ongoing datasets are now on the V5 API. If
[`epidata_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_meta.md)
does not recognize a source name, check the [V5 signals
documentation](https://cmu-delphi.github.io/delphi-epidata/api/v5_signals.html)
for its current name, or the [API mailing
list](https://lists.andrew.cmu.edu/mailman/listinfo/delphi-covidcast-api)
for announcements. A handful of legacy `{pub/pvt}_*` endpoints cover
data sources that stopped updating before the transition. Those are
listed below.

## Endpoints kept for historical reference

Not every V4 endpoint is moving to V5. The functions below cover data
sources whose collection has already ended (e.g. Google Flu Trends, the
Twitter/HealthTweets signal, the various nowcasts). They are not part of
the V4-to-V5 transition, so they are not deprecated and will keep
working. The historical data they return is frozen and will remain
available. They will just no longer receive new data.

| Function | Data source |
|----|----|
| [`pvt_cdc()`](https://cmu-delphi.github.io/epidatr/dev/reference/pvt_cdc.md) | CDC total and by-topic webpage visits |
| [`pub_covid_hosp_facility_lookup()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covid_hosp_facility_lookup.md) | COVID hospitalization facility lookup |
| [`pub_covid_hosp_facility()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covid_hosp_facility.md) | COVID hospitalizations by facility |
| [`pub_covid_hosp_state_timeseries()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covid_hosp_state_timeseries.md) | COVID hospitalizations by state |
| [`pub_delphi()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_delphi.md) | Delphi’s ILINet outpatient doctor visits forecasts |
| [`pub_dengue_nowcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_dengue_nowcast.md) | Delphi’s PAHO dengue nowcasts (Americas) |
| [`pvt_dengue_sensors()`](https://cmu-delphi.github.io/epidatr/dev/reference/pvt_dengue_sensors.md) | PAHO dengue digital surveillance sensors (Americas) |
| [`pub_ecdc_ili()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_ecdc_ili.md) | ECDC ILI incidence (Europe) |
| [`pub_gft()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_gft.md) | Google Flu Trends flu search volume |
| [`pvt_ght()`](https://cmu-delphi.github.io/epidatr/dev/reference/pvt_ght.md) | Google Health Trends health topics search volume |
| [`pub_kcdc_ili()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_kcdc_ili.md) | KCDC ILI incidence (Korea) |
| [`pvt_meta_norostat()`](https://cmu-delphi.github.io/epidatr/dev/reference/pvt_meta_norostat.md) | Metadata for the NoroSTAT endpoint |
| [`pub_nidss_dengue()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_nidss_dengue.md) | NIDSS dengue cases (Taiwan) |
| [`pub_nidss_flu()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_nidss_flu.md) | NIDSS flu doctor visits (Taiwan) |
| [`pvt_norostat()`](https://cmu-delphi.github.io/epidatr/dev/reference/pvt_norostat.md) | CDC NoroSTAT norovirus outbreaks |
| [`pub_nowcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_nowcast.md) | Delphi’s ILI Nearby nowcasts |
| [`pub_paho_dengue()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_paho_dengue.md) | PAHO dengue data (Americas) |
| [`pvt_sensors()`](https://cmu-delphi.github.io/epidatr/dev/reference/pvt_sensors.md) | Influenza and dengue digital surveillance sensors |
| [`pvt_twitter()`](https://cmu-delphi.github.io/epidatr/dev/reference/pvt_twitter.md) | HealthTweets total and influenza-related tweets |
| [`pub_wiki()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_wiki.md) | Wikipedia webpage counts by article |
