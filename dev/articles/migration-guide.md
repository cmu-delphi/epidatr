# Migrating from pub_covidcast to the new Epidata API

``` r

library(epidatr)
```

The Delphi Epidata API is moving from the covidcast endpoint (API v4,
served by
[`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md))
to a new set of endpoints (API v5, served by
[`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md),
[`epidata_archive()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md),
and
[`epidata_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_meta.md)).
The transition is in progress: sources are moving to the new API one at
a time, and
[`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md)
still works for sources that have not moved yet. New analyses should
start with the new functions and fall back to
[`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md)
only when a source is not yet available there.

This guide maps the old interface onto the new one.

## Function mapping

| Old | New | Purpose |
|----|----|----|
| [`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md) | [`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md) | Data as it appeared on a given date (or the latest) |
| `pub_covidcast(issues = ...)` | [`epidata_archive()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md) | Full revision history of a signal |
| [`pub_covidcast_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast_meta.md), [`covidcast_epidata()`](https://cmu-delphi.github.io/epidatr/dev/reference/covidcast_epidata.md) | [`epidata_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_meta.md) | Discover sources, signals, geo types, and date ranges |

[`epidata()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md)
is a convenience wrapper that routes to
[`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md)
or
[`epidata_archive()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md)
based on which versioning argument you pass.

## Argument changes

| [`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md) argument | New argument | Notes |
|----|----|----|
| `source`, `signals`, `geo_type`, `geo_values` | same |  |
| `time_type` | none | Dropped. Times in the new API are always `Date`s. |
| `time_values` | `reference_time` | Accepts dates or [`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md). Filtered locally after the fetch. |
| `as_of` | `snapshot_date` | [`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md) only. `NULL` returns the latest data. |
| `issues` | `report_time` | [`epidata_archive()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md) only. Accepts exact dates, operators like `"<2025-10-16"`, or [`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md). |
| `lag` | none | Compute it yourself: `report_time - reference_time`. |

The new functions also add `fill_method`, which has no covidcast
equivalent. Some sources publish several variants of the same signal
that differ in how nulls were handled during geographic aggregation:
`"source"` (raw source data, no imputation), `"fill_ave"` (nulls filled
with the average of neighboring values), and `"fill_zero"` (nulls filled
with zero). The default `NULL` returns all variants, so filter on this
column (or pass the argument) if you want exactly one time series per
location.

## Column changes

| [`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md) column | New column | Notes |
|----|----|----|
| `geo_value`, `geo_type`, `signal`, `value` | same |  |
| `time_value` | `reference_time` | The date the value describes. Always a `Date`. |
| `issue` | `report_time` | The date the value was published. Present in both snapshot and archive output. |
| `source` | dropped | You queried by source; add it back with [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) if you bind rows across sources. |
| `time_type` | dropped | No longer needed since times are `Date`s. |
| `lag` | dropped | Compute as `report_time - reference_time`. |
| `direction` | dropped | Was already deprecated in the covidcast API. |
| `stderr`, `sample_size` | `ci_lower`, `ci_upper` | Uncertainty is now expressed as confidence interval bounds on `value` instead of a standard error. Populated only for sources that publish them. See below. |
| `missing_value`, `missing_stderr`, `missing_sample_size` | dropped | Missingness is now expressed through `fill_method` variants and plain `NA`s. |
| none | `fill_method` | Which null-handling variant of the signal this row belongs to. See above. |

Some sources also carry extra columns in the new API, for example
`age_group` (pophive) and `nwss_source`, `sample_index`, `pcr_target`
(nwss).

### Uncertainty columns

The covidcast columns `stderr` and `sample_size` have no fixed
replacement. The shared schema carries only `value`; a source that
quantifies uncertainty adds its own columns, such as `ci_lower` and
`ci_upper`. Use the metadata or the
[documentation](https://cmu-delphi.github.io/delphi-epidata/api/v5_signals.html)
to see which value columns a source returns:

``` r

meta_sleepcycle <- epidata_meta(source = "sleepcycle")
meta_sleepcycle$sleepcycle$value_columns
#> [1] "ci_lower" "ci_upper" "value"
```

## A query, before and after

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
head(old)
#> # A tibble: 6 × 15
#>   geo_value signal     source geo_type time_type time_value direction issue     
#>   <chr>     <chr>      <chr>  <fct>    <fct>     <date>         <dbl> <date>    
#> 1 ca        pct_ed_vi… nssp   state    week      2024-09-29        NA 2026-08-16
#> 2 pa        pct_ed_vi… nssp   state    week      2024-09-29        NA 2026-08-16
#> 3 ca        pct_ed_vi… nssp   state    week      2024-10-06        NA 2026-08-16
#> 4 pa        pct_ed_vi… nssp   state    week      2024-10-06        NA 2026-08-16
#> 5 ca        pct_ed_vi… nssp   state    week      2024-10-13        NA 2026-08-16
#> 6 pa        pct_ed_vi… nssp   state    week      2024-10-13        NA 2026-08-16
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
#>   signal         report_time geo_type geo_value fill_method reference_time value
#>   <chr>          <date>      <chr>    <chr>     <chr>       <date>         <dbl>
#> 1 pct_ed_visits… 2024-12-27  state    ca        source      2024-10-05     0.140
#> 2 pct_ed_visits… 2024-12-27  state    ca        source      2024-10-12     0.140
#> 3 pct_ed_visits… 2024-12-27  state    ca        source      2024-10-19     0.160
#> 4 pct_ed_visits… 2024-12-27  state    ca        source      2024-10-26     0.200
#> 5 pct_ed_visits… 2024-12-27  state    ca        source      2024-11-02     0.25 
#> 6 pct_ed_visits… 2024-12-27  state    ca        source      2024-11-09     0.310
```

## Revision history queries

Where you used to pass `issues` to
[`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md),
use
[`epidata_archive()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md)
with `report_time`:

``` r

revisions <- epidata_archive(
  source = "nssp",
  signals = "pct_ed_visits_influenza",
  geo_type = "state",
  geo_values = "pa",
  report_time = "<2025-06-01"
)
head(revisions)
#> # A tibble: 6 × 7
#>   signal         report_time geo_type geo_value fill_method reference_time value
#>   <chr>          <date>      <chr>    <chr>     <chr>       <date>         <dbl>
#> 1 pct_ed_visits… 2024-04-18  state    pa        source      2022-10-01     0.120
#> 2 pct_ed_visits… 2024-04-18  state    pa        source      2022-10-08     0.100
#> 3 pct_ed_visits… 2024-04-18  state    pa        source      2022-10-15     0.210
#> 4 pct_ed_visits… 2024-04-18  state    pa        source      2022-10-22     0.330
#> 5 pct_ed_visits… 2024-04-18  state    pa        source      2022-10-29     0.770
#> 6 pct_ed_visits… 2024-04-18  state    pa        source      2022-11-05     1.63
```

If you filtered by `lag`, fetch the archive and filter afterwards:

``` r

revisions[revisions$report_time - revisions$reference_time <= 7, ]
```

## Checking whether a source has moved

Use
[`epidata_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_meta.md)
to see what a source offers in the new API. It returns signals, geo
types, and the available `reference_time` and `report_time` ranges:

``` r

meta <- epidata_meta(source = "nssp")
meta$nssp$signals
#> [1] "pct_ed_visits_ari"                "pct_ed_visits_combined"          
#> [3] "pct_ed_visits_covid"              "pct_ed_visits_influenza"         
#> [5] "pct_ed_visits_rsv"                "smoothed_pct_ed_visits_combined" 
#> [7] "smoothed_pct_ed_visits_covid"     "smoothed_pct_ed_visits_influenza"
#> [9] "smoothed_pct_ed_visits_rsv"
meta$nssp$time_value_range
#> NULL
```

If
[`epidata_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_meta.md)
does not know the source yet, keep using
[`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md)
for it and check back after package updates. The [API mailing
list](https://lists.andrew.cmu.edu/mailman/listinfo/delphi-covidcast-api)
announces sources as they move.
