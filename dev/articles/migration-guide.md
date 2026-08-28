# Migrating from pub_covidcast to the new Epidata API

``` r

library(epidatr)
```

The Delphi Epidata API is moving from its V4 endpoints
([`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md)
and other `{pub/pvt}_*` endpoints, such as
[`pub_fluview()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_fluview.md),
[`pub_flusurv()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_flusurv.md),
and
[`pvt_quidel()`](https://cmu-delphi.github.io/epidatr/dev/reference/pvt_quidel.md))
to a new set of V5 endpoints, served by
[`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md),
[`epidata_archive()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md),
and
[`epidata_meta()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_meta.md).
The transition is in progress: sources are moving to the new API one at
a time, and the V4 functions still work for sources that have not moved
yet. New analyses should start with the new functions and fall back to a
V4 function only when a source is not yet available there.

For the current list of sources and indicators available on the new API,
see the [V5 signals
documentation](https://cmu-delphi.github.io/delphi-epidata/api/v5_signals.html).

This guide walks through
[`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md)’s
arguments and columns in detail, since it’s the most widely used V4
endpoint, but the mapping is the same for the other `{pub/pvt}_*`
endpoints.

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
#> 1 ca        pct_ed_vi… nssp   state    week      2024-09-29        NA 2026-08-23
#> 2 pa        pct_ed_vi… nssp   state    week      2024-09-29        NA 2026-08-23
#> 3 ca        pct_ed_vi… nssp   state    week      2024-10-06        NA 2026-08-23
#> 4 pa        pct_ed_vi… nssp   state    week      2024-10-06        NA 2026-08-23
#> 5 ca        pct_ed_vi… nssp   state    week      2024-10-13        NA 2026-08-23
#> 6 pa        pct_ed_vi… nssp   state    week      2024-10-13        NA 2026-08-23
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
  reference_time = epirange("2024-10-01", "2025-01-01"),
  report_time = "<2025-06-01"
)
head(revisions)
#> # A tibble: 6 × 7
#>   signal        report_time geo_type geo_value fill_method reference_time  value
#>   <chr>         <date>      <chr>    <chr>     <chr>       <date>          <dbl>
#> 1 pct_ed_visit… 2024-11-08  state    pa        source      2024-10-05     0.0500
#> 2 pct_ed_visit… 2024-11-08  state    pa        source      2024-10-12     0.0700
#> 3 pct_ed_visit… 2024-11-08  state    pa        source      2024-10-19     0.0800
#> 4 pct_ed_visit… 2024-11-08  state    pa        source      2024-10-26     0.130 
#> 5 pct_ed_visit… 2024-11-08  state    pa        source      2024-11-02     0.140 
#> 6 pct_ed_visit… 2024-11-23  state    pa        source      2024-10-05     0.0500
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
(or the relevant `{pub/pvt}_*` function) for it and check back after
package updates. The [API mailing
list](http://lists.andrew.cmu.edu/mailman/listinfo/delphi-covidcast-api)
announces sources as they move.

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
