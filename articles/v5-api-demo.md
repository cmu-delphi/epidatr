# epidatr v5 API demo

``` r

library(dplyr)
library(ggplot2)
library(epidatr)
```

### Metadata

Let’s check the source-specific metadata for `nssp`.

``` r

meta_nssp <- epidata_meta(source = "nssp")
meta_nssp$nssp$signals
#> [1] "pct_ed_visits_ari"                "pct_ed_visits_combined"          
#> [3] "pct_ed_visits_covid"              "pct_ed_visits_influenza"         
#> [5] "pct_ed_visits_rsv"                "smoothed_pct_ed_visits_combined" 
#> [7] "smoothed_pct_ed_visits_covid"     "smoothed_pct_ed_visits_influenza"
#> [9] "smoothed_pct_ed_visits_rsv"
meta_nssp$nssp$geo_types
#> [1] "county"  "hhs"     "hrr"     "hsa_nci" "msa"     "nation"  "state"
meta_nssp$nssp$version_range
#> NULL
meta_nssp$nssp$time_value_range
#> NULL
```

### Basic Queries

We can pull the latest snapshot of a signal.

``` r

nssp_data <- epidata_snapshot(
  source = "nssp",
  signal = "pct_ed_visits_influenza",
  geo_type = "state"
)
head(nssp_data)
#> # A tibble: 6 × 7
#>   signal        report_time geo_type geo_value fill_method reference_time  value
#>   <chr>         <date>      <chr>    <chr>     <chr>       <date>          <dbl>
#> 1 pct_ed_visit… 2024-04-18  state    mn        source      2023-10-28     0.110 
#> 2 pct_ed_visit… 2024-04-18  state    ia        source      2023-11-04     0.280 
#> 3 pct_ed_visit… 2024-04-18  state    wi        source      2023-07-15     0.0300
#> 4 pct_ed_visit… 2024-04-18  state    ms        source      2023-01-14     1.04  
#> 5 pct_ed_visit… 2024-04-18  state    mn        source      2022-10-01     0.0900
#> 6 pct_ed_visit… 2024-04-18  state    in        source      2023-06-10     0.0600
```

If you want to inspect the API request URL or query structure without
actually fetching the data, you can use the `dry_run` argument via
[`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md):

``` r

dry_run_call <- epidata_snapshot(
  source = "nssp",
  signal = "pct_ed_visits_influenza",
  geo_type = "state",
  fetch_args = fetch_args_list(dry_run = TRUE)
)
dry_run_call
#> 
#> ── <epidata_call> object: ──────────────────────────────────────────────────────
#> • Pipe this object into `fetch()` to actually fetch the data
#> • Request URL:
#>   https://delphi.cmu.edu/epidata/v5/snapshot/?source=nssp&signal=pct_ed_visits_influenza&geo_type=state
```

Filtering by specific geographies and versions:

``` r

pa_ca_data <- epidata_snapshot(
  source = "nssp",
  signal = "pct_ed_visits_influenza",
  geo_type = "state",
  geo_values = c("PA", "CA"),
  as_of = "2025-01-01" # fetch data as it was known on this date
)
#> Warning: The `as_of` argument of `epidata_snapshot()` is deprecated as of epidatr 1.3.0.
#> ℹ The `as_of` argument is deprecated and will be removed in a future version.
#>   Use `snapshot_date` instead.
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
head(pa_ca_data)
#> # A tibble: 6 × 7
#>   signal         report_time geo_type geo_value fill_method reference_time value
#>   <chr>          <date>      <chr>    <chr>     <chr>       <date>         <dbl>
#> 1 pct_ed_visits… 2024-08-30  state    ca        source      2022-10-01     0.210
#> 2 pct_ed_visits… 2024-08-30  state    ca        source      2022-10-08     0.280
#> 3 pct_ed_visits… 2024-12-07  state    ca        source      2022-10-15     0.450
#> 4 pct_ed_visits… 2024-08-30  state    ca        source      2022-10-22     0.680
#> 5 pct_ed_visits… 2024-11-08  state    ca        source      2022-10-29     1.04 
#> 6 pct_ed_visits… 2024-09-06  state    ca        source      2022-11-05     1.88
```

### Archive Queries

If you want to track how data for a specific time period was revised
over time, you can use
[`epidata_archive()`](https://cmu-delphi.github.io/epidatr/reference/cast_api_queries.md).

``` r

archive_data <- epidata_archive(
  source = "nssp",
  signal = "pct_ed_visits_influenza",
  geo_type = "state"
)
head(archive_data)
#> # A tibble: 6 × 7
#>   signal         report_time geo_type geo_value fill_method reference_time value
#>   <chr>          <date>      <chr>    <chr>     <chr>       <date>         <dbl>
#> 1 pct_ed_visits… 2024-04-18  state    ak        source      2022-10-01     0.140
#> 2 pct_ed_visits… 2024-04-18  state    ak        source      2022-10-08     0.240
#> 3 pct_ed_visits… 2024-04-18  state    ak        source      2022-10-15     0.320
#> 4 pct_ed_visits… 2024-04-18  state    ak        source      2022-10-22     0.760
#> 5 pct_ed_visits… 2024-08-30  state    ak        source      2022-10-29     1.16 
#> 6 pct_ed_visits… 2024-04-18  state    ak        source      2022-10-29     1.17
```

### Other Sources

Here are some examples for NHSN (hospitalizations), POPHIVE, and NWSS
(wastewater).

``` r

# NHSN: Hospital Admissions
meta_nhsn <- epidata_meta(source = "nhsn")
meta_nhsn$nhsn$signals
#> [1] "confirmed_admissions_covid_ew"        
#> [2] "confirmed_admissions_flu_ew"          
#> [3] "confirmed_admissions_rsv_ew"          
#> [4] "hosprep_confirmed_admissions_covid_ew"
#> [5] "hosprep_confirmed_admissions_flu_ew"  
#> [6] "hosprep_confirmed_admissions_rsv_ew"  
#> [7] "inpatient_beds_ew"                    
#> [8] "inpatient_beds_occupied_pct_ew"
meta_nhsn$nhsn$geo_types
#> [1] "hhs"    "nation" "state"
meta_nhsn$nhsn$version_range
#> NULL
meta_nhsn$nhsn$time_value_range
#> NULL
nhsn_data <- epidata_snapshot(
  source = "nhsn",
  signal = "confirmed_admissions_flu_ew",
  geo_type = "state"
)
head(nhsn_data)
#> # A tibble: 6 × 7
#>   signal         report_time geo_type geo_value fill_method reference_time value
#>   <chr>          <date>      <chr>    <chr>     <chr>       <date>         <dbl>
#> 1 confirmed_adm… 2026-05-22  state    ak        source      2025-02-15        53
#> 2 confirmed_adm… 2026-05-22  state    ak        source      2025-02-01        30
#> 3 confirmed_adm… 2026-05-22  state    ak        source      2025-01-25        29
#> 4 confirmed_adm… 2026-05-22  state    ak        source      2025-01-18        33
#> 5 confirmed_adm… 2026-05-22  state    ak        source      2025-01-11        45
#> 6 confirmed_adm… 2026-05-22  state    ak        source      2025-01-04        64

# POPHIVE
meta_pophive <- epidata_meta(source = "pophive")
meta_pophive$pophive$signals
#> [1] "all_n_encounters_ed" "covid_n_ed"          "covid_pct_ed"       
#> [4] "flu_n_ed"            "flu_pct_ed"          "rsv_n_ed"           
#> [7] "rsv_pct_ed"
meta_pophive$pophive$geo_types
#> [1] "hhs"    "nation" "state"
meta_pophive$pophive$version_range
#> NULL
meta_pophive$pophive$time_value_range
#> NULL
pophive_data <- epidata_snapshot(
  source = "pophive",
  signal = "covid_pct_ed",
  geo_type = "state"
)
head(pophive_data)
#> # A tibble: 6 × 8
#>   signal     report_time geo_type geo_value fill_method reference_time age_group
#>   <chr>      <date>      <chr>    <chr>     <chr>       <date>         <chr>    
#> 1 covid_pct… 2025-09-24  state    ut        source      2024-02-17     0-1      
#> 2 covid_pct… 2025-09-24  state    pr        source      2024-11-30     5-18     
#> 3 covid_pct… 2025-09-24  state    ak        source      2023-12-09     0-1      
#> 4 covid_pct… 2025-09-24  state    ak        source      2024-05-25     0-1      
#> 5 covid_pct… 2025-09-24  state    ut        source      2023-04-15     0-1      
#> 6 covid_pct… 2025-09-24  state    pr        source      2025-02-15     1-5      
#> # ℹ 1 more variable: value <dbl>

# NWSS: Wastewater Surveillance
meta_nwss <- epidata_meta(source = "nwss")
meta_nwss$nwss$signals
#>  [1] "covid_avg_conc"             "covid_avg_conc_lin"        
#>  [3] "covid_flowpop_lin"          "covid_mic_lin"             
#>  [5] "flu_avg_conc"               "flu_avg_conc_lin"          
#>  [7] "flu_flowpop_lin"            "flu_h5_avg_conc"           
#>  [9] "flu_h5_avg_conc_lin"        "flu_h5_flowpop_lin"        
#> [11] "flu_h5_mic_lin"             "flu_mic_lin"               
#> [13] "measles_avg_conc"           "measles_avg_conc_lin"      
#> [15] "measles_flowpop_lin"        "measles_mic_lin"           
#> [17] "mpox_all_avg_conc"          "mpox_all_avg_conc_lin"     
#> [19] "mpox_all_flowpop_lin"       "mpox_all_mic_lin"          
#> [21] "mpox_clade_i_avg_conc"      "mpox_clade_i_avg_conc_lin" 
#> [23] "mpox_clade_i_flowpop_lin"   "mpox_clade_ii_avg_conc"    
#> [25] "mpox_clade_ii_avg_conc_lin" "mpox_clade_ii_flowpop_lin" 
#> [27] "mpox_clade_ii_mic_lin"      "mpox_clade_i_mic_lin"      
#> [29] "mpox_nvo_avg_conc"          "mpox_nvo_avg_conc_lin"     
#> [31] "mpox_nvo_flowpop_lin"       "mpox_nvo_mic_lin"          
#> [33] "rsv_avg_conc"               "rsv_avg_conc_lin"          
#> [35] "rsv_flowpop_lin"            "rsv_mic_lin"
meta_nwss$nwss$geo_types
#> [1] "sewershed"
meta_nwss$nwss$version_range
#> NULL
meta_nwss$nwss$time_value_range
#> NULL
nwss_data <- epidata_snapshot(
  source = "nwss",
  signal = "covid_avg_conc",
  geo_type = "sewershed"
)
head(nwss_data)
#> # A tibble: 6 × 10
#>   signal   report_time geo_type geo_value fill_method reference_time nwss_source
#>   <chr>    <date>      <chr>    <chr>     <chr>       <date>         <chr>      
#> 1 covid_a… 2026-04-03  sewersh… 1364      source      2024-10-15     State_Terr…
#> 2 covid_a… 2026-04-03  sewersh… 200       source      2022-10-13     State_Terr…
#> 3 covid_a… 2026-04-03  sewersh… 360       source      2022-07-12     CDC_Biobot 
#> 4 covid_a… 2026-04-03  sewersh… 715       source      2023-05-09     State_Terr…
#> 5 covid_a… 2026-04-03  sewersh… 518       source      2022-12-19     CDC_Biobot 
#> 6 covid_a… 2026-04-03  sewersh… 1266      source      2025-02-19     State_Terr…
#> # ℹ 3 more variables: sample_index <chr>, pcr_target <chr>, value <dbl>
```
