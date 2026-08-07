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
#> [1] "census_division" "census_region"   "county"          "hhs"            
#> [5] "hrr"             "hsa_nci"         "msa"             "nation"         
#> [9] "state"
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
#>   signal         report_time geo_type geo_value fill_method reference_time value
#>   <chr>          <date>      <chr>    <chr>     <chr>       <date>         <dbl>
#> 1 pct_ed_visits… 2026-06-26  state    ak        source      2022-10-01     0.140
#> 2 pct_ed_visits… 2026-06-26  state    ak        source      2022-10-08     0.240
#> 3 pct_ed_visits… 2026-06-26  state    ak        source      2022-10-15     0.320
#> 4 pct_ed_visits… 2026-06-26  state    ak        source      2022-10-22     0.760
#> 5 pct_ed_visits… 2026-06-26  state    ak        source      2022-10-29     1.16 
#> 6 pct_ed_visits… 2026-06-26  state    ak        source      2022-11-05     1.94
```

If you want to inspect the API request URL or query structure without
actually fetching the data, you can use the `dry_run` argument via
[`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md):

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
#> 1 pct_ed_visits… 2024-12-27  state    ca        source      2022-10-01     0.210
#> 2 pct_ed_visits… 2024-12-27  state    ca        source      2022-10-08     0.280
#> 3 pct_ed_visits… 2024-12-27  state    ca        source      2022-10-15     0.450
#> 4 pct_ed_visits… 2024-12-27  state    ca        source      2022-10-22     0.680
#> 5 pct_ed_visits… 2024-12-27  state    ca        source      2022-10-29     1.04 
#> 6 pct_ed_visits… 2024-12-27  state    ca        source      2022-11-05     1.88
```

### Archive Queries

If you want to track how data for a specific time period was revised
over time, you can use
[`epidata_archive()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md).

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
#> 5 pct_ed_visits… 2024-04-18  state    ak        source      2022-10-29     1.17 
#> 6 pct_ed_visits… 2024-04-18  state    ak        source      2022-11-05     1.95
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
#> [1] "census_division" "census_region"   "hhs"             "nation"         
#> [5] "state"
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
#> 1 confirmed_adm… 2026-06-26  state    ak        source      2026-06-20         3
#> 2 confirmed_adm… 2026-06-26  state    ak        source      2026-06-13         0
#> 3 confirmed_adm… 2026-06-26  state    ak        source      2026-06-06         3
#> 4 confirmed_adm… 2026-06-26  state    ak        source      2026-05-30         2
#> 5 confirmed_adm… 2026-06-26  state    ak        source      2026-05-23         0
#> 6 confirmed_adm… 2026-06-26  state    ak        source      2026-05-16         0

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
#> 1 covid_pct… 2026-07-31  state    ak        source      2018-01-13     <1       
#> 2 covid_pct… 2026-07-31  state    ak        source      2018-01-13     1-4      
#> 3 covid_pct… 2026-07-31  state    ak        source      2018-01-13     18-49    
#> 4 covid_pct… 2026-07-31  state    ak        source      2018-01-13     50-64    
#> 5 covid_pct… 2026-07-31  state    ak        source      2018-01-13     5-17     
#> 6 covid_pct… 2026-07-31  state    ak        source      2018-01-13     65+      
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
#> 1 covid_a… 2026-06-26  sewersh… 1591      source      2022-05-15     State_Terr…
#> 2 covid_a… 2026-06-26  sewersh… 148       source      2021-04-12     State_Terr…
#> 3 covid_a… 2026-06-26  sewersh… 496       source      2025-02-12     State_Terr…
#> 4 covid_a… 2026-06-26  sewersh… 1384      source      2023-05-02     CDC_Biobot 
#> 5 covid_a… 2026-06-26  sewersh… 1152      source      2023-04-11     State_Terr…
#> 6 covid_a… 2026-06-26  sewersh… 71        source      2024-03-06     CDC_Verily 
#> # ℹ 3 more variables: sample_index <chr>, pcr_target <chr>, value <dbl>
```

### Auxiliary Data

Some sources ship extra columns connected to the signal data, such as
the population served by each NWSS sewershed.
[`epidata_aux()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_aux.md)
retrieves it, either on its own or merged onto a signal pull.

You can pull auxiliary data directly by source. Two things control
download size and speed by shrinking the returned data. Pass named
filters on the key columns through `...` to filter to fewer rows (each
key accepts one or more values), and use `columns` to choose which
columns are returned.

``` r

aux_data <- epidata_aux(
  source = "nwss",
  pcr_target = "sars-cov-2",
  sample_index = c("92012", "92013")
)
head(aux_data)
#> # A tibble: 6 × 36
#>   report_time geo_value reference_time nwss_source sample_index pcr_target
#>   <date>      <chr>     <date>         <chr>       <chr>        <chr>     
#> 1 2026-06-26  162       2026-01-27     CDC_Verily  92012        sars-cov-2
#> 2 2026-06-19  162       2026-01-27     CDC_Verily  92012        sars-cov-2
#> 3 2026-06-12  162       2026-01-27     CDC_Verily  92012        sars-cov-2
#> 4 2026-06-05  162       2026-01-27     CDC_Verily  92012        sars-cov-2
#> 5 2026-05-30  162       2026-01-27     CDC_Verily  92012        sars-cov-2
#> 6 2026-05-29  162       2026-01-27     CDC_Verily  92012        sars-cov-2
#> # ℹ 30 more variables: report_ts_nominal_end <chr>, state_territory <chr>,
#> #   county_fips <chr>, counties_served <chr>, population_served <chr>,
#> #   sample_type <chr>, sample_matrix <chr>, sample_location <chr>,
#> #   flow_rate <chr>, concentration_method <chr>, pasteurized <chr>,
#> #   pcr_type <chr>, extraction_method <chr>, major_lab_method <chr>,
#> #   inhibition_detect <chr>, inhibition_adjust <chr>, ntc_amplify <chr>,
#> #   pcr_gene_target_agg <chr>, pcr_target_units <chr>, lod_sewage <chr>, …
```

You may want the auxiliary columns attached to the original signal pull.
The output of
[`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md)
or `epidata_archive` can be passed directly to
[`epidata_aux()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_aux.md).
In this use case,
[`epidata_aux()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_aux.md)
uses the `source` parameter from the original signal pull function. It
fetches the matching auxiliary data and left-joins it onto the shared
key columns, keeping only the sample data from the original signal pull.

``` r

# Filter the base dataset to only one site, so the auxiliary
# pull stays small for this example
nwss_small <- nwss_data %>%
  dplyr::filter(geo_value == first(geo_value))

nwss_merged <- nwss_small %>%
  epidata_aux()
head(nwss_merged)
#> # A tibble: 6 × 40
#>   signal   report_time geo_type geo_value fill_method reference_time nwss_source
#>   <chr>    <date>      <chr>    <chr>     <chr>       <date>         <chr>      
#> 1 covid_a… 2026-06-26  sewersh… 1591      source      2022-05-15     State_Terr…
#> 2 covid_a… 2026-06-26  sewersh… 1591      source      2022-08-16     State_Terr…
#> 3 covid_a… 2026-06-26  sewersh… 1591      source      2022-07-31     State_Terr…
#> 4 covid_a… 2026-06-26  sewersh… 1591      source      2022-08-30     State_Terr…
#> 5 covid_a… 2026-06-26  sewersh… 1591      source      2022-08-21     State_Terr…
#> 6 covid_a… 2026-06-26  sewersh… 1591      source      2022-01-25     State_Terr…
#> # ℹ 33 more variables: sample_index <chr>, pcr_target <chr>, value <dbl>,
#> #   report_ts_nominal_end <chr>, state_territory <chr>, county_fips <chr>,
#> #   counties_served <chr>, population_served <chr>, sample_type <chr>,
#> #   sample_matrix <chr>, sample_location <chr>, flow_rate <chr>,
#> #   concentration_method <chr>, pasteurized <chr>, pcr_type <chr>,
#> #   extraction_method <chr>, major_lab_method <chr>, inhibition_detect <chr>,
#> #   inhibition_adjust <chr>, ntc_amplify <chr>, pcr_gene_target_agg <chr>, …
```

If you don’t pass any key filters,
[`epidata_aux()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_aux.md)
infers them from the base dataset. Any key column the base narrows to at
most 10 distinct values (like `geo_value`) is used to automatically
narrow the auxiliary request, so you download only the auxiliary rows
you need.

The same key filters work on
[`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md)
and
[`epidata_archive()`](https://cmu-delphi.github.io/epidatr/dev/reference/cast_api_queries.md),
where they are sent server-side to shrink the download:

``` r

epidata_snapshot(
  source = "nwss",
  signals = "pcr_conc_smoothed",
  geo_type = "county",
  pcr_target = c("sars-cov-2", "influenza")
)
```
