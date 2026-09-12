# fixture parses: classic-covidcast.json

    Code
      print(vapply(result, function(col) paste(class(col), collapse = "/"), character(
        1)))
    Output
                geo_value              signal              source            geo_type 
              "character"         "character"         "character"            "factor" 
                time_type          time_value           direction               issue 
                 "factor"              "Date"           "numeric"              "Date" 
                      lag       missing_value      missing_stderr missing_sample_size 
                "numeric"           "numeric"           "numeric"           "numeric" 
                    value              stderr         sample_size 
                "numeric"           "numeric"           "numeric" 
    Code
      print(head(as.data.frame(result), 3))
    Output
        geo_value                        signal   source geo_type time_type
      1        ca confirmed_7dav_incidence_prop jhu-csse    state       day
      2        ca confirmed_7dav_incidence_prop jhu-csse    state       day
      3        ca confirmed_7dav_incidence_prop jhu-csse    state       day
        time_value direction      issue  lag missing_value missing_stderr
      1 2020-06-01        NA 2023-03-10 1012             0              5
      2 2020-06-02        NA 2023-03-10 1011             0              5
      3 2020-06-03        NA 2023-03-10 1010             0              5
        missing_sample_size    value stderr sample_size
      1                   5 6.843108     NA          NA
      2                   5 6.825690     NA          NA
      3                   5 6.664936     NA          NA

# fixture parses: classic-fluview.json

    Code
      print(vapply(result, function(col) paste(class(col), collapse = "/"), character(
        1)))
    Output
       release_date        region         issue       epiweek           lag 
             "Date"   "character"        "Date"        "Date"     "numeric" 
            num_ili  num_patients num_providers     num_age_0     num_age_1 
          "numeric"     "numeric"     "numeric"     "numeric"     "numeric" 
          num_age_2     num_age_3     num_age_4     num_age_5          wili 
          "numeric"     "numeric"     "numeric"     "numeric"     "numeric" 
                ili 
          "numeric" 
    Code
      print(head(as.data.frame(result), 3))
    Output
        release_date region      issue    epiweek lag num_ili num_patients
      1   2021-10-08    nat 2021-09-26 2019-12-29  91   88731      1426691
      2   2021-10-08    nat 2021-09-26 2020-01-05  90   75614      1492251
      3   2021-10-08    nat 2021-09-26 2020-01-12  89   79783      1489132
        num_providers num_age_0 num_age_1 num_age_2 num_age_3 num_age_4 num_age_5
      1          2970     21594     23392        NA     27655      9209      6881
      2          3002     15564     22756        NA     23634      8196      5464
      3          2995     16587     29668        NA     21413      7386      4729
           wili     ili
      1 5.90066 6.21936
      2 4.94020 5.06711
      3 5.33135 5.35768

# fixture parses: classic-delphi.json

    Code
      str(result, max.level = 3)
    Output
      List of 1
       $ :List of 3
        ..$ epiweek : int 201501
        ..$ forecast:List of 10
        .. ..$ _version    : int 1
        .. ..$ baselines   :List of 11
        .. ..$ data        :List of 11
        .. ..$ epiweek     : int 201501
        .. ..$ ili_bin_size: int 1
        .. ..$ ili_bins    : int 11
        .. ..$ name        : chr "DELPHI-Epicast-(Carnegie-Mellon-University)"
        .. ..$ season      : int 2014
        .. ..$ season_weeks: int 34
        .. ..$ year_weeks  : int 53
        ..$ system  : chr "ec"

# fixture parses: cast-snapshot.csv

    Code
      print(vapply(result, function(col) paste(class(col), collapse = "/"), character(
        1)))
    Output
              signal    report_time       geo_type      geo_value    fill_method 
         "character"         "Date"    "character"    "character"    "character" 
      reference_time          value 
              "Date"      "numeric" 
    Code
      print(head(as.data.frame(result), 3))
    Output
                         signal report_time geo_type geo_value fill_method
      1 pct_ed_visits_influenza  2024-12-27   nation        us      source
      2 pct_ed_visits_influenza  2024-12-27   nation        us      source
      3 pct_ed_visits_influenza  2024-12-27   nation        us      source
        reference_time value
      1     2022-10-01  0.48
      2     2022-10-08  0.67
      3     2022-10-15  0.90

# fixture parses: cast-archive.csv

    Code
      print(vapply(result, function(col) paste(class(col), collapse = "/"), character(
        1)))
    Output
              signal    report_time       geo_type      geo_value    fill_method 
         "character"         "Date"    "character"    "character"    "character" 
      reference_time          value 
              "Date"      "numeric" 
    Code
      print(head(as.data.frame(result), 3))
    Output
                         signal report_time geo_type geo_value fill_method
      1 pct_ed_visits_influenza  2024-12-27   nation        us      source
      2 pct_ed_visits_influenza  2024-12-27   nation        us      source
      3 pct_ed_visits_influenza  2024-12-27   nation        us      source
        reference_time value
      1     2022-10-01  0.48
      2     2022-10-08  0.67
      3     2022-10-15  0.90

# fixture parses: cast-meta.json

    Code
      str(result, max.level = 3)
    Output
      List of 8
       $ report_time_range   :List of 2
        ..$ latest: chr "2026-08-19T00:00:00"
        ..$ first : chr "2024-04-18T00:00:00"
       $ reference_time_range:List of 2
        ..$ latest: chr "2026-08-15"
        ..$ first : chr "2022-10-01"
       $ signals             : chr [1:9] "pct_ed_visits_ari" "pct_ed_visits_combined" "pct_ed_visits_covid" "pct_ed_visits_influenza" ...
       $ geo_types           : chr [1:9] "census_division" "census_region" "county" "hhs" ...
       $ key_columns         : chr [1:6] "signal" "report_time" "geo_type" "geo_value" ...
       $ extra_key_columns   : list()
       $ value_columns       : chr "value"
       $ column_types        :List of 7
        ..$ report_time   : chr "timestamp without time zone"
        ..$ signal        : chr "text"
        ..$ geo_type      : chr "text"
        ..$ geo_value     : chr "text"
        ..$ fill_method   : chr "text"
        ..$ reference_time: chr "date"
        ..$ value         : chr "double precision"

# fixture parses: aux-data.csv

    Code
      print(vapply(result, function(col) paste(class(col), collapse = "/"), character(
        1)))
    Output
                report_time             geo_value        reference_time 
                     "Date"           "character"                "Date" 
                nwss_source          sample_index            pcr_target 
                "character"           "character"           "character" 
      report_ts_nominal_end       state_territory           county_fips 
                "character"           "character"           "character" 
            counties_served     population_served           sample_type 
                "character"           "character"           "character" 
              sample_matrix       sample_location             flow_rate 
                "character"           "character"           "character" 
       concentration_method           pasteurized              pcr_type 
                "character"           "character"           "character" 
          extraction_method      major_lab_method     inhibition_detect 
                "character"           "character"           "character" 
          inhibition_adjust           ntc_amplify   pcr_gene_target_agg 
                "character"           "character"           "character" 
           pcr_target_units            lod_sewage   hum_frac_target_mic 
                "character"           "character"           "character" 
          hum_frac_mic_conc     hum_frac_mic_unit       rec_eff_percent 
                "character"           "character"           "character" 
        rec_eff_target_name  rec_eff_spike_matrix    rec_eff_spike_conc 
                "character"           "character"           "character" 
            pipeline_run_id      report_ts_actual              comments 
                "character"           "character"           "character" 
    Code
      print(head(as.data.frame(result), 3))
    Output
        report_time geo_value reference_time nwss_source sample_index pcr_target
      1  2026-06-26        10     2022-12-13  CDC_Biobot      5639533        nvo
      2  2026-06-19        10     2022-12-13  CDC_Biobot      5639533        nvo
      3  2026-06-12        10     2022-12-13  CDC_Biobot      5639533        nvo
        report_ts_nominal_end state_territory county_fips counties_served
      1                  <NA>              al       01095        Marshall
      2   2026-06-26 00:00:00              al       01095        Marshall
      3   2026-06-19 00:00:00              al       01095        Marshall
        population_served                   sample_type  sample_matrix
      1              9000 24-hr time-weighted composite raw wastewater
      2              9000 24-hr time-weighted composite raw wastewater
      3              9000 24-hr time-weighted composite raw wastewater
        sample_location flow_rate concentration_method pasteurized pcr_type
      1            wwtp      4.14       ceres nanotrap           t     qpcr
      2            wwtp      4.14       ceres nanotrap           t     qpcr
      3            wwtp      4.14       ceres nanotrap           t     qpcr
                                                extraction_method major_lab_method
      1 thermo magmax microbiome ultra nucleic acid isolation kit                4
      2 thermo magmax microbiome ultra nucleic acid isolation kit                4
      3 thermo magmax microbiome ultra nucleic acid isolation kit                4
        inhibition_detect inhibition_adjust ntc_amplify pcr_gene_target_agg
      1                 f                 f           f            e9l-nvar
      2                 f                 f           f            e9l-nvar
      3                 f                 f           f            e9l-nvar
           pcr_target_units lod_sewage      hum_frac_target_mic hum_frac_mic_conc
      1 copies/l wastewater       1150 pepper mild mottle virus    17722821.72669
      2 copies/l wastewater       1150 pepper mild mottle virus    17722821.72669
      3 copies/l wastewater       1150 pepper mild mottle virus    17722821.72669
          hum_frac_mic_unit rec_eff_percent rec_eff_target_name
      1 copies/l wastewater        49.83329        brsv vaccine
      2 copies/l wastewater        49.83329        brsv vaccine
      3 copies/l wastewater        49.83329        brsv vaccine
                  rec_eff_spike_matrix rec_eff_spike_conc pipeline_run_id
      1 raw sample post pasteurization            5.08798            7961
      2 raw sample post pasteurization            5.08798            7904
      3 raw sample post pasteurization            5.08798            7886
           report_ts_actual comments
      1 2026-06-26 21:12:39     <NA>
      2 2026-06-26 21:03:00     <NA>
      3 2026-06-26 21:01:20     <NA>

