# endpoint request URLs are stable

    Code
      cat(lines, sep = "\n")
    Output
      pvt_cdc
        https://api.delphi.cmu.edu/epidata/cdc/?auth=test-auth-key&locations=fl%2Cca&epiweeks=201501-201601
      pvt_cdc wildcard
        https://api.delphi.cmu.edu/epidata/cdc/?auth=test-auth-key&locations=fl%2Cca&epiweeks=100001-300001
      pub_covid_hosp_facility_lookup
        https://api.delphi.cmu.edu/epidata/covid_hosp_facility_lookup/?state=fl
      pub_covid_hosp_facility
        https://api.delphi.cmu.edu/epidata/covid_hosp_facility/?hospital_pks=100075&collection_weeks=20200101-20200501
      pub_covid_hosp_facility wildcard
        https://api.delphi.cmu.edu/epidata/covid_hosp_facility/?hospital_pks=100075&collection_weeks=10000101-30000101
      pub_covid_hosp_state_timeseries
        https://api.delphi.cmu.edu/epidata/covid_hosp_state_timeseries/?states=fl&dates=20200101-20200501
      pub_covid_hosp_state_timeseries wildcard
        https://api.delphi.cmu.edu/epidata/covid_hosp_state_timeseries/?states=fl&dates=10000101-30000101
      pub_covidcast_meta
        https://api.delphi.cmu.edu/epidata/covidcast_meta/
      pub_covidcast
        https://api.delphi.cmu.edu/epidata/covidcast/?data_source=jhu-csse&signals=confirmed_7dav_incidence_prop&geo_type=state&time_type=day&geo_values=ca%2Cfl&time_values=20200601-20200801
      pub_covidcast wildcard
        https://api.delphi.cmu.edu/epidata/covidcast/?data_source=jhu-csse&signals=confirmed_7dav_incidence_prop&geo_type=state&time_type=day&geo_values=ca%2Cfl&time_values=%2A
      pub_delphi
        https://api.delphi.cmu.edu/epidata/delphi/?system=ec&epiweek=201501
      pub_dengue_nowcast
        https://api.delphi.cmu.edu/epidata/dengue_nowcast/?locations=pr&epiweeks=201401-202301
      pub_dengue_nowcast wildcard
        https://api.delphi.cmu.edu/epidata/dengue_nowcast/?locations=ca&epiweeks=100001-300001
      pvt_dengue_sensors
        https://api.delphi.cmu.edu/epidata/dengue_sensors/?auth=test-auth-key&names=ght&locations=ag&epiweeks=201501-202001
      pvt_dengue_sensors wildcard
        https://api.delphi.cmu.edu/epidata/dengue_sensors/?auth=test-auth-key&names=ght&locations=ag&epiweeks=100001-300001
      pub_ecdc_ili
        https://api.delphi.cmu.edu/epidata/ecdc_ili/?regions=austria&epiweeks=201901-202001
      pub_ecdc_ili wildcard
        https://api.delphi.cmu.edu/epidata/ecdc_ili/?regions=austria&epiweeks=100001-300001
      pub_flusurv
        https://api.delphi.cmu.edu/epidata/flusurv/?locations=ca&epiweeks=201701-201801
      pub_flusurv wildcard
        https://api.delphi.cmu.edu/epidata/flusurv/?locations=CA&epiweeks=100001-300001
      pub_fluview_clinical
        https://api.delphi.cmu.edu/epidata/fluview_clinical/?regions=nat&epiweeks=201601-201701
      pub_fluview_clinical wildcard
        https://api.delphi.cmu.edu/epidata/fluview_clinical/?regions=nat&epiweeks=100001-300001
      pub_fluview_meta
        https://api.delphi.cmu.edu/epidata/fluview_meta/
      pub_fluview
        https://api.delphi.cmu.edu/epidata/fluview/?regions=nat&epiweeks=201201-202005
      pub_fluview wildcard
        https://api.delphi.cmu.edu/epidata/fluview/?regions=nat&epiweeks=100001-300001
      pub_gft
        https://api.delphi.cmu.edu/epidata/gft/?locations=hhs1&epiweeks=201201-202001
      pub_gft wildcard
        https://api.delphi.cmu.edu/epidata/gft/?locations=hhs1&epiweeks=100001-300001
      pvt_ght
        https://api.delphi.cmu.edu/epidata/ght/?auth=test-auth-key&locations=ma&epiweeks=199301-202304&query=how%20to%20get%20over%20the%20flu
      pvt_ght wildcard
        https://api.delphi.cmu.edu/epidata/ght/?auth=test-auth-key&locations=ca&epiweeks=100001-300001&query=how%20to%20get%20over%20the%20flu
      pub_kcdc_ili
        https://api.delphi.cmu.edu/epidata/kcdc_ili/?regions=ROK&epiweeks=200436
      pub_kcdc_ili wildcard
        https://api.delphi.cmu.edu/epidata/kcdc_ili/?regions=ROK&epiweeks=100001-300001
      pvt_meta_norostat
        https://api.delphi.cmu.edu/epidata/meta_norostat/?auth=test-auth-key
      pub_meta
        https://api.delphi.cmu.edu/epidata/meta/
      pub_nidss_dengue
        https://api.delphi.cmu.edu/epidata/nidss_dengue/?locations=taipei&epiweeks=201201-201301
      pub_nidss_dengue wildcard
        https://api.delphi.cmu.edu/epidata/nidss_dengue/?locations=taipei&epiweeks=100001-300001
      pub_nidss_flu
        https://api.delphi.cmu.edu/epidata/nidss_flu/?regions=taipei&epiweeks=201501-201601
      pub_nidss_flu wildcard
        https://api.delphi.cmu.edu/epidata/nidss_flu/?regions=taipei&epiweeks=100001-300001
      pvt_norostat
        https://api.delphi.cmu.edu/epidata/norostat/?auth=test-auth-key&location=Minnesota%2C%20Ohio%2C%20Oregon%2C%20Tennessee%2C%20and%20Wisconsin&epiweeks=201233
      pvt_norostat wildcard
        https://api.delphi.cmu.edu/epidata/norostat/?auth=test-auth-key&location=Minnesota%2C%20Ohio%2C%20Oregon%2C%20Tennessee%2C%20and%20Wisconsin&epiweeks=100001-300001
      pub_nowcast
        https://api.delphi.cmu.edu/epidata/nowcast/?locations=ca&epiweeks=201201-201301
      pub_nowcast wildcard
        https://api.delphi.cmu.edu/epidata/nowcast/?locations=ca&epiweeks=100001-300001
      pub_paho_dengue
        https://api.delphi.cmu.edu/epidata/paho_dengue/?regions=ca&epiweeks=201401-201501
      pub_paho_dengue wildcard
        https://api.delphi.cmu.edu/epidata/paho_dengue/?regions=ca&epiweeks=100001-300001
      pvt_quidel
        https://api.delphi.cmu.edu/epidata/quidel/?auth=test-auth-key&locations=hhs1&epiweeks=201201-202001
      pvt_quidel wildcard
        https://api.delphi.cmu.edu/epidata/quidel/?auth=test-auth-key&locations=hhs1&epiweeks=100001-300001
      pvt_sensors
        https://api.delphi.cmu.edu/epidata/sensors/?auth=test-auth-key&names=sar3&locations=nat&epiweeks=201501-202001
      pvt_sensors wildcard
        https://api.delphi.cmu.edu/epidata/sensors/?auth=test-auth-key&names=sar3&locations=nat&epiweeks=100001-300001
      pvt_twitter week
        https://api.delphi.cmu.edu/epidata/twitter/?auth=test-auth-key&locations=CA&epiweeks=201501-202001
      pvt_twitter week wildcard
        https://api.delphi.cmu.edu/epidata/twitter/?auth=test-auth-key&locations=CA&epiweeks=100001-300001
      pvt_twitter day
        https://api.delphi.cmu.edu/epidata/twitter/?auth=test-auth-key&locations=CA&dates=20150101-20200101
      pvt_twitter day wildcard
        https://api.delphi.cmu.edu/epidata/twitter/?auth=test-auth-key&locations=CA&dates=10000101-30000101
      pub_wiki week
        https://api.delphi.cmu.edu/epidata/wiki/?articles=avian_influenza&epiweeks=201501-201601&language=en
      pub_wiki week wildcard
        https://api.delphi.cmu.edu/epidata/wiki/?articles=avian_influenza&epiweeks=100001-300001&language=en
      pub_wiki day
        https://api.delphi.cmu.edu/epidata/wiki/?articles=avian_influenza&dates=20150101-20200101&language=en
      pub_wiki day wildcard
        https://api.delphi.cmu.edu/epidata/wiki/?articles=avian_influenza&dates=10000101-30000101&language=en
      epidata_snapshot
        https://delphi.cmu.edu/epidata/v5/snapshot/?source=nssp&signal=pct_ed_visits_influenza&geo_type=state&snapshot_date=2025-01-01
      epidata_archive
        https://delphi.cmu.edu/epidata/v5/archive/?source=nssp&signal=pct_ed_visits_influenza&geo_type=state&report_time_query=%3C2025-06-01
      epidata_aux
        https://delphi.cmu.edu/epidata/v5/aux_data/?source=nwss&report_time_query=%3C2025-06-01&filtered_keys=pcr_target%3Asars-cov-2&columns=geo_value%2Cpopulation_served
      epidata dispatcher
        https://delphi.cmu.edu/epidata/v5/archive/?source=nssp&signal=pct_ed_visits_influenza&geo_type=state

