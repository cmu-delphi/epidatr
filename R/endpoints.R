#' fetch AFHSB data (point data, no min/max)
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/afhsb.html
#'
#' @param auth character authentication token (note that this is not the same as the API key)
#' @param locations character locations to fetch
#' @param epiweeks epirange epiweeks to fetch
#' @param flu_types character flu_types to fetch
#' @return an instance of epidata_call
#'
#' @examples
#' call <- pvt_afhsb(auth = "yourkey", "fl,ca", epirange(202001, 202110), "flu1,flu2-flu1")
#' # fetch_csv(call)
#' @export
pvt_afhsb <- function(auth, locations, epiweeks, flu_types) {
  check_single_string_param("auth", auth)
  check_string_param("locations", locations)
  check_epirange_param("epiweeks", epiweeks)
  check_string_param("flu_types", flu_types)

  create_epidata_call(
    "afhsb/",
    list(
      auth = auth,
      locations = locations,
      epiweeks = epiweeks,
      flu_types = flu_types
    ),
    list(
      create_epidata_field_info("location", "text"),
      create_epidata_field_info("flu_type", "text"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("visit_num", "int")
    )
  )
}

#' fetch CDC page hits
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/cdc.html
#'
#' @examples
#' call <- pvt_cdc(auth = "yourkey", epirange(20210101, 20210201), "fl,ca")
#' # fetch_csv(call)
#' @param auth character authentication token (note that this is not the same as the API key)
#' @param epiweeks epirange epiweeks to fetch
#' @param locations character locations to fetch
#' @return an instance of epidata_call
#'
#' @export
pvt_cdc <- function(auth, epiweeks, locations) {
  check_single_string_param("auth", auth)
  check_epirange_param("epiweeks", epiweeks)
  check_string_param("locations", locations)

  create_epidata_call(
    "cdc/",
    list(
      auth = auth,
      epiweeks = epiweeks,
      locations = locations
    ),
    list(
      create_epidata_field_info("location", "text"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("num1", "int"),
      create_epidata_field_info("num2", "int"),
      create_epidata_field_info("num3", "int"),
      create_epidata_field_info("num4", "int"),
      create_epidata_field_info("num5", "int"),
      create_epidata_field_info("num6", "int"),
      create_epidata_field_info("num7", "int"),
      create_epidata_field_info("num8", "int"),
      create_epidata_field_info("total", "int"),
      create_epidata_field_info("value", "float")
    )
  )
}

#' fetch COVID hospitalization facility identifiers
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/covid_hosp_facility_lookup.html
#'
#' @examples
#' \donttest{
#' # can take a few minutes; donttesting while backend performance is being improved
#' call <- covid_hosp_facility_lookup(state = "fl")
#' fetch_csv(call)
#' }
#' @param state optional character state
#' @param ccn optional character ccn
#' @param city optional character city
#' @param zip optional numeric zip code
#' @param fips_code optional numeric fips code
#' @return an instance of epidata_call
#'
#' @export
covid_hosp_facility_lookup <-
  function(state = NULL,
           ccn = NULL,
           city = NULL,
           zip = NULL,
           fips_code = NULL) {
    check_single_string_param("state", state, FALSE)
    check_single_string_param("ccn", ccn, FALSE)
    check_single_string_param("city", city, FALSE)
    check_single_string_param("zip", zip, FALSE)
    check_single_string_param("fips_code", fips_code, FALSE)
    if (missing(state) &&
      missing(ccn) &&
      missing(city) && missing(zip) && missing(fips_code)) {
      stop("one of `state`, `ccn`, `city`, `zip`, or `fips_code` is required")
    }
    create_epidata_call(
      "covid_hosp_facility_lookup/",
      list(
        state = state,
        ccn = ccn,
        city = city,
        zip = zip,
        fips_code = fips_code
      ),
      list(
        create_epidata_field_info("hospital_pk", "text"),
        create_epidata_field_info("state", "text"),
        create_epidata_field_info("ccn", "text"),
        create_epidata_field_info("hospital_name", "text"),
        create_epidata_field_info("address", "text"),
        create_epidata_field_info("city", "text"),
        create_epidata_field_info("zip", "text"),
        create_epidata_field_info("hospital_subtype", "text"),
        create_epidata_field_info("fips_code", "text"),
        create_epidata_field_info("is_metro_micro", "int")
      )
    )
  }

#' fetch COVID hospitalization data for specific facilities
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/covid_hosp_facility_lookup.html
#'
#' @examples
#' call <- covid_hosp_facility(hospital_pks = "100075", collection_weeks = epirange(202001, 202005))
#' fetch_csv(call)
#' @param hospital_pks character hospitals to fetch
#' @param collection_weeks epirange weeks to fetch
#' @param publication_dates optional epirange publication dates to fetch
#' @return an instance of epidata_call
#'
#' @export
#
covid_hosp_facility <-
  function(hospital_pks,
           collection_weeks,
           publication_dates = NULL) {
    check_string_param("hospital_pks", hospital_pks)
    check_epirange_param("collection_weeks", collection_weeks)
    check_epirange_param("publication_dates", publication_dates, required = FALSE)

    create_epidata_call(
      "covid_hosp_facility/",
      list(
        hospital_pks = hospital_pks,
        collection_weeks = collection_weeks,
        publication_dates = publication_dates
      ),
      list(
        create_epidata_field_info("hospital_pk", "text"),
        create_epidata_field_info("state", "text"),
        create_epidata_field_info("ccn", "text"),
        create_epidata_field_info("hospital_name", "text"),
        create_epidata_field_info("address", "text"),
        create_epidata_field_info("city", "text"),
        create_epidata_field_info("zip", "text"),
        create_epidata_field_info("hospital_subtype", "text"),
        create_epidata_field_info("fips_code", "text"),
        create_epidata_field_info("publication_date", "date"),
        create_epidata_field_info("collection_week", "date"),
        create_epidata_field_info("is_metro_micro", "bool"),
        create_epidata_field_info("total_beds_7_day_sum", "int"),
        create_epidata_field_info("all_adult_hospital_beds_7_day_sum", "int"),
        create_epidata_field_info("all_adult_hospital_inpatient_beds_7_day_sum", "int"),
        create_epidata_field_info("inpatient_beds_used_7_day_sum", "int"),
        create_epidata_field_info(
          "all_adult_hospital_inpatient_bed_occupied_7_day_sum",
          "int"
        ),
        create_epidata_field_info(
          "total_adult_patients_hosp_confirmed_suspected_covid_7d_sum",
          "int"
        ),
        create_epidata_field_info(
          "total_adult_patients_hospitalized_confirmed_covid_7_day_sum",
          "int"
        ),
        create_epidata_field_info(
          "total_pediatric_patients_hosp_confirmed_suspected_covid_7d_sum",
          "int"
        ),
        create_epidata_field_info(
          "total_pediatric_patients_hospitalized_confirmed_covid_7_day_sum",
          "int"
        ),
        create_epidata_field_info("inpatient_beds_7_day_sum", "int"),
        create_epidata_field_info("total_icu_beds_7_day_sum", "int"),
        create_epidata_field_info("total_staffed_adult_icu_beds_7_day_sum", "int"),
        create_epidata_field_info("icu_beds_used_7_day_sum", "int"),
        create_epidata_field_info("staffed_adult_icu_bed_occupancy_7_day_sum", "int"),
        create_epidata_field_info(
          "staffed_icu_adult_patients_confirmed_suspected_covid_7d_sum",
          "int"
        ),
        create_epidata_field_info(
          "staffed_icu_adult_patients_confirmed_covid_7_day_sum",
          "int"
        ),
        create_epidata_field_info(
          "total_patients_hospitalized_confirmed_influenza_7_day_sum",
          "int"
        ),
        create_epidata_field_info("icu_patients_confirmed_influenza_7_day_sum", "int"),
        create_epidata_field_info(
          "total_patients_hosp_confirmed_influenza_and_covid_7d_sum",
          "int"
        ),
        create_epidata_field_info("total_beds_7_day_coverage", "int"),
        create_epidata_field_info("all_adult_hospital_beds_7_day_coverage", "int"),
        create_epidata_field_info("all_adult_hospital_inpatient_beds_7_day_coverage", "int"),
        create_epidata_field_info("inpatient_beds_used_7_day_coverage", "int"),
        create_epidata_field_info(
          "all_adult_hospital_inpatient_bed_occupied_7_day_coverage",
          "int"
        ),
        create_epidata_field_info(
          "total_adult_patients_hosp_confirmed_suspected_covid_7d_cov",
          "int"
        ),
        create_epidata_field_info(
          "total_adult_patients_hospitalized_confirmed_covid_7_day_coverage",
          "int"
        ),
        create_epidata_field_info(
          "total_pediatric_patients_hosp_confirmed_suspected_covid_7d_cov",
          "int"
        ),
        create_epidata_field_info(
          "total_pediatric_patients_hosp_confirmed_covid_7d_cov",
          "int"
        ),
        create_epidata_field_info("inpatient_beds_7_day_coverage", "int"),
        create_epidata_field_info("total_icu_beds_7_day_coverage", "int"),
        create_epidata_field_info("total_staffed_adult_icu_beds_7_day_coverage", "int"),
        create_epidata_field_info("icu_beds_used_7_day_coverage", "int"),
        create_epidata_field_info("staffed_adult_icu_bed_occupancy_7_day_coverage", "int"),
        create_epidata_field_info(
          "staffed_icu_adult_patients_confirmed_suspected_covid_7d_cov",
          "int"
        ),
        create_epidata_field_info(
          "staffed_icu_adult_patients_confirmed_covid_7_day_coverage",
          "int"
        ),
        create_epidata_field_info(
          "total_patients_hospitalized_confirmed_influenza_7_day_coverage",
          "int"
        ),
        create_epidata_field_info("icu_patients_confirmed_influenza_7_day_coverage", "int"),
        create_epidata_field_info(
          "total_patients_hosp_confirmed_influenza_and_covid_7d_cov",
          "int"
        ),
        create_epidata_field_info(
          "previous_day_admission_adult_covid_confirmed_7_day_sum",
          "int"
        ),
        create_epidata_field_info(
          "previous_day_admission_adult_covid_confirmed_18_19_7_day_sum",
          "int"
        ),
        create_epidata_field_info(
          "previous_day_admission_adult_covid_confirmed_20_29_7_day_sum",
          "int"
        ),
        create_epidata_field_info(
          "previous_day_admission_adult_covid_confirmed_30_39_7_day_sum",
          "int"
        ),
        create_epidata_field_info(
          "previous_day_admission_adult_covid_confirmed_40_49_7_day_sum",
          "int"
        ),
        create_epidata_field_info(
          "previous_day_admission_adult_covid_confirmed_50_59_7_day_sum",
          "int"
        ),
        create_epidata_field_info(
          "previous_day_admission_adult_covid_confirmed_60_69_7_day_sum",
          "int"
        ),
        create_epidata_field_info(
          "previous_day_admission_adult_covid_confirmed_70_79_7_day_sum",
          "int"
        ),
        create_epidata_field_info(
          "previous_day_admission_adult_covid_confirmed_80plus_7_day_sum",
          "int"
        ),
        create_epidata_field_info(
          "previous_day_admission_adult_covid_confirmed_unknown_7_day_sum",
          "int"
        ),
        create_epidata_field_info(
          "previous_day_admission_pediatric_covid_confirmed_7_day_sum",
          "int"
        ),
        create_epidata_field_info("previous_day_covid_ed_visits_7_day_sum", "int"),
        create_epidata_field_info(
          "previous_day_admission_adult_covid_suspected_7_day_sum",
          "int"
        ),
        create_epidata_field_info(
          "previous_day_admission_adult_covid_suspected_18_19_7_day_sum",
          "int"
        ),
        create_epidata_field_info(
          "previous_day_admission_adult_covid_suspected_20_29_7_day_sum",
          "int"
        ),
        create_epidata_field_info(
          "previous_day_admission_adult_covid_suspected_30_39_7_day_sum",
          "int"
        ),
        create_epidata_field_info(
          "previous_day_admission_adult_covid_suspected_40_49_7_day_sum",
          "int"
        ),
        create_epidata_field_info(
          "previous_day_admission_adult_covid_suspected_50_59_7_day_sum",
          "int"
        ),
        create_epidata_field_info(
          "previous_day_admission_adult_covid_suspected_60_69_7_day_sum",
          "int"
        ),
        create_epidata_field_info(
          "previous_day_admission_adult_covid_suspected_70_79_7_day_sum",
          "int"
        ),
        create_epidata_field_info(
          "previous_day_admission_adult_covid_suspected_80plus_7_day_sum",
          "int"
        ),
        create_epidata_field_info(
          "previous_day_admission_adult_covid_suspected_unknown_7_day_sum",
          "int"
        ),
        create_epidata_field_info(
          "previous_day_admission_pediatric_covid_suspected_7_day_sum",
          "int"
        ),
        create_epidata_field_info("previous_day_total_ed_visits_7_day_sum", "int"),
        create_epidata_field_info(
          "previous_day_admission_influenza_confirmed_7_day_sum",
          "int"
        ),
        create_epidata_field_info("total_beds_7_day_avg", "float"),
        create_epidata_field_info("all_adult_hospital_beds_7_day_avg", "float"),
        create_epidata_field_info("all_adult_hospital_inpatient_beds_7_day_avg", "float"),
        create_epidata_field_info("inpatient_beds_used_7_day_avg", "float"),
        create_epidata_field_info(
          "all_adult_hospital_inpatient_bed_occupied_7_day_avg",
          "float"
        ),
        create_epidata_field_info(
          "total_adult_patients_hosp_confirmed_suspected_covid_7d_avg",
          "float"
        ),
        create_epidata_field_info(
          "total_adult_patients_hospitalized_confirmed_covid_7_day_avg",
          "float"
        ),
        create_epidata_field_info(
          "total_pediatric_patients_hosp_confirmed_suspected_covid_7d_avg",
          "float"
        ),
        create_epidata_field_info(
          "total_pediatric_patients_hospitalized_confirmed_covid_7_day_avg",
          "float"
        ),
        create_epidata_field_info("inpatient_beds_7_day_avg", "float"),
        create_epidata_field_info("total_icu_beds_7_day_avg", "float"),
        create_epidata_field_info("total_staffed_adult_icu_beds_7_day_avg", "float"),
        create_epidata_field_info("icu_beds_used_7_day_avg", "float"),
        create_epidata_field_info("staffed_adult_icu_bed_occupancy_7_day_avg", "float"),
        create_epidata_field_info(
          "staffed_icu_adult_patients_confirmed_suspected_covid_7d_avg",
          "float"
        ),
        create_epidata_field_info(
          "staffed_icu_adult_patients_confirmed_covid_7_day_avg",
          "float"
        ),
        create_epidata_field_info(
          "total_patients_hospitalized_confirmed_influenza_7_day_avg",
          "float"
        ),
        create_epidata_field_info("icu_patients_confirmed_influenza_7_day_avg", "float"),
        create_epidata_field_info(
          "total_patients_hosp_confirmed_influenza_and_covid_7d_avg",
          "float"
        )
      )
    )
  }

#' fetch COVID hospitalization data
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/covid_hosp.html
#'
#' @examples
#' call <- covid_hosp_state_timeseries(states = "fl", dates = epirange(20200101, 20200501))
#' fetch_csv(call)
#' @param states states to fetch
#' @param dates dates to fetch
#' @param issues issues to fetch
#' @return an instance of epidata_call
#'
#' @export
#
covid_hosp_state_timeseries <-
  function(states, dates, issues = NULL) {
    check_string_param("states", states)
    check_epirange_param("dates", dates)
    check_epirange_param("issues", issues, FALSE)

    create_epidata_call(
      "covid_hosp_state_timeseries/",
      list(
        states = states,
        dates = dates,
        issues = issues
      ),
      list(
        create_epidata_field_info("state", "text"),
        create_epidata_field_info("issue", "date"),
        create_epidata_field_info("date", "date"),
        create_epidata_field_info("issue", "date"),
        create_epidata_field_info("critical_staffing_shortage_today_yes", "bool"),
        create_epidata_field_info("critical_staffing_shortage_today_no", "bool"),
        create_epidata_field_info("critical_staffing_shortage_today_not_reported", "bool"),
        create_epidata_field_info(
          "critical_staffing_shortage_anticipated_within_week_yes",
          "bool"
        ),
        create_epidata_field_info(
          "critical_staffing_shortage_anticipated_within_week_no",
          "bool"
        ),
        create_epidata_field_info(
          "critical_staffing_shortage_anticipated_within_week_not_reported",
          "bool"
        ),
        create_epidata_field_info("hospital_onset_covid", "int"),
        create_epidata_field_info("hospital_onset_covid_coverage", "int"),
        create_epidata_field_info("inpatient_beds", "int"),
        create_epidata_field_info("inpatient_beds_coverage", "int"),
        create_epidata_field_info("inpatient_beds_used", "int"),
        create_epidata_field_info("inpatient_beds_used_coverage", "int"),
        create_epidata_field_info("inpatient_beds_used_covid", "int"),
        create_epidata_field_info("inpatient_beds_used_covid_coverage", "int"),
        create_epidata_field_info("previous_day_admission_adult_covid_confirmed", "int"),
        create_epidata_field_info(
          "previous_day_admission_adult_covid_confirmed_coverage",
          "int"
        ),
        create_epidata_field_info("previous_day_admission_adult_covid_suspected", "int"),
        create_epidata_field_info(
          "previous_day_admission_adult_covid_suspected_coverage",
          "int"
        ),
        create_epidata_field_info("previous_day_admission_pediatric_covid_confirmed", "int"),
        create_epidata_field_info(
          "previous_day_admission_pediatric_covid_confirmed_coverage",
          "int"
        ),
        create_epidata_field_info("previous_day_admission_pediatric_covid_suspected", "int"),
        create_epidata_field_info(
          "previous_day_admission_pediatric_covid_suspected_coverage",
          "int"
        ),
        create_epidata_field_info("staffed_adult_icu_bed_occupancy", "int"),
        create_epidata_field_info("staffed_adult_icu_bed_occupancy_coverage", "int"),
        create_epidata_field_info(
          "staffed_icu_adult_patients_confirmed_suspected_covid",
          "int"
        ),
        create_epidata_field_info(
          "staffed_icu_adult_patients_confirmed_suspected_covid_coverage",
          "int"
        ),
        create_epidata_field_info("staffed_icu_adult_patients_confirmed_covid", "int"),
        create_epidata_field_info(
          "staffed_icu_adult_patients_confirmed_covid_coverage",
          "int"
        ),
        create_epidata_field_info(
          "total_adult_patients_hosp_confirmed_suspected_covid",
          "int"
        ),
        create_epidata_field_info(
          "total_adult_patients_hosp_confirmed_suspected_covid_coverage",
          "int"
        ),
        create_epidata_field_info("total_adult_patients_hosp_confirmed_covid", "int"),
        create_epidata_field_info(
          "total_adult_patients_hosp_confirmed_covid_coverage",
          "int"
        ),
        create_epidata_field_info(
          "total_pediatric_patients_hosp_confirmed_suspected_covid",
          "int"
        ),
        create_epidata_field_info(
          "total_pediatric_patients_hosp_confirmed_suspected_covid_coverage",
          "int"
        ),
        create_epidata_field_info("total_pediatric_patients_hosp_confirmed_covid", "int"),
        create_epidata_field_info(
          "total_pediatric_patients_hosp_confirmed_covid_coverage",
          "int"
        ),
        create_epidata_field_info("total_staffed_adult_icu_beds", "int"),
        create_epidata_field_info("total_staffed_adult_icu_beds_coverage", "int"),
        create_epidata_field_info("inpatient_beds_utilization_coverage", "int"),
        create_epidata_field_info("inpatient_beds_utilization_numerator", "int"),
        create_epidata_field_info("inpatient_beds_utilization_denominator", "int"),
        create_epidata_field_info("percent_of_inpatients_with_covid_coverage", "int"),
        create_epidata_field_info("percent_of_inpatients_with_covid_numerator", "int"),
        create_epidata_field_info("percent_of_inpatients_with_covid_denominator", "int"),
        create_epidata_field_info("inpatient_bed_covid_utilization_coverage", "int"),
        create_epidata_field_info("inpatient_bed_covid_utilization_numerator", "int"),
        create_epidata_field_info("inpatient_bed_covid_utilization_denominator", "int"),
        create_epidata_field_info("adult_icu_bed_covid_utilization_coverage", "int"),
        create_epidata_field_info("adult_icu_bed_covid_utilization_numerator", "int"),
        create_epidata_field_info("adult_icu_bed_covid_utilization_denominator", "int"),
        create_epidata_field_info("adult_icu_bed_utilization_coverage", "int"),
        create_epidata_field_info("adult_icu_bed_utilization_numerator", "int"),
        create_epidata_field_info("adult_icu_bed_utilization_denominator", "int"),
        create_epidata_field_info("inpatient_beds_utilization", "float"),
        create_epidata_field_info("percent_of_inpatients_with_covid", "float"),
        create_epidata_field_info("inpatient_bed_covid_utilization", "float"),
        create_epidata_field_info("adult_icu_bed_covid_utilization", "float"),
        create_epidata_field_info("adult_icu_bed_utilization", "float")
      )
    )
  }

#' fetch covidcast meta data
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/covidcast_meta.html
#'
#' @return an instance of epidata_call
#'
#' @export
covidcast_meta <- function() {
  create_epidata_call(
    "covidcast_meta/",
    list(),
    list(
      create_epidata_field_info("data_source", "text"),
      create_epidata_field_info("signal", "text"),
      create_epidata_field_info("time_type", "categorical",
        categories =
          c("week", "day")
      ),
      create_epidata_field_info("min_time", "date"),
      create_epidata_field_info("max_time", "date"),
      create_epidata_field_info("num_locations", "int"),
      create_epidata_field_info("min_value", "float"),
      create_epidata_field_info("max_value", "float"),
      create_epidata_field_info("mean_value", "float"),
      create_epidata_field_info("stdev_value", "float"),
      create_epidata_field_info("last_update", "int"),
      create_epidata_field_info("max_issue", "date"),
      create_epidata_field_info("min_lag", "int"),
      create_epidata_field_info("max_lag", "int")
    )
  )
}


#' fetch covidcast_nowcast data
#'
#' Not an active endpoint yet.
#'
#' @param data_source data source to fetch
#' @param signals data source to fetch
#' @param sensor_names sensor names to fetch
#' @param time_type data source to fetch
#' @param time_values data source to fetch
#' @param geo_type geo_type to fetch
#' @param geo_values data source to fetch
#' @param as_of data source to fetch
#' @param issues data source to fetch
#' @param lag data source to fetch
#' @return an instance of epidata_call
#'
#' @export
covidcast_nowcast <-
  function(data_source,
           signals,
           sensor_names,
           time_type,
           geo_type,
           time_values,
           geo_values,
           as_of = NULL,
           issues = NULL,
           lag = NULL) {
    # Check parameters
    if (missing(data_source) ||
      missing(signals) ||
      missing(sensor_names) ||
      missing(time_type) ||
      missing(geo_type) ||
      missing(time_values) || missing(geo_values)) {
      stop(
        paste0(
          "`data_source`, `signals`, `sensor_names`, `time_type`, `geo_type`, `time_values`, ",
          "and `geo_value` are all required"
        )
      )
    }
    if (!missing(issues) && !missing(lag)) {
      stop("`issues` and `lag` are mutually exclusive")
    }
    check_single_string_param("data_source", data_source)
    check_string_param("signals", signals)
    check_string_param("sensor_names", sensor_names)
    check_single_string_param("time_type", time_type)
    check_single_string_param("geo_type", geo_type)
    check_epirange_param("time_values", time_values)
    check_string_param("geo_values", geo_values)
    check_single_epirange_param("as_of", as_of, FALSE)
    check_epirange_param("issues", issues, FALSE)
    check_single_int_param("lag", lag, FALSE)

    create_epidata_call(
      "covidcast/",
      list(
        data_source = data_source,
        signals = signals,
        sensor_names = sensor_names,
        time_type = time_type,
        geo_type = geo_type,
        time_values = time_values,
        geo_values = geo_values,
        as_of = as_of,
        issues = issues,
        lag = lag
      ),
      list(
        create_epidata_field_info("geo_value", "text"),
        create_epidata_field_info("signal", "text"),
        create_epidata_field_info("time_value", "date"),
        create_epidata_field_info("issue", "date"),
        create_epidata_field_info("lag", "int"),
        create_epidata_field_info("value", "float")
      )
    )
  }

#' fetch covidcast data
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/covidcast.html
#'
#' @examples
#' call <- covidcast(
#'   data_source = "jhu-csse",
#'   signals = "confirmed_7dav_incidence_prop",
#'   time_type = "day",
#'   geo_type = "state",
#'   time_values = epirange(20200601, 20200801),
#'   geo_values = "ca,fl"
#' )
#' fetch_csv(call)
#' @param data_source data source to fetch
#' @param signals data source to fetch
#' @param time_type data source to fetch
#' @param time_values data source to fetch
#' @param geo_type geo_type to fetch
#' @param geo_values data source to fetch
#' @param as_of data source to fetch
#' @param issues data source to fetch
#' @param lag data source to fetch
#' @return an instance of epidata_call
#'
#' @export
covidcast <-
  function(data_source,
           signals,
           time_type,
           geo_type,
           time_values,
           geo_values,
           as_of = NULL,
           issues = NULL,
           lag = NULL) {
    # Check parameters
    if (missing(data_source) ||
      missing(signals) ||
      missing(time_type) ||
      missing(geo_type) ||
      missing(time_values) || missing(geo_values)) {
      stop(
        "`data_source`, `signals`, `time_type`, `geo_type`, `time_values`, and `geo_value` are all required"
      )
    }
    if (!missing(issues) && !missing(lag)) {
      stop("`issues` and `lag` are mutually exclusive")
    }
    check_single_string_param("data_source", data_source)
    check_string_param("signals", signals)
    check_single_string_param("time_type", time_type)
    check_single_string_param("geo_type", geo_type)
    check_epirange_param("time_values", time_values)
    check_string_param("geo_values", geo_values)
    check_single_epirange_param("as_of", as_of, FALSE)
    check_epirange_param("issues", issues, FALSE)
    check_single_int_param("lag", lag, FALSE)

    create_epidata_call(
      "covidcast/",
      list(
        data_source = data_source,
        signals = signals,
        time_type = time_type,
        geo_type = geo_type,
        time_values = time_values,
        geo_values = geo_values,
        as_of = as_of,
        issues = issues,
        lag = lag
      ),
      list(
        create_epidata_field_info("source", "text"),
        create_epidata_field_info("signal", "text"),
        create_epidata_field_info(
          "geo_type",
          "categorical",
          categories = c("nation", "msa", "hrr", "hhs", "state", "county")
        ),
        create_epidata_field_info("geo_value", "text"),
        create_epidata_field_info("time_type", "categorical",
          categories =
            c("day", "week")
        ),
        create_epidata_field_info("time_value", "date"),
        create_epidata_field_info("issue", "date"),
        create_epidata_field_info("lag", "int"),
        create_epidata_field_info("value", "float"),
        create_epidata_field_info("stderr", "float"),
        create_epidata_field_info("sample_size", "float"),
        create_epidata_field_info("direction", "float"),
        create_epidata_field_info("missing_value", "int"),
        create_epidata_field_info("missing_stderr", "int"),
        create_epidata_field_info("missing_sample_size", "int")
      )
    )
  }

#' fetch Delphi's forecast
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/delphi.html
#'
#' @examples
#' call <- delphi(system = "ec", epiweek = 202006)
#' fetch_classic(call)
#' @param system system to fetch
#' @param epiweek epiweek to fetch
#' @return an instance of epidata_call
#'
#' @export
delphi <- function(system, epiweek) {
  check_single_string_param("system", system)
  check_single_epirange_param("epiweek", epiweek)

  create_epidata_call(
    "delphi/",
    list(system = system, epiweek = epiweek),
    list(
      create_epidata_field_info("system", "text"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("json", "text")
    ),
    only_supports_classic = TRUE
  )
}

#' fetch Delphi's PAHO Dengue nowcast
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/dengue_nowcast.html
#'
#' TODO: what are valid locations?
#' @examples
#' call <- dengue_nowcast(locations = "?", epiweeks = epirange(201501, 202001))
#' fetch_classic(call)
#' @param locations locations to fetch
#' @param epiweeks epiweeks to fetch
#' @return an instance of epidata_call
#'
#' @export
dengue_nowcast <- function(locations, epiweeks) {
  check_string_param("locations", locations)
  check_epirange_param("epiweeks", epiweeks)

  create_epidata_call(
    "dengue_nowcast/",
    list(locations = locations, epiweeks = epiweeks),
    list(
      create_epidata_field_info("location", "text"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("value", "float"),
      create_epidata_field_info("std", "float")
    )
  )
}

#' fetch Delphi's digital surveillance sensors
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/dengue_sensors.html
#'
#' TODO: what are valid locations and names?
#' @examples
#' call <- pvt_dengue_sensors(auth = "yourkey", names = "?", locations = "?", epiweeks = epirange(201501, 202001))
#' # fetch_classic(call)
#' @param auth authenfication token (note that this is not the same as the API key)
#' @param names names to fetch
#' @param locations locations to fetch
#' @param epiweeks epiweeks to fetch
#' @return an instance of epidata_call
#'
#' @export
pvt_dengue_sensors <- function(auth, names, locations, epiweeks) {
  check_single_string_param("auth", auth)
  check_string_param("names", names)
  check_string_param("locations", locations)
  check_epirange_param("epiweeks", epiweeks)

  create_epidata_call(
    "dengue_sensors/",
    list(
      auth = auth,
      names = names,
      locations = locations,
      epiweeks = epiweeks
    ),
    list(
      create_epidata_field_info("name", "text"),
      create_epidata_field_info("location", "text"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("value", "float")
    )
  )
}

#' fetch ECDC data
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/ecdc_ili.html
#'
#' TODO: find a region that has non-trivial output
#' @examples
#' call <- ecdc_ili(regions = "?", epiweeks = epirange(201201, 202001))
#' fetch_classic(call)
#' @param regions regions to fetch
#' @param epiweeks epiweeks to fetch
#' @param issues optionally specify the exact issues to fetch
#' @param lag optionally specify the issue lag
#' @return an instance of epidata_call
#'
#' @export
ecdc_ili <- function(regions,
                     epiweeks,
                     issues = NULL,
                     lag = NULL) {
  check_string_param("regions", regions)
  check_epirange_param("epiweeks", epiweeks)
  check_epirange_param("issues", issues, FALSE)
  check_single_int_param("lag", lag, FALSE)
  if (!missing(issues) && !missing(lag)) {
    stop("`issues` and `lag` are mutually exclusive")
  }
  create_epidata_call(
    "ecdc_ili/",
    list(
      regions = regions,
      epiweeks = epiweeks,
      issues = issues,
      lag = lag
    ),
    list(
      create_epidata_field_info("region", "text"),
      create_epidata_field_info("release_date", "date"),
      create_epidata_field_info("issue", "date"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("lag", "int"),
      create_epidata_field_info("incidence_rate", "float")
    )
  )
}

#' fetch FluSurv virological data
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/flusurv.html
#'
#' @examples
#' call <- flusurv(locations = "CA", epiweeks = epirange(201201, 202001))
#' fetch_classic(call)
#' @param locations locations to fetch
#' @param epiweeks epiweeks to fetch
#' @param issues optionally specify the exact issues to fetch
#' @param lag optionally specify the issue lag
#' @return an instance of epidata_call
#'
#' @export
flusurv <- function(locations,
                    epiweeks,
                    issues = NULL,
                    lag = NULL) {
  check_string_param("locations", locations)
  check_epirange_param("epiweeks", epiweeks)
  check_epirange_param("issues", issues, FALSE)
  check_single_int_param("lag", lag, FALSE)
  if (!missing(issues) && !missing(lag)) {
    stop("`issues` and `lag` are mutually exclusive")
  }
  create_epidata_call(
    "flusurv/",
    list(
      locations = locations,
      epiweeks = epiweeks,
      issues = issues,
      lag = lag
    ),
    list(
      create_epidata_field_info("release_date", "text"),
      create_epidata_field_info("location", "text"),
      create_epidata_field_info("issue", "date"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("lag", "int"),
      create_epidata_field_info("rage_age_0", "float"),
      create_epidata_field_info("rage_age_1", "float"),
      create_epidata_field_info("rage_age_2", "float"),
      create_epidata_field_info("rage_age_3", "float"),
      create_epidata_field_info("rage_age_4", "float"),
      create_epidata_field_info("rage_overall", "float")
    )
  )
}

#' fetch FluView virological data
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/fluview_clinical.html
#'
#' @examples
#' \donttest{
#' # can take a couple minutes; donttesting while cmu-delphi/delphi-epidata#48 is unresolved
#' call <- fluview_clinical(regions = "nat", epiweeks = epirange(201201, 202001))
#' fetch_classic(call)
#' }
#' @param regions regions to fetch
#' @param epiweeks epiweeks to fetch
#' @param issues optionally specify the exact issues to fetch
#' @param lag optionally specify the issue lag
#' @return an instance of epidata_call
#'
#' @export
fluview_clinical <-
  function(regions,
           epiweeks,
           issues = NULL,
           lag = NULL) {
    check_string_param("regions", regions)
    check_epirange_param("epiweeks", epiweeks)
    check_epirange_param("issues", issues, FALSE)
    check_single_int_param("lag", lag, FALSE)
    if (!missing(issues) && !missing(lag)) {
      stop("`issues` and `lag` are mutually exclusive")
    }
    create_epidata_call(
      "fluview_clinical/",
      list(
        regions = regions,
        epiweeks = epiweeks,
        issues = issues,
        lag = lag
      ),
      list(
        create_epidata_field_info("release_date", "text"),
        create_epidata_field_info("region", "text"),
        create_epidata_field_info("issue", "date"),
        create_epidata_field_info("epiweek", "epiweek"),
        create_epidata_field_info("lag", "int"),
        create_epidata_field_info("total_specimens", "int"),
        create_epidata_field_info("total_a", "int"),
        create_epidata_field_info("total_b", "int"),
        create_epidata_field_info("percent_positive", "float"),
        create_epidata_field_info("percent_a", "float"),
        create_epidata_field_info("percent_b", "float")
      )
    )
  }

#' fetch fluview meta data
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/fluview_meta.html
#'
#' @return an instance of epidata_call
#'
#' @export
fluview_meta <- function() {
  create_epidata_call(
    "fluview_meta/",
    list(),
    list(
      create_epidata_field_info("latest_update", "text"),
      create_epidata_field_info("latest_issue", "date"),
      create_epidata_field_info("table_rows", "int")
    )
  )
}

#' fetch fluview data
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/fluview.html
#'
#' @examples
#' call <- fluview(regions = "nat", epiweeks = epirange(201201, 202001))
#' fetch_classic(call)
#' @param regions regions to fetch
#' @param epiweeks epiweeks to fetch
#' @param issues optionally specify the exact issues to fetch
#' @param lag optionally specify the issue lag
#' @param auth optional authentication token (note that this is not the same as the API key)
#' @return an instance of epidata_call
#'
#' @export
fluview <-
  function(regions,
           epiweeks,
           issues = NULL,
           lag = NULL,
           auth = NULL) {
    check_string_param("regions", regions)
    check_epirange_param("epiweeks", epiweeks)
    check_epirange_param("issues", issues, FALSE)
    check_single_int_param("lag", lag, FALSE)
    check_single_string_param("auth", auth, FALSE)
    if (!is.null(issues) && !is.null(lag)) {
      stop("`issues` and `lag` are mutually exclusive")
    }

    create_epidata_call(
      "fluview/",
      list(
        regions = regions,
        epiweeks = epiweeks,
        issues = issues,
        lag = lag,
        auth = auth
      ),
      list(
        create_epidata_field_info("release_date", "text"),
        create_epidata_field_info("region", "text"),
        create_epidata_field_info("issue", "date"),
        create_epidata_field_info("epiweek", "epiweek"),
        create_epidata_field_info("lag", "int"),
        create_epidata_field_info("num_ili", "int"),
        create_epidata_field_info("num_patients", "int"),
        create_epidata_field_info("num_age_0", "int"),
        create_epidata_field_info("num_age_1", "int"),
        create_epidata_field_info("num_age_2", "int"),
        create_epidata_field_info("num_age_3", "int"),
        create_epidata_field_info("num_age_4", "int"),
        create_epidata_field_info("num_age_5", "int"),
        create_epidata_field_info("wili", "float"),
        create_epidata_field_info("ili", "float")
      )
    )
  }

#' fetch Google Flu Trends data
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/gft.html
#'
#' @examples
#' call <- gft(locations = "hhs1", epiweeks = epirange(201201, 202001))
#' fetch_classic(call)
#' @param locations locations to fetch
#' @param epiweeks epiweeks to fetch
#' @return an instance of epidata_call
#'
#' @export
gft <- function(locations, epiweeks) {
  check_string_param("locations", locations)
  check_epirange_param("epiweeks", epiweeks)
  create_epidata_call(
    "gft/",
    list(locations = locations, epiweeks = epiweeks),
    list(
      create_epidata_field_info("location", "text"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("num", "int")
    )
  )
}

#' fetch Google Health Trends data
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/ght.html
#'
#' TODO: find a non-trivial query
#' @examples
#' call <- pvt_ght(auth = "yourkey", locations = "ca", epiweeks = epirange(201201, 202001), query = "?")
#' # fetch_classic(call)
#' @param auth authentication token (note that this is not the same as the API key)
#' @param locations locations to fetch
#' @param epiweeks epiweeks to fetch
#' @param query query
#' @return an instance of epidata_call
#'
#' @export
pvt_ght <- function(auth, locations, epiweeks, query) {
  check_single_string_param("auth", auth)
  check_string_param("locations", locations)
  check_epirange_param("epiweeks", epiweeks)
  check_single_string_param("query", query)
  create_epidata_call(
    "ght/",
    list(
      auth = auth,
      locations = locations,
      epiweeks = epiweeks,
      query = query
    ),
    list(
      create_epidata_field_info("location", "text"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("value", "float")
    )
  )
}

#' fetch KCDC data
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/kcdc_ili.html
#'
#' TODO: find a non-trivial region
#' @examples
#' call <- kcdc_ili(regions = "?", epiweeks = epirange(201201, 202001))
#' fetch_csv(call)
#' @param regions regions to fetch
#' @param epiweeks epiweeks to fetch
#' @param issues optionally specify the exact issues to fetch
#' @param lag optionally specify the issue lag
#' @return an instance of epidata_call
#'
#' @export
kcdc_ili <- function(regions,
                     epiweeks,
                     issues = NULL,
                     lag = NULL) {
  check_string_param("regions", regions)
  check_epirange_param("epiweeks", epiweeks)
  check_epirange_param("issues", issues, FALSE)
  check_single_int_param("lag", lag, FALSE)
  if (!missing(issues) && !missing(lag)) {
    stop("`issues` and `lag` are mutually exclusive")
  }
  create_epidata_call(
    "kcdc_ili/",
    list(
      regions = regions,
      epiweeks = epiweeks,
      issues = issues,
      lag = lag
    ),
    list(
      create_epidata_field_info("release_date", "text"),
      create_epidata_field_info("region", "text"),
      create_epidata_field_info("issue", "date"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("lag", "int"),
      create_epidata_field_info("ili", "float")
    )
  )
}

#' fetch AFHSB meta data
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/meta_afhsb.html
#'
#' @param auth authentication token (note that this is not the same as the API key)
#' @return an instance of epidata_call
#'
#' @export
pvt_meta_afhsb <- function(auth) {
  check_single_string_param("auth", auth)

  create_epidata_call("meta_afhsb/", list(auth = auth), only_supports_classic = TRUE)
}

#' fetch NoroSTAT meta data
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/meta_norostat.html
#'
#' @param auth authentication token (note that this is not the same as the API key)
#' @return an instance of epidata_call
#'
#' @export
pvt_meta_norostat <- function(auth) {
  check_single_string_param("auth", auth)

  create_epidata_call("meta_norostat/", list(auth = auth), only_supports_classic = TRUE)
}

#' fetch api meta data
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/meta.html
#'
#' @return an instance of epidata_call
#'
#' @export
meta <- function() {
  create_epidata_call("meta/", list(), only_supports_classic = TRUE)
}

#' fetch NIDSS dengue data
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/nidss_dengue.html
#'
#' @examples
#' call <- nidss_dengue(locations = "taipei", epiweeks = epirange(201201, 202001))
#' fetch_classic(call)
#' @param locations locations to fech
#' @param epiweeks epiweeks to fetch
#' @return an instance of epidata_call
#'
#' @export
nidss_dengue <- function(locations, epiweeks) {
  check_string_param("locations", locations)
  check_epirange_param("epiweeks", epiweeks)

  create_epidata_call(
    "nidss_dengue/",
    list(locations = locations, epiweeks = epiweeks),
    list(
      create_epidata_field_info("location", "text"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("count", "int")
    )
  )
}

#' fetch NIDSS flu data
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/nidss_flu.html
#'
#' @examples
#' call <- nidss_flu(regions = "taipei", epiweeks = epirange(201201, 202001))
#' fetch_classic(call)
#' @param regions regions to fetch
#' @param epiweeks epiweeks to fetch
#' @param issues optional issues
#' @param lag optional lag
#' @return an instance of epidata_call
#'
#' @export
nidss_flu <-
  function(regions,
           epiweeks,
           issues = NULL,
           lag = NULL) {
    check_string_param("regions", regions)
    check_epirange_param("epiweeks", epiweeks)
    check_epirange_param("issues", issues, FALSE)
    check_single_int_param("lag", lag, FALSE)

    if (!is.null(issues) && !is.null(lag)) {
      stop("`issues` and `lag` are mutually exclusive")
    }

    create_epidata_call(
      "nidss_flu/",
      list(
        regions = regions,
        epiweeks = epiweeks,
        issues = issues,
        lag = lag
      ),
      list(
        create_epidata_field_info("release_date", "text"),
        create_epidata_field_info("region", "text"),
        create_epidata_field_info("epiweek", "epiweek"),
        create_epidata_field_info("issue", "date"),
        create_epidata_field_info("lag", "int"),
        create_epidata_field_info("visits", "int"),
        create_epidata_field_info("ili", "float")
      )
    )
  }


#' fetch NoroSTAT data (point data, no min/max)
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/norostat.html
#'
#' @examples
#' call <- pvt_norostat(
#'   auth = "yourkey",
#'   location = "Minnesota, Ohio, Oregon, Tennessee, and Wisconsin",
#'   epiweeks = epirange(201201, 202001)
#' )
#' # fetch_classic(call)
#' @param auth authentication token (note that this is not the same as the API key)
#' @param location location to fetch
#' @param epiweeks epiweeks to fetch
#' @return an instance of epidata_call
#'
#' @export
pvt_norostat <- function(auth, location, epiweeks) {
  check_single_string_param("auth", auth)
  check_single_string_param("locations", location)
  check_epirange_param("epiweeks", epiweeks)

  create_epidata_call(
    "norostat/",
    list(
      auth = auth,
      location = location,
      epiweeks = epiweeks
    ),
    list(
      create_epidata_field_info("release_date", "text"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("value", "int")
    )
  )
}

#' fetch Delphi's wILI nowcast
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/nowcast.html
#'
#' @examples
#' call <- nowcast(location = "ca", epiweeks = epirange(201201, 202001))
#' fetch_classic(call)
#' @param locations locations to fetch
#' @param epiweeks epiweeks to fetch
#' @return an instance of epidata_call
#'
#' @export
nowcast <- function(locations, epiweeks) {
  check_string_param("locations", locations)
  check_epirange_param("epiweeks", epiweeks)

  create_epidata_call(
    "nowcast/",
    list(locations = locations, epiweeks = epiweeks),
    list(
      create_epidata_field_info("location", "text"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("value", "float"),
      create_epidata_field_info("std", "float")
    )
  )
}

#' fetch Paho Dengue
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/paho_dengue.html
#'
#' @examples
#' call <- paho_dengue(regions = "ca", epiweeks = epirange(201201, 202001))
#' fetch_classic(call)
#' @param regions regions to fetch
#' @param epiweeks epiweeks to fetch
#' @param issues issues to fetch
#' @param lag lag to fetch
#' @return an instance of epidata_call
#'
#' @export
paho_dengue <- function(regions,
                        epiweeks,
                        issues = NULL,
                        lag = NULL) {
  check_string_param("regions", regions)
  check_epirange_param("epiweeks", epiweeks)
  check_epirange_param("issues", issues, FALSE)
  check_single_int_param("lag", lag, FALSE)

  create_epidata_call(
    "quidel/",
    list(
      regions = regions,
      epiweeks = epiweeks,
      issues = issues,
      lag = lag
    ),
    list(
      create_epidata_field_info("release_date", "text"),
      create_epidata_field_info("region", "text"),
      create_epidata_field_info("serotype", "text"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("issue", "date"),
      create_epidata_field_info("lag", "int"),
      create_epidata_field_info("total_pop", "int"),
      create_epidata_field_info("num_dengue", "int"),
      create_epidata_field_info("num_severe", "int"),
      create_epidata_field_info("num_deaths", "int"),
      create_epidata_field_info("incidence_rate", "float")
    )
  )
}

#' fetch Quidel data
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/quidel.html
#'
#' @examples
#' call <- pvt_quidel(auth = "yourkey", epiweeks = epirange(201201, 202001), locations = "hhs1")
#' # fetch_classic(call)
#' @param auth authentication token (note that this is not the same as the API key)
#' @param epiweeks epiweeks to fetch
#' @param locations locations to fetch
#' @return an instance of epidata_call
#'
#' @export
pvt_quidel <- function(auth, epiweeks, locations) {
  check_single_string_param("auth", auth)
  check_epirange_param("epiweeks", epiweeks)
  check_string_param("locations", locations)

  create_epidata_call(
    "quidel/",
    list(
      auth = auth,
      epiweeks = epiweeks,
      locations = locations
    ),
    list(
      create_epidata_field_info("location", "text"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("value", "float")
    )
  )
}

#' fetch Delphi's digital surveillance sensors
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/sensors.html
#'
#' @examples
#' call <- pvt_sensors(auth = "yourkey", names = "sar3", locations = "nat", epiweeks = epirange(201501, 202001))
#' # fetch_classic(call)
#' @param auth authentication token (note that this is not the same as the API key)
#' @param names names to fetch
#' @param locations locations to fetch
#' @param epiweeks epiweeks to fetch
#' @return an instance of epidata_call
#'
#' @export
pvt_sensors <- function(auth, names, locations, epiweeks) {
  check_single_string_param("auth", auth)
  check_string_param("names", names)
  check_string_param("locations", locations)
  check_epirange_param("epiweeks", epiweeks)

  create_epidata_call(
    "sensors/",
    list(
      auth = auth,
      names = names,
      locations = locations,
      epiweeks = epiweeks
    ),
    list(
      create_epidata_field_info("name", "text"),
      create_epidata_field_info("location", "text"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("value", "float")
    )
  )
}

#' fetch HealthTweets data
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/twitter.html
#'
#' @examples
#' call <- pvt_twitter(auth = "yourkey", locations = "CA", epiweeks = epirange(201501, 202001))
#' # fetch_csv(call)
#' @param auth authentication token (note that this is not the same as the API key)
#' @param locations locations to fetch
#' @param dates epiweeks to fetch
#' @param epiweeks epiweeks to fetch
#' @return an instance of epidata_call
#'
#' @export
pvt_twitter <-
  function(auth,
           locations,
           dates = NULL,
           epiweeks = NULL) {
    check_single_string_param("auth", auth)
    check_string_param("locations", locations)
    check_epirange_param("dates", dates, FALSE)
    check_epirange_param("epiweeks", epiweeks, FALSE)
    if (!xor(missing(dates), missing(epiweeks))) {
      stop("exactly one of `dates` and `epiweeks` is required")
    }
    time_field <- if (!is.null(dates)) {
      create_epidata_field_info("date", "date")
    } else {
      create_epidata_field_info("epiweek", "epiweek")
    }
    create_epidata_call(
      "twitter/",
      list(
        auth = auth,
        locations = locations,
        dates = dates,
        epiweeks = epiweeks
      ),
      list(
        create_epidata_field_info("location", "text"),
        time_field,
        create_epidata_field_info("num", "int"),
        create_epidata_field_info("total", "int"),
        create_epidata_field_info("percent", "float")
      )
    )
  }

#' fetch Wikipedia access data
#'
#' API docs: https://cmu-delphi.github.io/delphi-epidata/api/wiki.html
#'
#' @examples
#' call <- wiki(articles = "avian_influenza", epiweeks = epirange(201501, 202001))
#' fetch_csv(call)
#' @param articles articles to fetch
#' @param dates dates to fetch
#' @param epiweeks epiweeks to fetch
#' @param hours hours to fetch
#' @param language language
#' @return an instance of epidata_call
#'
#' @export
wiki <-
  function(articles,
           dates = NULL,
           epiweeks = NULL,
           hours = NULL,
           language = "en") {
    check_string_param("articles", articles)
    check_epirange_param("dates", dates, FALSE)
    check_epirange_param("epiweeks", epiweeks, FALSE)
    check_int_param("hours", hours, FALSE)
    check_single_string_param("language", language, FALSE)
    if (!xor(missing(dates), missing(epiweeks))) {
      stop("exactly one of `dates` and `epiweeks` is required")
    }
    time_field <- if (!is.null(dates)) {
      create_epidata_field_info("date", "date")
    } else {
      create_epidata_field_info("epiweek", "epiweek")
    }
    create_epidata_call(
      "wiki/",
      list(
        articles = articles,
        dates = dates,
        epiweeks = epiweeks,
        hours = hours,
        language = language
      ),
      list(
        create_epidata_field_info("article", "text"),
        time_field,
        create_epidata_field_info("count", "int"),
        create_epidata_field_info("total", "int"),
        create_epidata_field_info("hour", "int"),
        create_epidata_field_info("value", "float")
      )
    )
  }
