#' Fetch AFHSB data (point data, no min/max)
#'
#' @param auth Character. Authentication token.
#' @param locations Character vector. Locations to fetch (region/state, or 3-letter country code labels).
#' @param epiweeks Epirange. Epiweeks to fetch in the form epirange(startweek, endweek), where
#'   startweek and endweek are in the form YYYYWW.
#' @param flu_types Character. Flu types to fetch (disjoint (flu1, flu2-flu1, flu3-flu2, ili-flu3)
#'   or subset (flu2, flu3, ili) flu type labels).
#' @return An instance of epidata_call.
#'
#' @references API docs: https://cmu-delphi.github.io/delphi-epidata/api/afhsb.html
#'
#' @examples
#' \donttest{
#' call <- pvt_afhsb(auth = "yourkey", "fl,ca", epirange(202001, 202110), "flu1,flu2-flu1")
#' fetch_tbl(call)
#' }
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

#' Fetch CDC page hits
#'
#' @examples
#' \donttest{
#' call <- pvt_cdc(auth = "yourkey", epirange(20210101, 20210201), "fl,ca")
#' fetch_tbl(call)
#' }
#' @param auth Character. Authentication token.
#' @param epiweeks Epirange. Epiweeks to fetch in the form epirange(startweek, endweek), where
#'   startweek and endweek are in the form YYYYWW.
#' @param locations Character vector. Locations to fetch (region/state labels).
#' @return An instance of epidata_call
#'
#' @references API docs: https://cmu-delphi.github.io/delphi-epidata/api/cdc.html
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

#' Fetch COVID hospitalization facility identifiers
#'
#' Obtains unique identifiers and other metadata for COVID hospitalization facilities of interest.
#' This is a companinon endpoint to the [covid_hosp_facility()] endpoint.
#'
#' @details Only one argument needs to be specified.
#' Combinations of the arguments are not currently supported.
#' For instance, specifying both city and state are not supported.
#'
#' @examples
#' \donttest{
#' call <- covid_hosp_facility_lookup(state = "fl")
#' fetch_tbl(call)
#' }
#' @param state Character. A two-letter character string state abbreviation.
#' @param ccn Character. A character string for facility CMS certification number.
#' @param city Character. A characater string for city name.
#' @param zip Character. A 5-digit zip code.
#' @param fips_code Character. A 5-digit fips county code, zero-padded.
#' @return An instance of epidata_call.
#'
#' @references API documentation: <https://cmu-delphi.github.io/delphi-epidata/api/covid_hosp_facility_lookup.html>
#'
#' @seealso [covid_hosp_facility()]
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

#' Fetch COVID hospitalization data for specific facilities
#'
#' Obtains the COVID-19 reported patient impact and hospital capacity data by facility.
#' This dataset is provided by the US Department of Health & Human Services via healthdata.gov.
#'
#' @details Starting October 1, 2022, some facilities are only required to report annually.
#' The companion function [covid_hosp_facility_lookup()] can be used to look up facility identifiers
#' in a variety of ways.
#'
#' @examples
#' \donttest{
#' call <- covid_hosp_facility(hospital_pks = "100075", collection_weeks = epirange(20200101, 20200501))
#' fetch_tbl(call)
#' }
#' @param hospital_pks Character. A character string of facility unique identifiers.
#' @param collection_weeks Epirange. Epiweeks to fetch, in the form epirange(startdate,enddate), where
#'   startdate and enddate are YYYYMMDD (string or numeric).
#' @param publication_dates Epirange. Publication dates to fetch, in the form epirange(startdate,enddate), where
#'   startdate and enddate are YYYYMMDD (string or numeric).
#' @return An instance of epidata_call.
#'
#' @references API documentation: <https://cmu-delphi.github.io/delphi-epidata/api/covid_hosp_facility.html>
#'
#' See also the official description and data dictionary at <healthdata.gov> for more information.
#'
#' @seealso [covid_hosp_facility_lookup()], [epirange()]
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

#' Fetch COVID Hospitalization Data by State
#'
#' Obtains the COVID-19 reported patient impact and hospital capacity data by state.
#' This dataset is provided by the US Department of Health & Human Services via healthdata.gov.
#'
#' @details Starting October 1, 2022, some facilities are only required to report annually.
#'
#' @examples
#' \donttest{
#' call <- covid_hosp_state_timeseries(states = "fl", dates = epirange(20200101, 20200501))
#' fetch_tbl(call)
#' }
#' @param states Character vector. Two letter state abbreviations.
#' @param dates Epirange. Dates to fetch, in the form epirange(startdate,enddate), where
#'   startdate and enddate are in the form YYYYMMDD (string or numeric).
#' @param issues Epirange. Optional parameter to specify the issue dates of the data. The parameter is
#'   in the form epirange(startdate,enddate), where startdate and enddate are in the form YYYYMMDD.
#'   If issues is not specified, then the most recent issue is used by default.
#' @return an instance of epidata_call
#'
#' @references API Documentation: <https://cmu-delphi.github.io/delphi-epidata/api/covid_hosp.html>.
#'
#' See also the official description and data dictionary at <healthdata.gov> for more information.
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

#' Fetch covidcast meta data
#'
#' Fetch a summary of metadata for all sources and signals that are available in the API, along with
#' basic summary statistics such as the dates they are available, the geographic levels at which they
#' are reported, and etc.
#'
#' @return an instance of epidata_call
#'
#' @references API Documentation: <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_meta.html>.
#'
#' @examples
#' \donttest{
#' call_meta <- covidcast_meta()
#' fetch_classic(call_meta)
#' }
#'
#' @seealso [covidcast()]
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

#' Fetch covidcast data
#'
#' @examples
#' \donttest{
#' call <- covidcast(
#'   data_source = "jhu-csse",
#'   signals = "confirmed_7dav_incidence_prop",
#'   time_type = "day",
#'   geo_type = "state",
#'   time_values = epirange(20200601, 20200801),
#'   geo_values = "ca,fl"
#' )
#' fetch_tbl(call)
#' }
#' @param data_source Character. A data source to query: (1) fb-survey; (2) jhu-csse; (3) google-symptoms;
#'   (4) doctor-visits; (5) quidel; (6) hhs. For more information about each data source,
#'   see <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html>.
#' @param signals Character. The signals to query from a specific source. A list of available signals for
#'   each data source can be found in <https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals>.
#'   Each data source may have different signals.
#' @param time_type Character. The temporal resolution of the data. Either "day" or "week". Availability
#'   depends on the signal.
#' @param geo_type Character. The geographic resolution of the data: (1) county; (2) hrr, hospital referral
#'   region; (3) hhs;
#'   (4) msa, metropolitan statistical area; (5) dma, designated market areas; (6) state; (7) nation.
#'   See <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html> for details on which types
#'   are available for each data source.
#' @param time_values Epirange. The dates to fetch, specified in the form epirange(startdate,enddate), where
#'   startdate and enddate are of the form YYYYMMDD (can be passed as string or numeric).
#' @param geo_values Character vector. Character strings that specify which geographies to return. "*" fetches
#'   all geographies. A list of string IDs or a character vector of IDs will fetch specific locations. More
#'   information can be found in <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html>.
#' @param as_of Epirange. Fetch the data as of a specific date. If not specified, the most recent data will
#'   be returned. Specified in the form epirange(startdate,enddate), where startdate and enddate are of the
#'   form YYYYMMDD (string or numeric). Cannot be used in conjunction with `issues` or `lag`.
#' @param issues Epirange. Fetch the data as of a specific issue date. If not specified, the most recent data
#'   will be returned. Specified in the form epirange(startdate,enddate), where startdate and enddate are of
#'   the form YYYYMMDD (string or numeric). Cannot be used in conjunction with `as_of` or `lag`.
#' @param lag Integer. Fetch the issues with a specific lag. If not specified, the most recent data will be
#'   returned. An alternative to and mutually exclusive with `issues`. Cannot be used in conjunction with `as_of`
#'   or `issues`.
#' @return an instance of `epidata_call`
#'
#' @references COVIDcast API documentation: <https://cmu-delphi.github.io/delphi-epidata/api/covidcast.html>
#'
#' Documentation for all COVIDcast sources and signals:
#' <https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals>
#'
#' COVIDcast public dashboard: <https://delphi.cmu.edu/covidcast/>
#'
#' @seealso [covidcast_meta()], [epirange()]
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

#' Fetch Delphi's forecast
#'
#' @examples
#' \donttest{
#' call <- delphi(system = "ec", epiweek = 202006)
#' fetch_classic(call)
#' }
#' @param system Character. The system name to fetch.
#' @param epiweek Epirange. The epiweek to fetch, in the form epirange(startdate,enddate), where startdate
#'   and enddate are of the form YYYYMMDD (string or numeric).
#' @return An instance of epidata_call
#'
#' @references API docs: https://cmu-delphi.github.io/delphi-epidata/api/delphi.html
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

#' Fetch Delphi's PAHO Dengue nowcast
#'
#' TODO: what are valid locations here?
#' @examples
#' \donttest{
#' call <- dengue_nowcast(locations = "?", epiweeks = epirange(201501, 202001))
#' fetch_classic(call)
#' }
#' @param locations Character vector. The locations to fetch.
#' @param epiweeks Epirange. The epiweeks to fetch, specified in the form epirange(startdate,enddate), where startdate
#'   and enddate are of the form YYYYMMDD (string or numeric).
#' @return An instance of epidata_call
#'
#' @references API docs: https://cmu-delphi.github.io/delphi-epidata/api/dengue_nowcast.html
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

#' Fetch Delphi's digital surveillance sensors
#'
#' TODO: what are valid locations and names?
#' @examples
#' \donttest{
#' call <- pvt_dengue_sensors(auth = "yourkey", names = "?", locations = "?", epiweeks = epirange(201501, 202001))
#' fetch_classic(call)
#' }
#' @param auth Character. Authentication token.
#' @param names Character vector. The list of names to fetch.
#' @param locations Character vector. The locations to fetch.
#' @param epiweeks Epirange. The epiweeks to fetch in the form epirange(startweek,endweek), where startweek
#'   and endweek are of the form YYYYWW (string or numeric).
#' @return An instance of epidata_call
#'
#' @references API docs: https://cmu-delphi.github.io/delphi-epidata/api/dengue_sensors.html
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

#' Fetch ECDC data
#'
#' Obtain information on influenza-like-illness from the European Center of Disease Control.
#'
#' @details The list of location argument can be found in
#' <https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/ecdc_regions.txt>.
#'
#' @examples
#' \donttest{
#' call <- ecdc_ili(regions = "austria", epiweeks = epirange(201201, 202001))
#' fetch_classic(call)
#' }
#' @param regions Character vector. The regions to fetch.
#' @param epiweeks Epirange. The epiweeks to fetch in the form epirange(startdate,enddate), where
#'   startdate and enddate are of the form YYYYWW (string or numeric).
#' @param issues Epirange. Optionally, the issues to fetch in the form epirange(startdate,enddate), where
#'   startdate and enddate are of the form YYYYWW (string or numeric). Mutually exclusive with `lag`.
#' @param lag Integer. Optionally, the lag of the issues to fetch. An alternative to and mutually exclusive
#'   with `issues`.
#' @return An instance of epidata_call
#'
#' @references API Documentation: <https://cmu-delphi.github.io/delphi-epidata/api/ecdc_ili.html>.
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

#' Fetch FluSurv virological data
#'
#' Obtain information on flu hospitalization rates from the Center of Disease Control.
#'
#' @details The list of location argument can be found in
#' <https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/flusurv_locations.txt>.
#'
#' @examples
#' \donttest{
#' call <- flusurv(locations = "CA", epiweeks = epirange(201201, 202001))
#' fetch_classic(call)
#' }
#' @param locations Character vector. Character strings indicating location.
#' @param epiweeks Epirange. The epiweeks to fetch the form epirange(startweek,endweek), where startweek
#'   and endweek are of the form YYYYWW (string or numeric).
#' @param issues Epirange. Optionally, specify the issues to fetch in the form epirange(startweek,endweek), where
#'   startweek and endweek are of the form YYYYWW (string or numeric). Mutually exclusive with `lag`.
#' @param lag Integer. Optionally, specify the lag for the issues to fetch. An alternative to and mutually exclusive
#'   with `issues`.
#' @return An instance of epidata_call
#'
#' @references API Documentation: <https://cmu-delphi.github.io/delphi-epidata/api/flusurv.html>.
#' See also <https://gis.cdc.gov/GRASP/Fluview/FluHospRates.html>.
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

#' Fetch FluView virological data
#'
#' @examples
#' \donttest{
#' # can take a couple minutes; donttesting while cmu-delphi/delphi-epidata#48 is unresolved
#' call <- fluview_clinical(regions = "nat", epiweeks = epirange(201201, 202001))
#' fetch_classic(call)
#' }
#' @param regions Character vector. The regions to fetch.
#' @param epiweeks Epirange. The epiweeks to fetch in the form epirange(startweek,endweek), where startweek
#'   and endweek are of the form YYYYWW (string or numeric).
#' @param issues Epirange. Optionally, specify the issues to fetch in the form epirange(startweek,endweek), where startweek
#'   and endweek are of the form YYYYWW (string or numeric). Mutually exclusive with `lag`.
#' @param lag Integer. Optionally, specify the lag for the issues to fetch. An alternative to and mutually exclusive
#'   with `issues`.
#' @return An instance of epidata_call
#'
#' @references API docs: https://cmu-delphi.github.io/delphi-epidata/api/fluview_clinical.html
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

#' Fetch fluview meta data
#'
#' @return An instance of epidata_call
#'
#' @references API docs: https://cmu-delphi.github.io/delphi-epidata/api/fluview_meta.html
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


#' Fetch fluview data
#'
#' Obtains information on outpatient inluenza-like-illness (ILI) from U.S. Outpatient Influenza-like Illness Surveillance
#'   Network (ILINet).
#'
#' @details The full list of location inputs can be accsssed at
#'   <https://github.com/cmu-delphi/delphi-epidata/blob/main/src/acquisition/fluview/fluview_locations.py>.
#'
#' @examples
#' call <- fluview(regions = "nat", epiweeks = epirange(201201, 202001))
#' fetch_classic(call)
#' @param regions Character vector. The locations to fetch. Can we any string IDs in national, HHS region,
#'   census division, most states and territories, and so on. Full list link below.
#' @param epiweeks Epirange. The epiweeks to fetch in the form epirange(startweek,endweek), where startweek
#'   and endweek are of the form YYYYWW (string or numeric).
#' @param issues Epirange. Optionally, specify the issues to fetch in the form epirange(startweek,endweek), where startweek
#'   and endweek are of the form YYYYWW (string or numeric). Mutually exclusive with `lag`.
#' @param lag Integer. Optionally, specify the lag for the issues to fetch. An alternative to and mutually exclusive
#'   with `issues`.
#' @param auth Character. Optionally, specify your authentication token.
#' @return An instance of epidata_call
#'
#' @references API Documentation: <https://cmu-delphi.github.io/delphi-epidata/api/fluview.html>. 
#' For more information on ILINet, see <https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html>.
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

#' Fetch Google Flu Trends data
#'
#' Obtains estimates of inluenza activity based on volume of certain search queries from Google.
#'
#' @details Google has discontinues Flu Trends, and this is now a static endpoint. Possibile input for locations
#'   can be found in <https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/regions.txt>,
#'   <https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/states.txt>, and
#'   <https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/cities.txt>.
#'
#' @examples
#' call <- gft(locations = "hhs1", epiweeks = epirange(201201, 202001))
#' fetch_classic(call)
#' @param locations Character vector. Specifies the locations to be fetched.
#' @param epiweeks Epirange. Specifies the epiweeks to be fetched in the form epirange(startweek,endweek), where startweek
#'   and endweek are of the form YYYYWW (string or numeric).
#'
#' @references API Documentation: <https://cmu-delphi.github.io/delphi-epidata/api/gft.html>
#'
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

#' Fetch Google Health Trends data
#'
#' TODO: find a non-trivial query
#' @examples
#' \donttest{
#' call <- pvt_ght(auth = "yourkey", locations = "ca", epiweeks = epirange(201201, 202001), query = "?")
#' fetch_classic(call)
#' }
#' @param auth Character. Specifies your authentication token.
#' @param locations Character vector. Specifies the locations to be fetched.
#' @param epiweeks Epirange. Specifies the epiweeks to be fetched in the form epirange(startweek,endweek), where startweek
#'   and endweek are of the form YYYYWW (string or numeric).
#' @param query Character. Specifies the query to be fetched.
#' @return an instance of epidata_call
#'
#' @references API docs: https://cmu-delphi.github.io/delphi-epidata/api/ght.html
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

#' Fetch KCDC data
#'
#' TODO: find a non-trivial region
#' @examples
#' \donttest{
#' call <- kcdc_ili(regions = "?", epiweeks = epirange(201201, 202001))
#' fetch_tbl(call)
#' }
#' @param regions Character vector. Specifies the regions to be fetched.
#' @param epiweeks Epirange. Specifies the epiweeks to be fetched in the form epirange(startweek,endweek), where
#'   startweek and endweek are of the form YYYYWW (string or numeric).
#' @param issues Epirange. Optionally, specifies the issue range to be fetched in the form
#'   epirange(startweek,endweek), where startweek and endweek are of the form YYYYWW (string or numeric).
#' @param lag Integer. Optionally, specifies the lag of the issues. An alternative to and mutually exclusive
#'   with `issues`.
#' @return an instance of epidata_call
#'
#' @references API docs: https://cmu-delphi.github.io/delphi-epidata/api/kcdc_ili.html
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

#' Fetch AFHSB meta data
#'
#' @param auth Character. Specifies your authentication token.
#' @return An instance of epidata_call
#'
#' @references API docs: https://cmu-delphi.github.io/delphi-epidata/api/meta_afhsb.html
#'
#' @export
pvt_meta_afhsb <- function(auth) {
  check_single_string_param("auth", auth)

  create_epidata_call("meta_afhsb/", list(auth = auth), only_supports_classic = TRUE)
}

#' Fetch NoroSTAT meta data
#'
#' @param auth Character. Specifies your authentication token.
#' @return An instance of epidata_call
#'
#' @references API docs: https://cmu-delphi.github.io/delphi-epidata/api/meta_norostat.html
#'
#' @export
pvt_meta_norostat <- function(auth) {
  check_single_string_param("auth", auth)

  create_epidata_call("meta_norostat/", list(auth = auth), only_supports_classic = TRUE)
}

#' Fetch api meta data
#'
#' @return An instance of epidata_call
#'
#' @references API docs: https://cmu-delphi.github.io/delphi-epidata/api/meta.html
#'
#' @export
meta <- function() {
  create_epidata_call("meta/", list(), only_supports_classic = TRUE)
}

#' Fetch NIDSS dengue data
#'
#' Obtains counts of confirmed dengue cases in Taiwan from Taiwan National Infectious Disease Statistical System.
#'
#' @details Possible location inputs can be found in
#' <https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/nidss_regions.txt> and
#' <https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/nidss_locations.txt>.
#'
#' @examples
#' \donttest{
#' call <- nidss_dengue(locations = "taipei", epiweeks = epirange(201201, 202001))
#' fetch_classic(call)
#' }
#' @param locations Character vector. Specifies the locations of interest. Possible inputs include but are not limited
#'   to (1) nationwide, (2) central, (3) eastern, (4) kaoping, (5) northern, (6) southern, and (7) taipei.
#' @param epiweeks Eppirange. Specifies the epiweeks to be fetched in the form epirange(startweek,endweek), where
#'    startweek and endweek are of the form YYYYWW (string or numeric).
#'
#' @return An instance of epidata_call
#'
#' @references API Documentation: <https://cmu-delphi.github.io/delphi-epidata/api/nidss_dengue.html>
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

#' Fetch NIDSS flu data
#'
#' Obtains information on outpatient inluenza-like-illness from Taiwan National Infectious Disease Statistical System.
#'
#' @examples
#' \donttest{
#' call <- nidss_flu(regions = "taipei", epiweeks = epirange(201201, 202001))
#' fetch_classic(call)
#' }
#' @param regions Character vector. The regions to fetch.
#' @param epiweeks Epirange. The epiweeks to fetch in the form epirange(startweek,endweek), where
#'   startweek and endweek are of the form YYYYWW (string or numeric).
#' @param issues Epirange. Optionally, the issues to fetch in the form epirange(startweek,endweek), where
#'   startweek and endweek are of the form YYYYWW (string or numeric). Mutually exclusive with `lag`.
#' @param lag Integer. Optionally, specifies the lag of the issues. An alternative to and mutually exclusive
#'   with `issues`.
#' @return An instance of epidata_call
#'
#' @references API Documentation: <https://cmu-delphi.github.io/delphi-epidata/api/nidss_flu.html>
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


#' Fetch NoroSTAT data (point data, no min/max)
#'
#' @examples
#' \donttest{
#' call <- pvt_norostat(
#'   auth = "yourkey",
#'   location = "Minnesota, Ohio, Oregon, Tennessee, and Wisconsin",
#'   epiweeks = epirange(201201, 202001)
#' )
#' fetch_classic(call)
#' }
#' @param auth Character. Specifies the authentication key.
#' @param location Character vector. Specifies the locations of interest.
#' @param epiweeks Eppirange. Specifies the epiweeks to be fetched in the form epirange(startweek,endweek), where
#'   startweek and endweek are of the form YYYYWW (string or numeric).
#' @return An instance of epidata_call
#'
#' @references API docs: https://cmu-delphi.github.io/delphi-epidata/api/norostat.html
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

#' Fetch Delphi's ILI nowcast
#'
#' Obtains information on outpatient inluenza-like-illness (ILI) from Delphi's epidemiological data
#'
#' @details The full list of location inputs can be accsssed at
#' <https://github.com/cmu-delphi/delphi-epidata/blob/main/src/acquisition/fluview/fluview_locations.py>.
#'
#' @examples
#' \donttest{
#' call <- nowcast(location = "ca", epiweeks = epirange(201201, 202001))
#' fetch_classic(call)
#' }
#' @param locations Character vector. The locations to be fetched, which can take ID codes in the following
#'  geo types: national, HHS region, census division, most states and territories, and so on.
#' @param epiweeks Epirange. The epiweeks to fetch in the form epirange(startweek,endweek), where
#'    startweek and endweek are of the form YYYYWW (string or numeric).
#' @return An instance of epidata_call
#'
#' @references API Documentation: <https://cmu-delphi.github.io/delphi-epidata/api/nowcast.html>.
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

#' Fetch Paho Dengue
#'
#' @examples
#' \donttest{
#' call <- paho_dengue(regions = "ca", epiweeks = epirange(201201, 202001))
#' fetch_classic(call)
#' }
#' @param regions Character vector. The regions to fetch.
#' @param epiweeks Epirange. The epiweeks to fetch in the form epirange(startweek,endweek), where
#'   startweek and endweek are of the form YYYYWW (string or numeric).
#' @param issues Epirange. Optionally, the issues to fetch in the form epirange(startweek,endweek), where
#'   startweek and endweek are of the form YYYYWW (string or numeric). Mutually exclusive with `lag`.
#' @param lag Integer. Optionally, the lag to fetch in the form epirange(startweek,endweek), where
#'   startweek and endweek are of the form YYYYWW (string or numeric). Mutually exclusive with `issues`.
#' @return An instance of epidata_call
#'
#' @references API docs: https://cmu-delphi.github.io/delphi-epidata/api/paho_dengue.html
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

#' Fetch Quidel data
#'
#' @examples
#' \donttest{
#' call <- pvt_quidel(auth = "yourkey", epiweeks = epirange(201201, 202001), locations = "hhs1")
#' fetch_classic(call)
#' }
#' @param auth Character. The authentication key.
#' @param epiweeks Epirange. The epiweeks to fetch in the form epirange(startweek,endweek), where
#'   startweek and endweek are of the form YYYYWW (string or numeric).
#' @param locations Character vector. The locations to fetch.
#' @return An instance of epidata_call
#'
#' @references API docs: https://cmu-delphi.github.io/delphi-epidata/api/quidel.html
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

#' Fetch Delphi's digital surveillance sensors
#'
#' @examples
#' \donttest{
#' call <- pvt_sensors(auth = "yourkey", names = "sar3", locations = "nat", epiweeks = epirange(201501, 202001))
#' fetch_classic(call)
#' }
#' @param auth Character. The authentication key.
#' @param names Character vector. The names of the sensors to fetch.
#' @param locations Character vector. The locations to fetch.
#' @param epiweeks Epirange. The epiweeks to fetch in the form epirange(startweek,endweek), where
#'   startweek and endweek are of the form YYYYWW (string or numeric).
#' @return An instance of epidata_call
#'
#' @references API docs: https://cmu-delphi.github.io/delphi-epidata/api/sensors.html
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

#' Fetch HealthTweets data
#'
#' @examples
#' \donttest{
#' call <- pvt_twitter(auth = "yourkey", locations = "CA", epiweeks = epirange(201501, 202001))
#' fetch_tbl(call)
#' }
#' @param auth Character. The authentication key.
#' @param locations Character vector. The locations to fetch.
#' @param dates Epirange. The dates to fetch in the form epirange(startdate,enddate), where
#'   startdate and enddate are of the form YYYYMMDD (string or numeric). Mutually exclusive with `epiweeks`.
#' @param epiweeks Epirange. The epiweeks to fetch in the form epirange(startweek,endweek), where
#'   startweek and endweek are of the form YYYYWW (string or numeric). Mutually exclusive with `dates`.
#' @return an instance of epidata_call
#'
#' @references API docs: https://cmu-delphi.github.io/delphi-epidata/api/twitter.html
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

#' Fetch Wikipedia access data
#'
#' @examples
#' \donttest{
#' call <- wiki(articles = "avian_influenza", epiweeks = epirange(201501, 202001))
#' fetch_tbl(call)
#' }
#' @param articles Character vector. The articles to fetch.
#' @param dates Epirange. The dates to fetch in the form epirange(startdate,enddate), where
#'   startdate and enddate are of the form YYYYMMDD (string or numeric). Mutually exclusive with `epiweeks`.
#' @param epiweeks Epirange. The epiweeks to fetch in the form epirange(startweek,endweek), where
#'   startweek and endweek are of the form YYYYWW (string or numeric). Mutually exclusive with `dates`.
#' @param language Character. The language to fetch, in the form "en", "es", or "pt".
#' @param hours Integer. Optionally, the hours to fetch, in the range 0-23.
#' @return an instance of epidata_call
#'
#' @references API docs: https://cmu-delphi.github.io/delphi-epidata/api/wiki.html
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
