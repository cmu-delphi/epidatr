#' CDC total and by topic webpage visits
#'
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/cdc.html>
#'
#' @examples
#' \dontrun{
#' pvt_cdc(
#'   auth = Sys.getenv("SECRET_API_AUTH_CDC"),
#'   locations = "fl,ca",
#'   epirange(201501, 201601)
#' )
#' }
#'
#' @param auth string. Restricted access key (not the same as API key).
#' @param locations character. Locations to fetch.
#' @param epiweeks [`timeset`]. Epiweeks to fetch. Defaults to all ("*") dates.
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#'   See `fetch_args_list()` for details.
#' @return [`tibble::tibble`]
#'
#' @keywords endpoint
#' @export
pvt_cdc <- function(
    auth,
    locations,
    epiweeks = "*",
    fetch_args = fetch_args_list()) {
  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("auth", auth, len = 1)
  assert_character_param("locations", locations)
  assert_timeset_param("epiweeks", epiweeks)
  epiweeks <- parse_timeset_input(epiweeks)

  create_epidata_call(
    "cdc/",
    list(
      auth = auth,
      locations = locations,
      epiweeks = epiweeks
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
  ) %>% fetch(fetch_args = fetch_args)
}

#' Helper for finding COVID hospitalization facilities
#'
#' @description
#' API docs:
#' <https://cmu-delphi.github.io/delphi-epidata/api/covid_hosp_facility_lookup.html>
#'
#' Obtains unique identifiers and other metadata for COVID hospitalization
#' facilities of interest. This is a companion endpoint to the
#' [`pub_covid_hosp_facility()`] endpoint.
#'
#' @details Only one location argument needs to be specified.
#' Combinations of the arguments are not currently supported.
#'
#' @examples
#' \dontrun{
#' pub_covid_hosp_facility_lookup(state = "fl")
#' pub_covid_hosp_facility_lookup(city = "southlake")
#' }
#' @param ... not used for values, forces later arguments to bind by name
#' @param state string. A two-letter character state abbreviation.
#' @param ccn string. A facility CMS certification number.
#' @param city string. A city name.
#' @param zip string. A 5-digit zip code.
#' @param fips_code string. A 5-digit fips county code, zero-padded.
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#' @return [`tibble::tibble`]
#'
#' @seealso [`pub_covid_hosp_facility()`]
#' @keywords endpoint
#' @export
pub_covid_hosp_facility_lookup <- function(
    ...,
    state = NULL,
    ccn = NULL,
    city = NULL,
    zip = NULL,
    fips_code = NULL,
    fetch_args = fetch_args_list()) {
  rlang::check_dots_empty()

  assert_character_param("state", state, len = 1, required = FALSE)
  assert_character_param("ccn", ccn, len = 1, required = FALSE)
  assert_character_param("city", city, len = 1, required = FALSE)
  assert_character_param("zip", zip, len = 1, required = FALSE)
  assert_character_param("fips_code", fips_code, len = 1, required = FALSE)

  if (
    missing(state) &&
      missing(ccn) &&
      missing(city) &&
      missing(zip) &&
      missing(fips_code)
  ) {
    stop("one of `state`, `ccn`, `city`, `zip`, or `fips_code` is required")
  }

  if (sum(!missing(state), !missing(ccn), !missing(city), !missing(zip), !missing(fips_code)) > 1) {
    stop("only one of `state`, `ccn`, `city`, `zip`, or `fips_code` can be specified")
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
  ) %>% fetch(fetch_args = fetch_args)
}

#' COVID hospitalizations by facility
#'
#' @description
#' API docs:
#' <https://cmu-delphi.github.io/delphi-epidata/api/covid_hosp_facility.html>
#'
#' Obtains the COVID-19 reported patient impact and hospital capacity data by
#' facility. This dataset is provided by the US Department of Health & Human
#' Services. The companion function [`pub_covid_hosp_facility_lookup()`] can be
#' used to look up facility identifiers in a variety of ways.
#'
#' @details Starting October 1, 2022, some facilities are only required to
#' report annually.
#'
#' @examples
#' \dontrun{
#' pub_covid_hosp_facility(
#'   hospital_pks = "100075",
#'   collection_weeks = epirange(20200101, 20200501)
#' )
#'
#' pub_covid_hosp_facility(
#'   hospital_pks = "050063",
#'   collection_weeks = epirange(20240101, 20240301)
#' )
#' }
#' @param hospital_pks character. Facility identifiers.
#' @param collection_weeks [`timeset`]. Dates (corresponding to epiweeks) to
#'  fetch. Defaults to all ("*") dates.
#' @param ... not used for values, forces later arguments to bind by name
#' @param publication_dates [`timeset`]. Publication dates to fetch.
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#' @return [`tibble::tibble`]
#'
#' @importFrom checkmate test_class test_integerish test_character
#'
#' @seealso [`pub_covid_hosp_facility()`], [`epirange()`]
#' @keywords endpoint
#' @export
#
pub_covid_hosp_facility <- function(
    hospital_pks,
    collection_weeks = "*",
    ...,
    publication_dates = NULL,
    fetch_args = fetch_args_list()) {
  rlang::check_dots_empty()

  collection_weeks <- get_wildcard_equivalent_dates(collection_weeks, "day")

  assert_character_param("hospital_pks", hospital_pks)
  assert_timeset_param("collection_weeks", collection_weeks)
  assert_timeset_param("publication_dates", publication_dates, required = FALSE)
  collection_weeks <- parse_timeset_input(collection_weeks)
  publication_dates <- parse_timeset_input(publication_dates)

  # Confusingly, the endpoint expects `collection_weeks` to be in day format,
  # but correspond to epiweeks. Allow `collection_weeks` to be provided in
  # either day or week format.
  coercion_msg <- c(
    "`collection_weeks` is in week format but `pub_covid_hosp_facility`
       expects day format; dates will be converted to day format but may not
       correspond exactly to desired time range"
  )
  if (test_class(collection_weeks, "EpiRange") && nchar(collection_weeks$from) == 6) {
    cli::cli_warn(coercion_msg, class = "epidatr__epirange_week_coercion")
    collection_weeks <- reformat_epirange(collection_weeks, to_type = "day")
    # Single week date.
  } else if (
    (test_integerish(collection_weeks) || test_character(collection_weeks)) &&
      nchar(collection_weeks) == 6
  ) {
    cli::cli_warn(coercion_msg, class = "epidatr__single_week_coercion")
    collection_weeks <- parse_api_week(collection_weeks)
  }

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
      ),
      create_epidata_field_info("geocoded_hospital_address", "text"),
      create_epidata_field_info("hhs_ids", "text"),
      create_epidata_field_info("is_corrected", "bool"),
      create_epidata_field_info(
        "previous_day_admission_adult_covid_confirmed_7_day_coverage",
        "int"
      ),
      create_epidata_field_info(
        "previous_day_admission_adult_covid_suspected_7_day_coverage",
        "int"
      ),
      create_epidata_field_info(
        "previous_day_admission_pediatric_covid_confirmed_7_day_coverage",
        "int"
      ),
      create_epidata_field_info(
        "previous_day_admission_pediatric_covid_suspected_7_day_coverage",
        "int"
      ),
      create_epidata_field_info(
        "previous_week_patients_covid_vaccinated_doses_all_7_day",
        "int"
      ),
      create_epidata_field_info(
        "previous_week_patients_covid_vaccinated_doses_all_7_day_sum",
        "int"
      ),
      create_epidata_field_info(
        "previous_week_patients_covid_vaccinated_doses_one_7_day",
        "int"
      ),
      create_epidata_field_info(
        "previous_week_patients_covid_vaccinated_doses_one_7_day_sum",
        "int"
      ),
      create_epidata_field_info(
        "previous_week_personnel_covid_vaccd_doses_administered_7_day",
        "int"
      ),
      create_epidata_field_info(
        "previous_week_personnel_covid_vaccd_doses_administered_7_day_sum",
        "int"
      ),
      create_epidata_field_info("total_personnel_covid_vaccinated_doses_all_7_day", "int"),
      create_epidata_field_info(
        "total_personnel_covid_vaccinated_doses_all_7_day_sum",
        "int"
      ),
      create_epidata_field_info("total_personnel_covid_vaccinated_doses_none_7_day", "int"),
      create_epidata_field_info(
        "total_personnel_covid_vaccinated_doses_none_7_day_sum",
        "int"
      ),
      create_epidata_field_info("total_personnel_covid_vaccinated_doses_one_7_day", "int"),
      create_epidata_field_info(
        "total_personnel_covid_vaccinated_doses_one_7_day_sum",
        "int"
      )
    )
  ) %>% fetch(fetch_args = fetch_args)
}

#' COVID hospitalizations by state
#'
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/covid_hosp.html>.
#'
#' Obtains the COVID-19 reported patient impact and hospital capacity data by
#' state. This dataset is provided by the US Department of Health & Human
#' Services.
#'
#' @details Starting October 1, 2022, some facilities are only required to
#' report annually.
#'
#' @examples
#' \dontrun{
#' pub_covid_hosp_state_timeseries(
#'   states = "fl",
#'   dates = epirange(20200101, 20200501)
#' )
#' }
#'
#' @param states character. Two letter state abbreviations.
#' @param dates [`timeset`]. Dates to fetch. Defaults to all ("*") dates.
#' @param ... not used for values, forces later arguments to bind by name
#' @param as_of Date. Optionally, the as of date for the issues to fetch. If not
#'   specified, the most recent data is returned. Mutually exclusive with
#'   `issues`.
#' @param issues [`timeset`]. Optionally, the issue of the data to fetch. If not
#'   specified, the most recent issue is returned. Mutually exclusive with
#'   `as_of` or `lag`.
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#' @return [`tibble::tibble`]
#'
#' @keywords endpoint
#' @export
#
pub_covid_hosp_state_timeseries <- function(
    states,
    dates = "*",
    ...,
    as_of = NULL,
    issues = NULL,
    fetch_args = fetch_args_list()) {
  # Check parameters
  rlang::check_dots_empty()

  if (missing(states)) {
    cli::cli_abort(
      "`states` is required",
      class = "epidatr__pub_covid_hosp_state_timeseries__missing_required_args"
    )
  }

  if (sum(!is.null(issues), !is.null(as_of)) > 1) {
    stop("`issues`and `as_of` are mutually exclusive")
  }

  dates <- get_wildcard_equivalent_dates(dates, "day")

  assert_character_param("states", states)
  assert_timeset_param("dates", dates)
  assert_date_param("as_of", as_of, len = 1, required = FALSE)
  assert_timeset_param("issues", issues, required = FALSE)

  dates <- parse_timeset_input(dates)
  issues <- parse_timeset_input(issues)
  as_of <- parse_timeset_input(as_of)

  create_epidata_call(
    "covid_hosp_state_timeseries/",
    list(
      states = states,
      dates = dates,
      issues = issues,
      as_of = as_of
    ),
    list(
      create_epidata_field_info("state", "text"),
      create_epidata_field_info("issue", "date"),
      create_epidata_field_info("date", "date"),
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
      create_epidata_field_info("adult_icu_bed_utilization", "float"),
      create_epidata_field_info("geocoded_state", "text"),
      create_epidata_field_info("deaths_covid", "int"),
      create_epidata_field_info("deaths_covid_coverage", "int"),
      create_epidata_field_info("icu_patients_confirmed_influenza", "int"),
      create_epidata_field_info("icu_patients_confirmed_influenza_coverage", "int"),
      create_epidata_field_info(
        "on_hand_supply_therapeutic_a_casirivimab_imdevimab_courses",
        "int"
      ),
      create_epidata_field_info("on_hand_supply_therapeutic_b_bamlanivimab_courses", "int"),
      create_epidata_field_info(
        "on_hand_supply_therapeutic_c_bamlanivimab_etesevimab_courses",
        "int"
      ),
      create_epidata_field_info("previous_day_admission_adult_covid_confirmed_18_19", "int"),
      create_epidata_field_info(
        "previous_day_admission_adult_covid_confirmed_18_19_coverage",
        "int"
      ),
      create_epidata_field_info("previous_day_admission_adult_covid_confirmed_20_29", "int"),
      create_epidata_field_info(
        "previous_day_admission_adult_covid_confirmed_20_29_coverage",
        "int"
      ),
      create_epidata_field_info("previous_day_admission_adult_covid_confirmed_30_39", "int"),
      create_epidata_field_info(
        "previous_day_admission_adult_covid_confirmed_30_39_coverage",
        "int"
      ),
      create_epidata_field_info("previous_day_admission_adult_covid_confirmed_40_49", "int"),
      create_epidata_field_info(
        "previous_day_admission_adult_covid_confirmed_40_49_coverage",
        "int"
      ),
      create_epidata_field_info("previous_day_admission_adult_covid_confirmed_50_59", "int"),
      create_epidata_field_info(
        "previous_day_admission_adult_covid_confirmed_50_59_coverage",
        "int"
      ),
      create_epidata_field_info("previous_day_admission_adult_covid_confirmed_60_69", "int"),
      create_epidata_field_info(
        "previous_day_admission_adult_covid_confirmed_60_69_coverage",
        "int"
      ),
      create_epidata_field_info("previous_day_admission_adult_covid_confirmed_70_79", "int"),
      create_epidata_field_info(
        "previous_day_admission_adult_covid_confirmed_70_79_coverage",
        "int"
      ),
      create_epidata_field_info(
        "previous_day_admission_adult_covid_confirmed_80plus",
        "int"
      ),
      create_epidata_field_info(
        "previous_day_admission_adult_covid_confirmed_80plus_coverage",
        "int"
      ),
      create_epidata_field_info(
        "previous_day_admission_adult_covid_confirmed_unknown",
        "int"
      ),
      create_epidata_field_info(
        "previous_day_admission_adult_covid_confirmed_unknown_coverage",
        "int"
      ),
      create_epidata_field_info("previous_day_admission_adult_covid_suspected_18_19", "int"),
      create_epidata_field_info(
        "previous_day_admission_adult_covid_suspected_18_19_coverage",
        "int"
      ),
      create_epidata_field_info("previous_day_admission_adult_covid_suspected_20_29", "int"),
      create_epidata_field_info(
        "previous_day_admission_adult_covid_suspected_20_29_coverage",
        "int"
      ),
      create_epidata_field_info("previous_day_admission_adult_covid_suspected_30_39", "int"),
      create_epidata_field_info(
        "previous_day_admission_adult_covid_suspected_30_39_coverage",
        "int"
      ),
      create_epidata_field_info("previous_day_admission_adult_covid_suspected_40_49", "int"),
      create_epidata_field_info(
        "previous_day_admission_adult_covid_suspected_40_49_coverage",
        "int"
      ),
      create_epidata_field_info("previous_day_admission_adult_covid_suspected_50_59", "int"),
      create_epidata_field_info(
        "previous_day_admission_adult_covid_suspected_50_59_coverage",
        "int"
      ),
      create_epidata_field_info("previous_day_admission_adult_covid_suspected_60_69", "int"),
      create_epidata_field_info(
        "previous_day_admission_adult_covid_suspected_60_69_coverage",
        "int"
      ),
      create_epidata_field_info("previous_day_admission_adult_covid_suspected_70_79", "int"),
      create_epidata_field_info(
        "previous_day_admission_adult_covid_suspected_70_79_coverage",
        "int"
      ),
      create_epidata_field_info(
        "previous_day_admission_adult_covid_suspected_80plus",
        "int"
      ),
      create_epidata_field_info(
        "previous_day_admission_adult_covid_suspected_80plus_coverage",
        "int"
      ),
      create_epidata_field_info(
        "previous_day_admission_adult_covid_suspected_unknown",
        "int"
      ),
      create_epidata_field_info(
        "previous_day_admission_adult_covid_suspected_unknown_coverage",
        "int"
      ),
      create_epidata_field_info("previous_day_admission_influenza_confirmed", "int"),
      create_epidata_field_info(
        "previous_day_admission_influenza_confirmed_coverage",
        "int"
      ),
      create_epidata_field_info("previous_day_deaths_covid_and_influenza", "int"),
      create_epidata_field_info("previous_day_deaths_covid_and_influenza_coverage", "int"),
      create_epidata_field_info("previous_day_deaths_influenza", "int"),
      create_epidata_field_info("previous_day_deaths_influenza_coverage", "int"),
      create_epidata_field_info(
        "previous_week_therapeutic_a_casirivimab_imdevimab_courses_used",
        "int"
      ),
      create_epidata_field_info(
        "previous_week_therapeutic_b_bamlanivimab_courses_used",
        "int"
      ),
      create_epidata_field_info(
        "previous_week_therapeutic_c_bamlanivimab_etesevimab_courses_used",
        "int"
      ),
      create_epidata_field_info(
        "total_patients_hospitalized_confirmed_influenza_covid",
        "int"
      ),
      create_epidata_field_info(
        "total_patients_hospitalized_confirmed_influenza_covid_coverage",
        "int"
      ),
      create_epidata_field_info("total_patients_hospitalized_confirmed_influenza", "int"),
      create_epidata_field_info(
        "total_patients_hospitalized_confirmed_influenza_coverage",
        "int"
      )
    )
  ) %>% fetch(fetch_args = fetch_args)
}

#' Metadata for the COVIDcast endpoint
#'
#' @description
#' API docs:
#' <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_meta.html>.
#'
#' Fetch a summary of metadata for all sources and signals that are available in
#' the API, along with basic summary statistics such as the dates they are
#' available, the geographic levels at which they are reported, and etc.
#'
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#'
#' @return [`tibble::tibble`]
#'
#' @examples
#' \dontrun{
#' pub_covidcast_meta()
#' }
#'
#' @seealso [pub_covidcast()],[covidcast_epidata()]
#' @keywords endpoint
#' @export
pub_covidcast_meta <- function(fetch_args = fetch_args_list()) {
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
      create_epidata_field_info(
        "geo_type",
        "categorical",
        categories = c("nation", "msa", "hrr", "hhs", "state", "county", "dma")
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
  ) %>% fetch(fetch_args = fetch_args)
}

#' Various COVID and flu signals via the COVIDcast endpoint
#'
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html>
#'
#' The primary endpoint for fetching COVID-19 data, providing access to a wide
#' variety of signals from a wide variety of sources. See the API documentation
#' link above for more. Delphi's [COVIDcast public
#' dashboard](https://delphi.cmu.edu/covidcast/) is powered by this endpoint.
#'
#' @examples
#' \dontrun{
#' pub_covidcast(
#'   source = "jhu-csse",
#'   signals = "confirmed_7dav_incidence_prop",
#'   geo_type = "state",
#'   time_type = "day",
#'   geo_values = c("ca", "fl"),
#'   time_values = epirange(20200601, 20200801)
#' )
#' pub_covidcast(
#'   source = "jhu-csse",
#'   signals = "confirmed_7dav_incidence_prop",
#'   geo_type = "state",
#'   time_type = "day",
#'   geo_values = "*",
#'   time_values = epirange(20200601, 20200801)
#' )
#' }
#'
#' @param source string. The data source to query (see:
#'   <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html>).
#' @param signals string. The signals to query from a specific source (see:
#'   <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html>).
#' @param geo_type string. The geographic resolution of the data (see:
#'   <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html>).
#' @param time_type string. The temporal resolution of the data (either "day" or
#' "week", depending on signal).
#' @param geo_values character. The geographies to return. Defaults to all
#'  ("*") geographies within requested geographic resolution (see:
#'  <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html>.).
#' @param time_values [`timeset`]. Dates to fetch. Defaults to all ("*") dates.
#' @param ... not used for values, forces later arguments to bind by name
#' @param as_of Date. Optionally, the as of date for the issues to fetch. If not
#'   specified, the most recent data is returned. Mutually exclusive with
#'   `issues` or `lag`.
#' @param issues [`timeset`]. Optionally, the issue of the data to fetch. If not
#'   specified, the most recent issue is returned. Mutually exclusive with
#'   `as_of` or `lag`.
#' @param lag integer. Optionally, the lag of the issues to fetch. If not set,
#'   the most recent issue is returned. Mutually exclusive with `as_of` or
#'   `issues`.
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#' @return [`tibble::tibble`]
#'
#' @seealso [pub_covidcast_meta()], [covidcast_epidata()], [epirange()]
#' @keywords endpoint
#' @export
pub_covidcast <- function(
    source,
    signals,
    geo_type,
    time_type,
    geo_values = "*",
    time_values = "*",
    ...,
    as_of = NULL,
    issues = NULL,
    lag = NULL,
    fetch_args = fetch_args_list()) {
  rlang::check_dots_empty()

  # Check parameters
  if (
    missing(source) ||
      missing(signals) ||
      missing(time_type) ||
      missing(geo_type)
  ) {
    cli::cli_abort(
      "`source`, `signals`, `time_type`, and `geo_type` are all required",
      class = "epidatr__pub_covidcast__missing_required_args"
    )
  }

  if (sum(!is.null(issues), !is.null(lag), !is.null(as_of)) > 1) {
    cli::cli_abort(
      "`issues`, `lag`, and `as_of` are mutually exclusive",
      class = "epidatr__pub_covidcast__too_many_issue_params"
    )
  }

  assert_character_param("data_source", source, len = 1)
  assert_character_param("signals", signals)
  assert_character_param("time_type", time_type, len = 1)
  assert_character_param("geo_type", geo_type, len = 1)
  assert_timeset_param("time_values", time_values)
  assert_character_param("geo_values", geo_values)
  assert_date_param("as_of", as_of, len = 1, required = FALSE)
  assert_timeset_param("issues", issues, required = FALSE)
  assert_integerish_param("lag", lag, len = 1, required = FALSE)
  time_values <- parse_timeset_input(time_values)
  as_of <- parse_timeset_input(as_of)
  issues <- parse_timeset_input(issues)

  if (source == "nchs-mortality" && time_type != "week") {
    cli::cli_abort(
      "{source} data is only available at the week level",
      class = "epidatr__nchs_week_only"
    )
  }

  create_epidata_call(
    "covidcast/",
    list(
      data_source = source,
      signals = signals,
      geo_type = geo_type,
      time_type = time_type,
      geo_values = geo_values,
      time_values = time_values,
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
      create_epidata_field_info("time_type", "categorical",
        categories =
          c("day", "week")
      ),
      create_epidata_field_info("geo_value", "text"),
      create_epidata_field_info("time_value", switch(time_type,
        day = "date",
        week = "epiweek"
      )),
      create_epidata_field_info("issue", switch(time_type,
        day = "date",
        week = "epiweek"
      )),
      create_epidata_field_info("lag", "int"),
      create_epidata_field_info("value", "float"),
      create_epidata_field_info("stderr", "float"),
      create_epidata_field_info("sample_size", "float"),
      create_epidata_field_info("direction", "float"),
      create_epidata_field_info("missing_value", "int"),
      create_epidata_field_info("missing_stderr", "int"),
      create_epidata_field_info("missing_sample_size", "int")
    )
  ) %>% fetch(fetch_args = fetch_args)
}

#' Delphi's ILINet outpatient doctor visits forecasts
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/delphi.html>
#'
#' @examples
#' \dontrun{
#' pub_delphi(system = "ec", epiweek = 201501)
#' }
#' @param system character. System name to fetch.
#' @param epiweek [`timeset`]. Epiweek to fetch. Does not support multiple dates.
#'  Make separate calls to fetch data for multiple epiweeks.
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#' @return [`list`]
#' @keywords endpoint
#' @export
pub_delphi <- function(
    system,
    epiweek,
    fetch_args = fetch_args_list()) {
  assert_character_param("system", system)
  assert_timeset_param("epiweek", epiweek, len = 1)
  epiweek <- parse_timeset_input(epiweek)

  create_epidata_call(
    "delphi/",
    list(system = system, epiweek = epiweek),
    list(
      create_epidata_field_info("system", "text"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("json", "text")
    ),
    only_supports_classic = TRUE
  ) %>% fetch(fetch_args = fetch_args)
}

#' Delphi's PAHO dengue nowcasts (North and South America)
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/dengue_nowcast.html>
#'
#' @examples
#' \dontrun{
#' pub_dengue_nowcast(
#'   locations = "pr",
#'   epiweeks = epirange(201401, 202301)
#' )
#' }
#' @param locations character. Locations to fetch.
#' @param epiweeks [`timeset`]. Epiweeks to fetch. Defaults to all ("*") dates.
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#' @return [`tibble::tibble`]
#' @keywords endpoint
#' @export
pub_dengue_nowcast <- function(
    locations,
    epiweeks = "*",
    fetch_args = fetch_args_list()) {
  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("locations", locations)
  assert_timeset_param("epiweeks", epiweeks)
  epiweeks <- parse_timeset_input(epiweeks)

  create_epidata_call(
    "dengue_nowcast/",
    list(locations = locations, epiweeks = epiweeks),
    list(
      create_epidata_field_info("location", "text"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("value", "float"),
      create_epidata_field_info("std", "float")
    )
  ) %>% fetch(fetch_args = fetch_args)
}

#' PAHO dengue digital surveillance sensors (North and South America)
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/dengue_sensors.html>
#'
#' @examples
#' \dontrun{
#' pvt_dengue_sensors(
#'   auth = Sys.getenv("SECRET_API_AUTH_SENSORS"),
#'   names = "ght",
#'   locations = "ag",
#'   epiweeks = epirange(201501, 202001)
#' )
#' }
#' @param auth string. Restricted access key (not the same as API key).
#' @param names character. Names to fetch.
#' @param locations character. Locations to fetch.
#' @param epiweeks [`timeset`]. Epiweeks to fetch. Defaults to all ("*") dates.
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#' @return [`tibble::tibble`]
#' @keywords endpoint
#' @export
pvt_dengue_sensors <- function(
    auth,
    names,
    locations,
    epiweeks = "*",
    fetch_args = fetch_args_list()) {
  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("auth", auth, len = 1)
  assert_character_param("names", names)
  assert_character_param("locations", locations)
  assert_timeset_param("epiweeks", epiweeks)
  epiweeks <- parse_timeset_input(epiweeks)

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
  ) %>% fetch(fetch_args = fetch_args)
}

#' ECDC ILI incidence (Europe)
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/ecdc_ili.html>.
#'
#' Obtain information on influenza-like-illness from the European Centre for
#' Disease Prevention and Control.
#'
#'
#' @details The list of location argument can be found in
#' <https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/ecdc_regions.txt>.
#'
#' @examples
#' \dontrun{
#' pub_ecdc_ili(regions = "austria", epiweeks = epirange(201901, 202001))
#' }
#' @param regions character. Regions to fetch.
#' @param epiweeks [`timeset`]. Epiweeks to fetch. Defaults to all ("*") dates.
#' @param ... not used for values, forces later arguments to bind by name
#' @param issues [`timeset`]. Optionally, the issues to fetch. If not set, the
#'   most recent issue is returned. Mutually exclusive with `lag`.
#' @param lag integer. Optionally, the lag of the issues to fetch. If not set,
#'   the most recent issue is returned. Mutually exclusive with `issues`.
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#' @return [`tibble::tibble`]
#' @keywords endpoint
#' @export
pub_ecdc_ili <- function(
    regions,
    epiweeks = "*",
    ...,
    issues = NULL,
    lag = NULL,
    fetch_args = fetch_args_list()) {
  rlang::check_dots_empty()

  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("regions", regions)
  assert_timeset_param("epiweeks", epiweeks)
  assert_timeset_param("issues", issues, required = FALSE)
  assert_integerish_param("lag", lag, len = 1, required = FALSE)
  epiweeks <- parse_timeset_input(epiweeks)
  issues <- parse_timeset_input(issues)

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
      create_epidata_field_info("issue", "epiweek"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("lag", "int"),
      create_epidata_field_info("incidence_rate", "float")
    )
  ) %>% fetch(fetch_args = fetch_args)
}

#' CDC FluSurv flu hospitalizations
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/flusurv.html>.
#'
#' Obtain information on influenza hospitalization rates from the Center of Disease
#' Control.
#'
#' See also <https://gis.cdc.gov/GRASP/Fluview/FluHospRates.html>.
#'
#' @details The list of location argument can be found in
#' <https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/flusurv_locations.txt>.
#'
#' @examples
#' \dontrun{
#' pub_flusurv(locations = "CA", epiweeks = epirange(201701, 201801))
#' }
#' @param locations character. Character vector indicating location.
#' @param epiweeks [`timeset`]. Epiweeks to fetch. Defaults to all ("*") dates.
#' @param ... not used for values, forces later arguments to bind by name
#' @param issues [`timeset`]. Optionally, the issues to fetch. If not set, the
#'   most recent issue is returned. Mutually exclusive with `lag`.
#' @param lag integer. Optionally, the lag of the issues to fetch. If not set,
#'   the most recent issue is returned. Mutually exclusive with `issues`.
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#' @return [`tibble::tibble`]
#' @keywords endpoint
#' @export
pub_flusurv <- function(
    locations,
    epiweeks = "*",
    ...,
    issues = NULL,
    lag = NULL,
    fetch_args = fetch_args_list()) {
  rlang::check_dots_empty()

  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("locations", locations)
  assert_timeset_param("epiweeks", epiweeks)
  assert_timeset_param("issues", issues, required = FALSE)
  assert_integerish_param("lag", lag, len = 1, required = FALSE)
  epiweeks <- parse_timeset_input(epiweeks)
  issues <- parse_timeset_input(issues)

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
      create_epidata_field_info("release_date", "date"),
      create_epidata_field_info("location", "text"),
      create_epidata_field_info("issue", "epiweek"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("lag", "int"),
      create_epidata_field_info("rate_age_0", "float"),
      create_epidata_field_info("rate_age_1", "float"),
      create_epidata_field_info("rate_age_2", "float"),
      create_epidata_field_info("rate_age_3", "float"),
      create_epidata_field_info("rate_age_4", "float"),
      create_epidata_field_info("rate_overall", "float")
    )
  ) %>% fetch(fetch_args = fetch_args)
}

#' CDC FluView flu tests from clinical labs
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/fluview_clinical.html>
#'
#' @examples
#' \dontrun{
#' pub_fluview_clinical(regions = "nat", epiweeks = epirange(201601, 201701))
#' }
#' @param regions character. Regions to fetch.
#' @param epiweeks [`timeset`]. Epiweeks to fetch in the form
#'   epirange(startweek,endweek), where startweek and endweek are of the form
#'   YYYYWW (string or numeric). Defaults to all ("*") dates.
#' @param ... not used for values, forces later arguments to bind by name
#' @param issues [`timeset`]. Optionally, the issues to fetch. If not set, the
#'   most recent issue is returned. Mutually exclusive with `lag`.
#' @param lag integer. Optionally, the lag of the issues to fetch. If not set,
#'   the most recent issue is returned. Mutually exclusive with `issues`.
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#' @return [`tibble::tibble`]
#' @keywords endpoint
#' @export
pub_fluview_clinical <- function(
    regions,
    epiweeks = "*",
    ...,
    issues = NULL,
    lag = NULL,
    fetch_args = fetch_args_list()) {
  rlang::check_dots_empty()

  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("regions", regions)
  assert_timeset_param("epiweeks", epiweeks)
  assert_timeset_param("issues", issues, required = FALSE)
  assert_integerish_param("lag", lag, len = 1, required = FALSE)
  epiweeks <- parse_timeset_input(epiweeks)
  issues <- parse_timeset_input(issues)

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
      create_epidata_field_info("release_date", "date"),
      create_epidata_field_info("region", "text"),
      create_epidata_field_info("issue", "epiweek"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("lag", "int"),
      create_epidata_field_info("total_specimens", "int"),
      create_epidata_field_info("total_a", "int"),
      create_epidata_field_info("total_b", "int"),
      create_epidata_field_info("percent_positive", "float"),
      create_epidata_field_info("percent_a", "float"),
      create_epidata_field_info("percent_b", "float")
    )
  ) %>% fetch(fetch_args = fetch_args)
}

#' Metadata for the FluView endpoint
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/fluview_meta.html>
#'
#' @examples
#' \dontrun{
#' pub_fluview_meta()
#' }
#'
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#'
#' @return [`tibble::tibble`]
#' @seealso [`pub_fluview()`]
#' @keywords endpoint
#' @export
pub_fluview_meta <- function(fetch_args = fetch_args_list()) {
  create_epidata_call(
    "fluview_meta/",
    list(),
    list(
      create_epidata_field_info("latest_update", "date"),
      create_epidata_field_info("latest_issue", "epiweek"),
      create_epidata_field_info("table_rows", "int")
    )
  ) %>% fetch(fetch_args = fetch_args)
}


#' CDC FluView ILINet outpatient doctor visits
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/fluview.html>. For
#'
#' Obtains information on outpatient inluenza-like-illness (ILI) from U.S.
#'   Outpatient Influenza-like Illness Surveillance Network (ILINet).
#'
#' more information on ILINet, see
#' <https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html>.
#'
#' @details The full list of location inputs can be accessed at
#'   <https://github.com/cmu-delphi/delphi-epidata/blob/main/src/acquisition/fluview/fluview_locations.py>.
#'
#' @examples
#' \dontrun{
#' pub_fluview(regions = "nat", epiweeks = epirange(201201, 202005))
#' }
#' @param regions character. Locations to fetch. Can be any string IDs in
#'   national, HHS region, census division, most states and territories, and so
#'   on. Full list link below.
#' @param epiweeks [`timeset`]. Epiweeks to fetch in the form
#'   `epirange(startweek, endweek)`, where startweek and endweek are of the form
#'   YYYYWW (string or numeric). Defaults to all ("*") dates.
#' @param ... not used for values, forces later arguments to bind by name
#' @param issues [`timeset`]. Optionally, the issues to fetch. If not set, the
#'   most recent issue is returned. Mutually exclusive with `lag`.
#' @param lag integer. Optionally, the lag of the issues to fetch. If not set,
#'   the most recent issue is returned. Mutually exclusive with `issues`.
#' @param auth string. Optionally, restricted access key (not the same as API
#' key).
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#' @return [`tibble::tibble`]
#' @keywords endpoint
#' @export
pub_fluview <- function(
    regions,
    epiweeks = "*",
    ...,
    issues = NULL,
    lag = NULL,
    auth = NULL,
    fetch_args = fetch_args_list()) {
  rlang::check_dots_empty()

  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("regions", regions)
  assert_timeset_param("epiweeks", epiweeks)
  assert_timeset_param("issues", issues, required = FALSE)
  assert_integerish_param("lag", lag, len = 1, required = FALSE)
  assert_character_param("auth", auth, len = 1, required = FALSE)
  epiweeks <- parse_timeset_input(epiweeks)
  issues <- parse_timeset_input(issues)

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
      create_epidata_field_info("release_date", "date"),
      create_epidata_field_info("region", "text"),
      create_epidata_field_info("issue", "epiweek"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("lag", "int"),
      create_epidata_field_info("num_ili", "int"),
      create_epidata_field_info("num_patients", "int"),
      create_epidata_field_info("num_providers", "int"),
      create_epidata_field_info("num_age_0", "int"),
      create_epidata_field_info("num_age_1", "int"),
      create_epidata_field_info("num_age_2", "int"),
      create_epidata_field_info("num_age_3", "int"),
      create_epidata_field_info("num_age_4", "int"),
      create_epidata_field_info("num_age_5", "int"),
      create_epidata_field_info("wili", "float"),
      create_epidata_field_info("ili", "float")
    )
  ) %>% fetch(fetch_args = fetch_args)
}

#' Google Flu Trends flu search volume
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/gft.html>
#'
#' Obtains estimates of inluenza activity based on volume of certain search
#' queries from Google.
#'
#'
#' @details Google has discontinued Flu Trends and this is now a static
#'   endpoint. Possibile input for locations can be found in
#'   <https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/regions.txt>,
#'   <https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/states.txt>,
#'   and
#'   <https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/cities.txt>.
#'
#' @examples
#' \dontrun{
#' pub_gft(locations = "hhs1", epiweeks = epirange(201201, 202001))
#' }
#' @param locations character. Locations to fetch.
#' @param epiweeks [`timeset`] Epiweeks to fetch. Defaults to all ("*") dates.
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#'
#' @return [`tibble::tibble`]
#' @keywords endpoint
#' @export
pub_gft <- function(
    locations,
    epiweeks = "*",
    fetch_args = fetch_args_list()) {
  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("locations", locations)
  assert_timeset_param("epiweeks", epiweeks)
  epiweeks <- parse_timeset_input(epiweeks)

  create_epidata_call(
    "gft/",
    list(locations = locations, epiweeks = epiweeks),
    list(
      create_epidata_field_info("location", "text"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("num", "int")
    )
  ) %>% fetch(fetch_args = fetch_args)
}

#' Google Health Trends health topics search volume
#'
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/ght.html>
#'
#' Estimate of influenza activity based on volume of certain search queries. …
#'
#' @examples
#' \dontrun{
#' pvt_ght(
#'   auth = Sys.getenv("SECRET_API_AUTH_GHT"),
#'   locations = "ma",
#'   epiweeks = epirange(199301, 202304),
#'   query = "how to get over the flu"
#' )
#' }
#' @param auth string. Restricted access key (not the same as API key).
#' @param locations character. Locations to fetch.
#' @param epiweeks [`timeset`]. Epiweeks to fetch. Defaults to all ("*") dates.
#' @param query string. The query to be fetched.
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#' @return [`tibble::tibble`]
#' @keywords endpoint
#' @export
pvt_ght <- function(
    auth,
    locations,
    epiweeks = "*",
    query,
    fetch_args = fetch_args_list()) {
  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("auth", auth, len = 1)
  assert_character_param("locations", locations)
  assert_timeset_param("epiweeks", epiweeks)
  assert_character_param("query", query, len = 1)
  epiweeks <- parse_timeset_input(epiweeks)

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
  ) %>% fetch(fetch_args = fetch_args)
}

#' KCDC ILI incidence (Korea)
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/kcdc_ili.html>
#'
#' @examples
#' \dontrun{
#' pub_kcdc_ili(regions = "ROK", epiweeks = 200436)
#' }
#' @param regions character. Regions to fetch.
#' @param epiweeks [`timeset`]. Epiweeks to fetch. Defaults to all ("*") dates.
#' @param ... not used for values, forces later arguments to bind by name
#' @param issues [`timeset`]. Optionally, the issues to fetch. If not set, the
#'   most recent issue is returned. Mutually exclusive with `lag`.
#' @param lag integer. Optionally, the lag of the issues to fetch. If not set,
#'   the most recent issue is returned. Mutually exclusive with `issues`.
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#' @return [`tibble::tibble`]
#' @keywords endpoint
#' @export
pub_kcdc_ili <- function(
    regions,
    epiweeks = "*",
    ...,
    issues = NULL,
    lag = NULL,
    fetch_args = fetch_args_list()) {
  rlang::check_dots_empty()

  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("regions", regions)
  assert_timeset_param("epiweeks", epiweeks)
  assert_timeset_param("issues", issues, required = FALSE)
  assert_integerish_param("lag", lag, len = 1, required = FALSE)
  epiweeks <- parse_timeset_input(epiweeks)
  issues <- parse_timeset_input(issues)

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
      create_epidata_field_info("release_date", "date"),
      create_epidata_field_info("region", "text"),
      create_epidata_field_info("issue", "epiweek"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("lag", "int"),
      create_epidata_field_info("ili", "float")
    )
  ) %>% fetch(fetch_args = fetch_args)
}

#' Metadata for the NoroSTAT endpoint
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/meta_norostat.html>
#'
#' @examples
#' \dontrun{
#' pvt_meta_norostat(auth = Sys.getenv("SECRET_API_AUTH_NOROSTAT"))
#' }
#' @param auth string. Restricted access key (not the same as API key).
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#' @return [`list`]
#' @seealso [`pvt_norostat()`]
#' @keywords endpoint
#' @export
pvt_meta_norostat <- function(auth, fetch_args = fetch_args_list()) {
  assert_character_param("auth", auth, len = 1)

  create_epidata_call(
    "meta_norostat/",
    list(auth = auth),
    only_supports_classic = TRUE
  ) %>% fetch(fetch_args = fetch_args)
}

#' Metadata for the Delphi Epidata API
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/meta.html>
#'
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#'
#' @return [`list`]
#' @keywords endpoint
#' @export
pub_meta <- function(fetch_args = fetch_args_list()) {
  create_epidata_call("meta/", list(), only_supports_classic = TRUE) %>% fetch(fetch_args = fetch_args)
}

#' NIDSS dengue cases (Taiwan)
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/nidss_dengue.html>
#'
#' Obtains counts of confirmed dengue cases in Taiwan from Taiwan National
#' Infectious Disease Statistical System.
#'
#'
#' @details Possible location inputs can be found in
#' <https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/nidss_regions.txt>
#' and
#' <https://github.com/cmu-delphi/delphi-epidata/blob/main/labels/nidss_locations.txt>.
#'
#' @examples
#' \dontrun{
#' pub_nidss_dengue(locations = "taipei", epiweeks = epirange(201201, 201301))
#' }
#' @param locations character. Locations to fetch.
#' @param epiweeks [`timeset`]. Epiweeks to fetch. Defaults to all ("*") dates.
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#'
#' @return [`tibble::tibble`]
#' @keywords endpoint
#' @export
pub_nidss_dengue <- function(
    locations,
    epiweeks = "*",
    fetch_args = fetch_args_list()) {
  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("locations", locations)
  assert_timeset_param("epiweeks", epiweeks)
  epiweeks <- parse_timeset_input(epiweeks)

  create_epidata_call(
    "nidss_dengue/",
    list(locations = locations, epiweeks = epiweeks),
    list(
      create_epidata_field_info("location", "text"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("count", "int")
    )
  ) %>% fetch(fetch_args = fetch_args)
}

#' NIDSS flu doctor visits (Taiwan)
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/nidss_flu.html>
#'
#' Obtains information on outpatient inluenza-like-illness from Taiwan National
#' Infectious Disease Statistical System.
#'
#'
#' @examples
#' \dontrun{
#' pub_nidss_flu(regions = "taipei", epiweeks = epirange(201501, 201601))
#' }
#' @param regions character. Regions to fetch.
#' @param epiweeks [`timeset`]. Epiweeks to fetch. Defaults to all ("*") dates.
#' @param ... not used for values, forces later arguments to bind by name
#' @param issues [`timeset`]. Optionally, the issues to fetch. If not set, the
#'   most recent issue is returned. Mutually exclusive with `lag`.
#' @param lag integer. Optionally, the lag of the issues to fetch. If not set,
#'   the most recent issue is returned. Mutually exclusive with `issues`.
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#' @return [`tibble::tibble`]
#' @keywords endpoint
#' @export
pub_nidss_flu <- function(
    regions,
    epiweeks = "*",
    ...,
    issues = NULL,
    lag = NULL,
    fetch_args = fetch_args_list()) {
  rlang::check_dots_empty()

  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("regions", regions)
  assert_timeset_param("epiweeks", epiweeks)
  assert_timeset_param("issues", issues, required = FALSE)
  assert_integerish_param("lag", lag, len = 1, required = FALSE)
  epiweeks <- parse_timeset_input(epiweeks)
  issues <- parse_timeset_input(issues)

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
      create_epidata_field_info("release_date", "date"),
      create_epidata_field_info("region", "text"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("issue", "epiweek"),
      create_epidata_field_info("lag", "int"),
      create_epidata_field_info("visits", "int"),
      create_epidata_field_info("ili", "float")
    )
  ) %>% fetch(fetch_args = fetch_args)
}


#' CDC NoroSTAT norovirus outbreaks
#' @description
#' This is point data only, and does not include minima or maxima.
#'
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/norostat.html>
#'
#' This is the documentation of the API for accessing the NoroSTAT endpoint of
#'   the Delphi’s epidemiological data.
#'
#' @examples
#' \dontrun{
#' pvt_norostat(
#'   auth = Sys.getenv("SECRET_API_AUTH_NOROSTAT"),
#'   locations = "Minnesota, Ohio, Oregon, Tennessee, and Wisconsin",
#'   epiweeks = 201233
#' )
#' }
#' @param auth string. Your authentication key.
#' @param locations character. Locations to fetch.
#' @param epiweeks [`timeset`]. Epiweeks to fetch. Defaults to all ("*") dates.
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#' @return [`tibble::tibble`]
#' @keywords endpoint
#' @export
pvt_norostat <- function(
    auth,
    locations,
    epiweeks = "*",
    fetch_args = fetch_args_list()) {
  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("auth", auth, len = 1)
  assert_character_param("locations", locations, len = 1)
  assert_timeset_param("epiweeks", epiweeks)
  epiweeks <- parse_timeset_input(epiweeks)

  create_epidata_call(
    "norostat/",
    list(
      auth = auth,
      location = locations,
      epiweeks = epiweeks
    ),
    list(
      create_epidata_field_info("release_date", "date"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("value", "int")
    )
  ) %>% fetch(fetch_args = fetch_args)
}

#' Delphi's ILI Nearby nowcasts
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/nowcast.html>.
#'
#' Obtains information on outpatient inluenza-like-illness (ILI) from Delphi's
#'
#' @details The full list of location inputs can be accessed at
#' <https://github.com/cmu-delphi/delphi-epidata/blob/main/src/acquisition/fluview/fluview_locations.py>.
#'
#' @examples
#' \dontrun{
#' pub_nowcast(locations = "ca", epiweeks = epirange(201201, 201301))
#' }
#' @param locations character. Locations to fetch.
#' @param epiweeks [`timeset`]. Epiweeks to fetch. Defaults to all ("*") dates.
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#' @return [`tibble::tibble`]
#' @keywords endpoint
#' @export
pub_nowcast <- function(
    locations,
    epiweeks = "*",
    fetch_args = fetch_args_list()) {
  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("locations", locations)
  assert_timeset_param("epiweeks", epiweeks)
  epiweeks <- parse_timeset_input(epiweeks)

  create_epidata_call(
    "nowcast/",
    list(locations = locations, epiweeks = epiweeks),
    list(
      create_epidata_field_info("location", "text"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("value", "float"),
      create_epidata_field_info("std", "float")
    )
  ) %>% fetch(fetch_args = fetch_args)
}

#' PAHO dengue data (North and South America)
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/paho_dengue.html>
#'
#' @examples
#' \dontrun{
#' pub_paho_dengue(regions = "ca", epiweeks = epirange(201401, 201501))
#' }
#' @param regions character. Regions to fetch.
#' @param epiweeks [`timeset`]. Epiweeks to fetch. Defaults to all ("*") dates.
#' @param ... not used for values, forces later arguments to bind by name
#' @param issues [`timeset`]. Optionally, the issues to fetch. If not set, the
#'   most recent issue is returned. Mutually exclusive with `lag`.
#' @param lag integer. Optionally, the lag of the issues to fetch. If not set,
#'   the most recent issue is returned. Mutually exclusive with `issues`.
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#' @return [`tibble::tibble`]
#' @keywords endpoint
#' @export
pub_paho_dengue <- function(
    regions,
    epiweeks = "*",
    ...,
    issues = NULL,
    lag = NULL,
    fetch_args = fetch_args_list()) {
  rlang::check_dots_empty()

  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("regions", regions)
  assert_timeset_param("epiweeks", epiweeks)
  assert_timeset_param("issues", issues, required = FALSE)
  assert_integerish_param("lag", lag, len = 1, required = FALSE)
  epiweeks <- parse_timeset_input(epiweeks)
  issues <- parse_timeset_input(issues)

  create_epidata_call(
    "paho_dengue/",
    list(
      regions = regions,
      epiweeks = epiweeks,
      issues = issues,
      lag = lag
    ),
    list(
      create_epidata_field_info("release_date", "date"),
      create_epidata_field_info("region", "text"),
      create_epidata_field_info("serotype", "text"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("issue", "epiweek"),
      create_epidata_field_info("lag", "int"),
      create_epidata_field_info("total_pop", "int"),
      create_epidata_field_info("num_dengue", "int"),
      create_epidata_field_info("num_severe", "int"),
      create_epidata_field_info("num_deaths", "int"),
      create_epidata_field_info("incidence_rate", "float")
    )
  ) %>% fetch(fetch_args = fetch_args)
}

#' Quidel COVID-19 and influenza testing data
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/quidel.html>
#'
#' Data provided by Quidel Corp., which contains flu lab test results.
#'
#' @examples
#' \dontrun{
#' pvt_quidel(
#'   auth = Sys.getenv("SECRET_API_AUTH_QUIDEL"),
#'   epiweeks = epirange(201201, 202001),
#'   locations = "hhs1"
#' )
#' }
#' @param auth string. Restricted access key (not the same as API key).
#' @param locations character. Locations to fetch.
#' @param epiweeks [`timeset`]. Epiweeks to fetch. Defaults to all ("*") dates.
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#' @return [`tibble::tibble`]
#' @keywords endpoint
#' @export
pvt_quidel <- function(
    auth,
    locations,
    epiweeks = "*",
    fetch_args = fetch_args_list()) {
  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("auth", auth, len = 1)
  assert_character_param("locations", locations)
  assert_timeset_param("epiweeks", epiweeks)
  epiweeks <- parse_timeset_input(epiweeks)

  create_epidata_call(
    "quidel/",
    list(
      auth = auth,
      locations = locations,
      epiweeks = epiweeks
    ),
    list(
      create_epidata_field_info("location", "text"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("value", "float")
    )
  ) %>% fetch(fetch_args = fetch_args)
}

#' Influenza and dengue digital surveillance sensors
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/sensors.html>
#'
#' This is the documentation of the API for accessing the Digital Surveillance
#'   Sensors endpoint of the Delphi’s epidemiological. Note: this
#'   repository was built to support modeling and forecasting efforts
#'   surrounding seasonal influenza (and dengue). In the current COVID-19
#'   pandemic, syndromic surveillance data, like ILI data (influenza-like
#'   illness) through FluView, will likely prove very useful. However, we urge
#'   caution to users examining the digital surveillance sensors, like ILI
#'   Nearby, Google Flu Trends, etc., during the COVID-19 pandemic, because
#'   these were designed to track ILI as driven by seasonal influenza, and were
#'   NOT designed to track ILI during the COVID-19 pandemic.
#'
#' @examples
#' \dontrun{
#' pvt_sensors(
#'   auth = Sys.getenv("SECRET_API_AUTH_SENSORS"),
#'   names = "sar3",
#'   locations = "nat",
#'   epiweeks = epirange(201501, 202001)
#' )
#' }
#' @param auth string. Restricted access key (not the same as API key).
#' @param names character. Sensor names to fetch.
#' @param locations character. Locations to fetch.
#' @param epiweeks [`timeset`]. Epiweeks to fetch. Defaults to all ("*") dates.
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#' @return [`tibble::tibble`]
#' @keywords endpoint
#' @export
pvt_sensors <- function(
    auth,
    names,
    locations,
    epiweeks = "*",
    fetch_args = fetch_args_list()) {
  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("auth", auth, len = 1)
  assert_character_param("names", names)
  assert_character_param("locations", locations)
  assert_timeset_param("epiweeks", epiweeks)
  epiweeks <- parse_timeset_input(epiweeks)

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
  ) %>% fetch(fetch_args = fetch_args)
}

#' HealthTweets total and influenza-related tweets
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/twitter.html>
#'
#' This is the API documentation for accessing the Twitter Stream endpoint of
#' Delphi’s epidemiological data. Sourced from
#' [Healthtweets](http://www.healthtweets.org/)
#'
#' @examples
#' \dontrun{
#' pvt_twitter(
#'   auth = Sys.getenv("SECRET_API_AUTH_TWITTER"),
#'   locations = "CA",
#'   time_type = "week",
#'   time_values = epirange(201501, 202001)
#' )
#' }
#' @param auth string. Restricted access key (not the same as API key).
#' @param locations character. Locations to fetch.
#' @param ... not used for values, forces later arguments to bind by name
#' @param time_type string. The temporal resolution of the data (either "day" or
#'  "week", depending on signal).
#' @param time_values [`timeset`]. Dates or epiweeks to fetch. Defaults to all
#'  ("*") dates.
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#' @return [`tibble::tibble`]
#' @keywords endpoint
#' @export
pvt_twitter <- function(
    auth,
    locations,
    ...,
    time_type = c("day", "week"),
    time_values = "*",
    fetch_args = fetch_args_list()) {
  rlang::check_dots_empty()

  time_type <- match.arg(time_type)
  if (time_type == "day") {
    dates <- time_values
    epiweeks <- NULL
    dates <- get_wildcard_equivalent_dates(dates, "day")
  } else {
    dates <- NULL
    epiweeks <- time_values
    epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")
  }

  assert_character_param("auth", auth, len = 1)
  assert_character_param("locations", locations)
  assert_character_param("time_type", time_type, len = 1)
  assert_timeset_param("time_values", time_values)
  assert_timeset_param("dates", dates, required = FALSE)
  assert_timeset_param("epiweeks", epiweeks, required = FALSE)
  dates <- parse_timeset_input(dates)
  epiweeks <- parse_timeset_input(epiweeks)

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
  ) %>% fetch(fetch_args = fetch_args)
}

#' Wikipedia webpage counts by article
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/wiki.html>
#
#' Number of page visits for selected English, Influenza-related wikipedia articles.
#'
#' * Source: Wikimedia
#' * Temporal Resolution: Hourly, daily, and weekly from 2007-12-09 (2007w50)
#' * Spatial Resolution: N/A
#' * Other resolution: By article (54)
#' * Open access
#'
#' @examples
#' \dontrun{
#' pub_wiki(
#'   articles = "avian_influenza",
#'   time_type = "week",
#'   time_values = epirange(201501, 201601)
#' )
#' }
#' @param articles character. Articles to fetch.
#' @param ... not used for values, forces later arguments to bind by name
#' @param time_type string. The temporal resolution of the data (either "day" or
#'  "week", depending on signal).
#' @param time_values [`timeset`]. Dates or epiweeks to fetch. Defaults to all
#'  ("*") dates.
#' @param language string. Language to fetch.
#' @param hours integer. Optionally, the hours to fetch.
#' @param fetch_args [`fetch_args`]. Additional arguments to pass to `fetch()`.
#' @return [`tibble::tibble`]
#' @keywords endpoint
#' @export
pub_wiki <- function(
    articles,
    ...,
    time_type = c("day", "week"),
    time_values = "*",
    hours = NULL,
    language = "en",
    fetch_args = fetch_args_list()) {
  rlang::check_dots_empty()

  time_type <- match.arg(time_type)
  if (time_type == "day") {
    dates <- time_values
    epiweeks <- NULL
    dates <- get_wildcard_equivalent_dates(dates, "day")
  } else {
    dates <- NULL
    epiweeks <- time_values
    epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")
  }

  assert_character_param("articles", articles)
  assert_character_param("time_type", time_type, len = 1)
  assert_timeset_param("time_values", time_values)
  assert_timeset_param("dates", dates, required = FALSE)
  assert_timeset_param("epiweeks", epiweeks, required = FALSE)
  assert_integerish_param("hours", hours, required = FALSE)
  assert_character_param("language", language, len = 1, required = FALSE)
  dates <- parse_timeset_input(dates)
  epiweeks <- parse_timeset_input(epiweeks)

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
  ) %>% fetch(fetch_args = fetch_args)
}
