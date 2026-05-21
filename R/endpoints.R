# The individual endpoint functions live in this file. Each function creates and
# `epidata_call` object and then calls `fetch()` on it. The endpoint functions
# are the main user-facing functions in this package.

#' @title Shared Documentation for epidatr Parameters
#'
#' @description This is a central text for parameter documentation
#' @name .epidatr_shared_params
#' @keywords internal
#'
#' @param auth string. Your restricted access key (not the same as API key).
#' @param locations character. List of locations to fetch.
#' @param states character. List of states to fetch, formatted as two letter state abbreviations.
#' @param regions character. List of regions to fetch.
#' @param epiweeks [`timeset`]. Epiweeks to fetch. Supports
#'  [`epirange()`] and defaults to all ("*") dates. Format as
#'  `epirange(startweek, endweek)`, where startweek and endweek are of the form
#'  YYYYWW (string or numeric).
#' @param time_type string. The temporal resolution of the data (either "day" or
#'  "week", depending on signal).
#' @param names character. Sensor names to fetch.
#' @param dates [`timeset`]. Dates to fetch. Supports
#'   [`epirange()`] and defaults to all ("*") dates.
#' @param time_values [`timeset`]. Dates or epiweeks to fetch.
#'   Supports [`epirange()`] and defaults to all ("*") dates.
#' @param as_of Date. Optionally, the as-of date for the issues to fetch.
#'   See the "Data Versioning" section for details.
#' @param issues [`timeset`]. Optionally, the issue(s) of the
#'   data to fetch. See the "Data Versioning" section for details.
#' @param lag integer. Optionally, the lag of the issues to fetch.
#'   See the "Data Versioning" section for details.
#' @param fetch_args [`fetch_args_list()`]. Additional arguments to pass
#'   to `fetch()`. See `fetch_args_list()` for details.
#' @param ... not used for values, forces later arguments to bind by name
#'
#' @section Data Versioning:
#' Several endpoints support retrieving historical versions of the data.
#' The following parameters control this and are mutually exclusive (only
#' one can be provided at a time).
#' \itemize{
#'   \item \code{as_of}: (Date) Retrieve the data as it was on this date.
#'   \item \code{issues}: [`timeset`] Retrieve data from a
#'     specific issue date or range of dates.
#'   \item \code{lag}: (integer) Retrieve data with a specific lag from
#'     its issue date.
#' }
#'
#' If none of these is specified, the most recent version of the data is
#' returned.
#'
#' See `vignette("versioned-data")` for details and more ways to specify
#' versioned data.
#'
#' @section See also:
#' For example queries showing how to discover signals and build calls,
#' see `vignette("signal-discovery", package = "epidatr")`.
NULL


#' CDC total and by topic webpage visits
#'
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/cdc.html>
#'
#'
#' @examples
#' \dontrun{
#' pvt_cdc(
#'   auth = Sys.getenv("DELPHI_EPIDATA_KEY"),
#'   locations = "fl,ca",
#'   epirange(201501, 201601)
#' )
#' }
#'
#' @inheritParams .epidatr_shared_params
#' @param locations character. List of locations to fetch.
#'   See [US Regions and States codes](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#us-regions-and-states) # nolint
#'   for details.
#' @return [`tibble::tibble`]
#'
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
pvt_cdc <- function(
  auth,
  locations,
  epiweeks = "*",
  fetch_args = fetch_args_list()
) {
  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("auth", auth, len = 1)
  assert_character_param("locations", locations)
  epiweeks <- validate_timeset_input("epiweeks", epiweeks)

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
#' @examplesIf curl::has_internet() && Sys.getenv("DELPHI_EPIDATA_KEY") != ""
#'
#' pub_covid_hosp_facility_lookup(state = "fl")
#' pub_covid_hosp_facility_lookup(city = "southlake")
#'
#' @inheritParams .epidatr_shared_params
#' @param state string. A two-letter character state abbreviation.
#'   See [US states codes](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#us-states)
#'   for details.
#' @param ccn string. A facility CMS certification number.
#' @param city string. A city name.
#' @param zip string. A 5-digit zip code.
#' @param fips_code string. A 5-digit fips county code, zero-padded.
#' @return [`tibble::tibble`]
#'
#' @seealso [`pub_covid_hosp_facility()`]
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
pub_covid_hosp_facility_lookup <- function(
  ...,
  state = NULL,
  ccn = NULL,
  city = NULL,
  zip = NULL,
  fips_code = NULL,
  fetch_args = fetch_args_list()
) {
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
#' @examplesIf curl::has_internet() && Sys.getenv("DELPHI_EPIDATA_KEY") != ""
#'
#' pub_covid_hosp_facility(
#'   hospital_pks = "100075",
#'   collection_weeks = epirange(20200101, 20200501)
#' )
#'
#' pub_covid_hosp_facility(
#'   hospital_pks = "050063",
#'   collection_weeks = epirange(20240101, 20240301)
#' )
#'
#' @inheritParams .epidatr_shared_params
#' @param hospital_pks character. Facility identifiers.
#' @param collection_weeks [`timeset`]. Dates (corresponding to epiweeks) to
#'  fetch. Defaults to all ("*") dates.
#' @param publication_dates [`timeset`]. Publication dates to fetch.
#' @return [`tibble::tibble`]
#'
#' @importFrom checkmate test_class test_integerish test_character
#'
#' @seealso [`pub_covid_hosp_facility()`], [`epirange()`]
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
#
pub_covid_hosp_facility <- function(
  hospital_pks,
  collection_weeks = "*",
  ...,
  publication_dates = NULL,
  fetch_args = fetch_args_list()
) {
  rlang::check_dots_empty()

  collection_weeks <- get_wildcard_equivalent_dates(collection_weeks, "day")

  assert_character_param("hospital_pks", hospital_pks)
  collection_weeks <- validate_timeset_input("collection_weeks", collection_weeks)
  publication_dates <- validate_timeset_input("publication_dates", publication_dates, required = FALSE)

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
#' @examplesIf curl::has_internet() && Sys.getenv("DELPHI_EPIDATA_KEY") != ""
#'
#' pub_covid_hosp_state_timeseries(
#'   states = "fl",
#'   dates = epirange(20200101, 20200501)
#' )
#'
#' @inheritParams .epidatr_shared_params
#' @param states character. Two-letter state abbreviations.
#'   See [US states codes](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#us-states)
#'   for details.
#'
#' @inheritSection .epidatr_shared_params Data Versioning
#'
#' @return [`tibble::tibble`]
#'
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
#
pub_covid_hosp_state_timeseries <- function(
  states,
  dates = "*",
  ...,
  as_of = NULL,
  issues = NULL,
  fetch_args = fetch_args_list()
) {
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
  dates <- validate_timeset_input("dates", dates)
  as_of <- validate_date_input("as_of", as_of, len = 1, required = FALSE)
  issues <- validate_timeset_input("issues", issues, required = FALSE)

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
#' The result can be filtered server-side by passing `signals`, `time_type`,
#' and/or `geo_type`. Omitted filters (the default) return metadata for
#' everything.
#'
#' @inheritParams .epidatr_shared_params
#' @param signals character. Optionally, the signals to return metadata for,
#'   each formatted as `"source:signal"` (e.g. `"fb-survey:smoothed_cli"`).
#'   Defaults to all signals.
#' @param geo_type string. Optionally, a single geographic resolution to return
#'   metadata for (see:
#'   <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html>).
#'   Defaults to all geographic resolutions.
#'
#' @return [`tibble::tibble`]
#'
#' @examplesIf curl::has_internet() && Sys.getenv("DELPHI_EPIDATA_KEY") != ""
#'
#' pub_covidcast_meta()
#' # All signals from the Facebook survey data source
#' pub_covidcast_meta(
#'   signals = "fb-survey:*"
#' )
#' # All signals with time_type "day".
#' pub_covidcast_meta(
#'   time_type = "day",
#' )
#' # All signals with geo_type "state".
#' pub_covidcast_meta(
#'   geo_type = "state",
#' )
#'
#' @seealso [pub_covidcast()],[covidcast_epidata()]
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
pub_covidcast_meta <- function(
  signals = NULL,
  time_type = NULL,
  geo_type = NULL,
  fetch_args = fetch_args_list()
) {
  assert_character_param("signals", signals, required = FALSE)
  assert_character_param("time_type", time_type, len = 1, required = FALSE)
  assert_character_param("geo_type", geo_type, len = 1, required = FALSE)

  create_epidata_call(
    "covidcast_meta/",
    list(
      signals = signals,
      time_types = time_type,
      geo_types = geo_type
    ),
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
        categories = c("nation", "msa", "hrr", "hhs", "state", "county", "dma", "hsa_nci")
      ),
      create_epidata_field_info("min_time", "int"),
      create_epidata_field_info("max_time", "int"),
      create_epidata_field_info("num_locations", "int"),
      create_epidata_field_info("min_value", "float"),
      create_epidata_field_info("max_value", "float"),
      create_epidata_field_info("mean_value", "float"),
      create_epidata_field_info("stdev_value", "float"),
      create_epidata_field_info("last_update", "timestamp"),
      create_epidata_field_info("max_issue", "int"),
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
#' @examplesIf curl::has_internet() && Sys.getenv("DELPHI_EPIDATA_KEY") != ""
#'
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
#'
#' @inheritParams .epidatr_shared_params
#' @param source string. The data source to query (see:
#'   <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html>).
#' @param signals string. The signals to query from a specific source (see:
#'   <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html>).
#' @param geo_type string. The geographic resolution of the data (see:
#'   <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html>).
#' @param geo_values character. The geographies to return. Defaults to all
#'  ("*") geographies within requested geographic resolution (see:
#'  <https://cmu-delphi.github.io/delphi-epidata/api/covidcast_geography.html>.).
#' @return [`tibble::tibble`]
#'
#' @inheritSection .epidatr_shared_params Data Versioning
#'
#' @seealso [pub_covidcast_meta()], [covidcast_epidata()], [epirange()]
#' @inheritSection .epidatr_shared_params See also
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
  fetch_args = fetch_args_list()
) {
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
  time_values <- validate_timeset_input("time_values", time_values)
  assert_character_param("geo_values", geo_values)
  as_of <- validate_date_input("as_of", as_of, len = 1, required = FALSE)
  issues <- validate_timeset_input("issues", issues, required = FALSE)
  assert_integerish_param("lag", lag, len = 1, required = FALSE)

  if (source == "nchs-mortality" && time_type != "week") {
    cli::cli_abort(
      "{source} data is only available at the week level",
      class = "epidatr__nchs_week_only"
    )
  }

  if (source == "nssp" && time_type != "week") {
    cli::cli_abort(
      "{source} data is only available at the week level",
      class = "epidatr__nchs_week_only"
    )
  }

  # TODO: This should probably be done in the create_epidata_call function. But
  # this is a quick fix for now.
  checkmate::assert_subset(time_type, c("day", "week"))

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
        categories = c("nation", "msa", "hrr", "hhs", "state", "county", "dma", "hsa_nci")
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

#' Get cast-API source metadata
#'
#' @description
#' `epidata_meta` returns source-level metadata from the cast-API,
#' including version ranges, time value ranges, and lists of available signals
#' and geo types.
#'
#' @param source string. The data source to query.
#' @inheritParams .epidatr_shared_params
#' @return list
#' @seealso [epidata_snapshot()], [epidata_archive()], [epidata()], [epirange()]
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
epidata_meta <- function(source, fetch_args = fetch_args_list()) {
  assert_character_param("source", source, len = 1, required = TRUE)
  create_epidata_call(
    endpoint = "metadata/",
    params = list(source = source),
    api_version = "cast",
    response_format = "json"
  ) %>% request_epidata(fetch_args = fetch_args)
}

#' cast-API snapshot and archive queries
#'
#' @description
#' - `epidata_snapshot` fetches a snapshot of signals as they appeared at a
#'   specific date (or the latest available if `as_of` is omitted).
#' - `epidata_archive` fetches the full version history of signals across all
#'   available issues.
#' - `epidata` is a wrapper that routes to one of the above based
#'   on which versioning argument is supplied.
#'
#' @inheritParams pub_covidcast
#' @param source string. The data source to query (e.g., `"nssp"`, `"nhsn"`).
#'   Use [epidata_meta()] to discover available sources.
#' @param signals character vector. One or more signals to query for the given
#'   source. Use [epidata_meta()] to discover available signals.
#' @param geo_type string. The geography type to query (e.g., `"state"`,
#'   `"nation"`, `"county"`). Use [epidata_meta()] to discover available
#'   geo types for a given source and signal.
#' @param time_values [`timeset`]. Time values to return. Supports individual
#'   dates or [`epirange()`]. Defaults to all (`"*"`). Filtered locally after
#'   the API call.
#' @param fill_method string. Optional filter to an imputation method.
#'   The API provides alternatives of the same signal differing in how
#'   nulls were handled during geographic aggregation: `"source"` means no
#'   imputation or aggregation (raw source data), `"fill_ave"` fills nulls with
#'   the average of neighboring values, and `"fill_zero"` fills nulls with zero.
#'   `NULL` (default) returns all fill methods.
#' @param as_of Date or `NULL`. The snapshot date; `NULL` returns the latest
#'   available version. Internally maps to the `snapshot_date` parameter.
#' @param version Date, string, or [`epirange()`]. A version query for the
#'   archive endpoint. Supports exact dates (e.g., `"2025-10-16"`),
#'   operators (e.g., `"<2025-10-16"`), or an [`epirange()`].
#'   Internally maps to the `version_query` parameter.
#' @return [`tibble::tibble`]
#' @seealso [epidata_meta()], [epirange()]
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @name cast_api_queries
NULL

#' @rdname cast_api_queries
#' @export
epidata_snapshot <- function(
  source,
  signals,
  geo_type,
  geo_values = "*",
  time_values = "*",
  ...,
  fill_method = NULL,
  as_of = NULL,
  fetch_args = fetch_args_list()
) {
  if (missing(source) || missing(signals) || missing(geo_type)) {
    cli::cli_abort(
      "`source`, `signals`, and `geo_type` are all required",
      class = "epidatr__epidata__missing_required_args"
    )
  }

  rlang::check_dots_empty()

  assert_character_param("source", source, len = 1)
  assert_character_param("signals", signals)
  assert_character_param("geo_type", geo_type, len = 1)
  assert_character_param("geo_values", geo_values)
  assert_character_param("fill_method", fill_method, len = 1, required = FALSE)
  assert_date_param("as_of", as_of, len = 1, required = FALSE)
  # as_of reformatting
  if (!is.null(as_of)) as_of <- format(parse_api_date(as_of), "%Y-%m-%d")

  parsed_time_values <- validate_timeset_input("time_values", time_values)

  create_epidata_call(
    endpoint = "snapshot/",
    params = list(
      source = source,
      signal = paste(signals, collapse = ","),
      geo_type = geo_type,
      fill_method = fill_method,
      snapshot_date = as_of
    ),
    meta = list(
      create_epidata_field_info("signal", "text"),
      create_epidata_field_info("version", "date"),
      create_epidata_field_info("geo_type", "text"),
      create_epidata_field_info("geo_value", "text"),
      create_epidata_field_info("fill_method", "text"),
      create_epidata_field_info("time_value", "date"),
      create_epidata_field_info("value", "float"),
      # source-specific extra columns
      create_epidata_field_info("age_group", "text"),     # pophive
      create_epidata_field_info("nwss_source", "text"),   # nwss
      create_epidata_field_info("sample_index", "text"),  # nwss
      create_epidata_field_info("pcr_target", "text")     # nwss
    ),
    api_version = "cast",
    response_format = "csv"
  ) %>%
    fetch(fetch_args = fetch_args) %>%
    .cast_filter(geo_values, time_values, parsed_time_values)
}

#' @rdname cast_api_queries
#' @export
epidata_archive <- function(
  source,
  signals,
  geo_type,
  geo_values = "*",
  time_values = "*",
  ...,
  fill_method = NULL,
  version = "*",
  fetch_args = fetch_args_list()
) {
  if (missing(source) || missing(signals) || missing(geo_type)) {
    cli::cli_abort(
      "`source`, `signals`, and `geo_type` are all required",
      class = "epidatr__epidata__missing_required_args"
    )
  }

  rlang::check_dots_empty()

  assert_character_param("source", source, len = 1)
  assert_character_param("signals", signals)
  assert_character_param("geo_type", geo_type, len = 1)
  assert_character_param("geo_values", geo_values)
  assert_character_param("fill_method", fill_method, len = 1, required = FALSE)

  parsed_time_values <- validate_timeset_input("time_values", time_values)
  version_query <- validate_version_query(version)

  create_epidata_call(
    endpoint = "archive/",
    params = list(
      source = source,
      signal = paste(signals, collapse = ","),
      geo_type = geo_type,
      fill_method = fill_method,
      version_query = version_query
    ),
    meta = list(
      create_epidata_field_info("signal", "text"),
      create_epidata_field_info("version", "date"),
      create_epidata_field_info("geo_type", "text"),
      create_epidata_field_info("geo_value", "text"),
      create_epidata_field_info("fill_method", "text"),
      create_epidata_field_info("time_value", "date"),
      create_epidata_field_info("value", "float"),
      # source-specific extra columns
      create_epidata_field_info("age_group", "text"),     # pophive
      create_epidata_field_info("nwss_source", "text"),   # nwss
      create_epidata_field_info("sample_index", "text"),  # nwss
      create_epidata_field_info("pcr_target", "text")     # nwss
    ),
    api_version = "cast",
    response_format = "csv"
  ) %>%
    fetch(fetch_args = fetch_args) %>%
    .cast_filter(geo_values, time_values, parsed_time_values, version = version)
}

#' @rdname cast_api_queries
#' @export
epidata <- function(
  source,
  signals,
  geo_type,
  geo_values = "*",
  time_values = "*",
  ...,
  fill_method = NULL,
  as_of = NULL,
  version = NULL,
  fetch_args = fetch_args_list()
) {
  if (!is.null(version) && !is.null(as_of)) {
    cli::cli_abort(
      "`version` and `as_of` are mutually exclusive",
      class = "epidatr__epidata__version_and_as_of_exclusive"
    )
  }

  if (!is.null(version) || identical(as_of, "*")) {
    epidata_archive(
      source = source, signals = signals, geo_type = geo_type,
      geo_values = geo_values, time_values = time_values,
      fill_method = fill_method,
      version = if (!is.null(version)) version else "*",
      fetch_args = fetch_args
    )
  } else {
    epidata_snapshot(
      source = source, signals = signals, geo_type = geo_type,
      geo_values = geo_values, time_values = time_values,
      fill_method = fill_method,
      as_of = as_of,
      fetch_args = fetch_args
    )
  }
}

#' Delphi's ILINet outpatient doctor visits forecasts
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/delphi.html>
#'
#' @examplesIf curl::has_internet() && Sys.getenv("DELPHI_EPIDATA_KEY") != ""
#'
#' pub_delphi(system = "ec", epiweek = 201501)
#'
#' @inheritParams .epidatr_shared_params
#' @param system character. System name to fetch.
#'   See the [available forecasting systems](https://cmu-delphi.github.io/delphi-epidata/api/delphi.html#forecasting-systems) # nolint
#'   for details.
#' @param epiweek [`timeset`]. Epiweek to fetch. Does not support multiple dates.
#'  Make separate calls to fetch data for multiple epiweeks.
#' @return [`list`]
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
pub_delphi <- function(
  system,
  epiweek,
  fetch_args = fetch_args_list()
) {
  assert_character_param("system", system)
  epiweek <- validate_timeset_input("epiweek", epiweek, len = 1)

  create_epidata_call(
    "delphi/",
    list(system = system, epiweek = epiweek),
    list(
      create_epidata_field_info("system", "text"),
      create_epidata_field_info("epiweek", "epiweek"),
      create_epidata_field_info("json", "text")
    )
  ) %>% request_epidata(fetch_args = fetch_args, simplify = FALSE)
}

#' Delphi's PAHO dengue nowcasts (North and South America)
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/dengue_nowcast.html>
#'
#' @examplesIf curl::has_internet() && Sys.getenv("DELPHI_EPIDATA_KEY") != ""
#'
#' pub_dengue_nowcast(
#'   locations = "pr",
#'   epiweeks = epirange(201401, 202301)
#' )
#'
#' @inheritParams .epidatr_shared_params
#' @param locations character. List of locations to fetch.
#'   See the [codes for countries and territories in the Americas](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#countries-and-territories-in-the-americas). # nolint
#' @return [`tibble::tibble`]
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
pub_dengue_nowcast <- function(
  locations,
  epiweeks = "*",
  fetch_args = fetch_args_list()
) {
  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("locations", locations)
  epiweeks <- validate_timeset_input("epiweeks", epiweeks)

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
#'   auth = Sys.getenv("DELPHI_EPIDATA_KEY"),
#'   names = "ght",
#'   locations = "ag",
#'   epiweeks = epirange(201501, 202001)
#' )
#' }
#'
#' @inheritParams .epidatr_shared_params
#' @param locations character. List of locations to fetch.
#'   See the [codes for countries and territories in the Americas](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#countries-and-territories-in-the-americas). # nolint
#' @param names character. List of sensor names to fetch.
#'   See the [available sensors](https://cmu-delphi.github.io/delphi-epidata/api/dengue_sensors.html#available-sensors).
#' @return [`tibble::tibble`]
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
pvt_dengue_sensors <- function(
  auth,
  names,
  locations,
  epiweeks = "*",
  fetch_args = fetch_args_list()
) {
  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("auth", auth, len = 1)
  assert_character_param("names", names)
  assert_character_param("locations", locations)
  epiweeks <- validate_timeset_input("epiweeks", epiweeks)

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
#' @examplesIf curl::has_internet() && Sys.getenv("DELPHI_EPIDATA_KEY") != ""
#'
#' pub_ecdc_ili(regions = "austria", epiweeks = epirange(201901, 202001))
#'
#' @inheritParams .epidatr_shared_params
#' @param regions character. List of regions to fetch.
#'   See the [codes for European countries](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#european-countries). # nolint
#' @return [`tibble::tibble`]
#'
#' @inheritSection .epidatr_shared_params Data Versioning
#'
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
pub_ecdc_ili <- function(
  regions,
  epiweeks = "*",
  ...,
  issues = NULL,
  lag = NULL,
  fetch_args = fetch_args_list()
) {
  rlang::check_dots_empty()

  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("regions", regions)
  epiweeks <- validate_timeset_input("epiweeks", epiweeks)
  issues <- validate_timeset_input("issues", issues, required = FALSE)
  assert_integerish_param("lag", lag, len = 1, required = FALSE)

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
#' @examplesIf curl::has_internet() && Sys.getenv("DELPHI_EPIDATA_KEY") != ""
#'
#' pub_flusurv(locations = "ca", epiweeks = epirange(201701, 201801))
#'
#' @inheritParams .epidatr_shared_params
#' @param locations character. List of locations to fetch.
#'   See [geographic codes](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#flusurv-locations)
#'   for details.
#' @return [`tibble::tibble`]
#'
#' @inheritSection .epidatr_shared_params Data Versioning
#'
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
pub_flusurv <- function(
  locations,
  epiweeks = "*",
  ...,
  issues = NULL,
  lag = NULL,
  fetch_args = fetch_args_list()
) {
  rlang::check_dots_empty()

  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("locations", locations)
  epiweeks <- validate_timeset_input("epiweeks", epiweeks)
  issues <- validate_timeset_input("issues", issues, required = FALSE)
  assert_integerish_param("lag", lag, len = 1, required = FALSE)

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
      create_epidata_field_info("rate_overall", "float"),
      create_epidata_field_info("rate_age_5", "float"),
      create_epidata_field_info("rate_age_6", "float"),
      create_epidata_field_info("rate_age_7", "float"),
      create_epidata_field_info("rate_age_18t29", "float"),
      create_epidata_field_info("rate_age_30t39", "float"),
      create_epidata_field_info("rate_age_40t49", "float"),
      create_epidata_field_info("rate_age_5t11", "float"),
      create_epidata_field_info("rate_age_12t17", "float"),
      create_epidata_field_info("rate_age_lt18", "float"),
      create_epidata_field_info("rate_age_gte18", "float"),
      create_epidata_field_info("rate_age_0tlt1", "float"),
      create_epidata_field_info("rate_age_1t4", "float"),
      create_epidata_field_info("rate_age_gte75", "float"),
      create_epidata_field_info("rate_race_white", "float"),
      create_epidata_field_info("rate_race_black", "float"),
      create_epidata_field_info("rate_race_hisp", "float"),
      create_epidata_field_info("rate_race_asian", "float"),
      create_epidata_field_info("rate_race_natamer", "float"),
      create_epidata_field_info("rate_sex_male", "float"),
      create_epidata_field_info("rate_sex_female", "float"),
      create_epidata_field_info("rate_flu_a", "float"),
      create_epidata_field_info("rate_flu_b", "float"),
      create_epidata_field_info("season", "text")
    )
  ) %>% fetch(fetch_args = fetch_args)
}

#' CDC FluView flu tests from clinical labs
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/fluview_clinical.html>
#'
#' @examplesIf curl::has_internet() && Sys.getenv("DELPHI_EPIDATA_KEY") != ""
#'
#' pub_fluview_clinical(regions = "nat", epiweeks = epirange(201601, 201701))
#'
#' @inheritParams .epidatr_shared_params
#' @param regions character. Vector of location IDs to fetch.  Can be
#'   "nat" for national, "hhs1"--"hhs10" for HHS Regions, "cen1"--"cen9" for
#'   census divisions, lowercase two-letter state or territory abbreviations
#'   for most states and territories,"jfk" for New York City, or "ny_minus_jfk"
#'   for upstate New York. Full list of locations is available
#'   [here](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#us-regions-and-states)
#'   and [here](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#fluview-cities).
#' @return [`tibble::tibble`]
#'
#' @inheritSection .epidatr_shared_params Data Versioning
#'
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
pub_fluview_clinical <- function(
  regions,
  epiweeks = "*",
  ...,
  issues = NULL,
  lag = NULL,
  fetch_args = fetch_args_list()
) {
  rlang::check_dots_empty()

  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("regions", regions)
  epiweeks <- validate_timeset_input("epiweeks", epiweeks)
  issues <- validate_timeset_input("issues", issues, required = FALSE)
  assert_integerish_param("lag", lag, len = 1, required = FALSE)

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
#' @examplesIf curl::has_internet() && Sys.getenv("DELPHI_EPIDATA_KEY") != ""
#'
#' pub_fluview_meta()
#'
#' @inheritParams .epidatr_shared_params
#'
#' @return [`tibble::tibble`]
#' @seealso [`pub_fluview()`]
#' @inheritSection .epidatr_shared_params See also
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
#' @examplesIf curl::has_internet() && Sys.getenv("DELPHI_EPIDATA_KEY") != ""
#'
#' pub_fluview(regions = "nat", epiweeks = epirange(201201, 202005))
#'
#' @inheritParams pub_fluview_clinical
#' @inheritParams .epidatr_shared_params
#' @return [`tibble::tibble`]
#'
#' @inheritSection .epidatr_shared_params Data Versioning
#'
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
pub_fluview <- function(
  regions,
  epiweeks = "*",
  ...,
  issues = NULL,
  lag = NULL,
  auth = NULL,
  fetch_args = fetch_args_list()
) {
  rlang::check_dots_empty()

  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("regions", regions)
  epiweeks <- validate_timeset_input("epiweeks", epiweeks)
  issues <- validate_timeset_input("issues", issues, required = FALSE)
  assert_integerish_param("lag", lag, len = 1, required = FALSE)
  assert_character_param("auth", auth, len = 1, required = FALSE)

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
#'   <https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#hhs-regions>,
#'   <https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#us-states>,
#'   and
#'   <https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#selected-us-cities>.
#'
#' @examplesIf curl::has_internet() && Sys.getenv("DELPHI_EPIDATA_KEY") != ""
#'
#' pub_gft(locations = "hhs1", epiweeks = epirange(201201, 202001))
#'
#' @inheritParams .epidatr_shared_params
#'
#' @return [`tibble::tibble`]
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
pub_gft <- function(
  locations,
  epiweeks = "*",
  fetch_args = fetch_args_list()
) {
  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("locations", locations)
  epiweeks <- validate_timeset_input("epiweeks", epiweeks)

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
#'   auth = Sys.getenv("DELPHI_EPIDATA_KEY"),
#'   locations = "ma",
#'   epiweeks = epirange(199301, 202304),
#'   query = "how to get over the flu"
#' )
#' }
#'
#' @inheritParams .epidatr_shared_params
#' @param locations character. List of locations to fetch.
#'   See [geographic codes](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#us-states-and-territories) # nolint
#'   for details.
#' @param query string. The query to be fetched.
#' @return [`tibble::tibble`]
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
pvt_ght <- function(
  auth,
  locations,
  epiweeks = "*",
  query,
  fetch_args = fetch_args_list()
) {
  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("auth", auth, len = 1)
  assert_character_param("locations", locations)
  epiweeks <- validate_timeset_input("epiweeks", epiweeks)
  assert_character_param("query", query, len = 1)

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
#' @examplesIf curl::has_internet() && Sys.getenv("DELPHI_EPIDATA_KEY") != ""
#'
#' pub_kcdc_ili(regions = "ROK", epiweeks = 200436)
#'
#' @inheritParams .epidatr_shared_params
#' @param regions character. List of regions to fetch.
#'   See [South Korea's geographic codes](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#republic-of-korea) # nolint
#'   for details.
#' @return [`tibble::tibble`]
#'
#' @inheritSection .epidatr_shared_params Data Versioning
#'
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
pub_kcdc_ili <- function(
  regions,
  epiweeks = "*",
  ...,
  issues = NULL,
  lag = NULL,
  fetch_args = fetch_args_list()
) {
  rlang::check_dots_empty()

  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("regions", regions)
  epiweeks <- validate_timeset_input("epiweeks", epiweeks)
  issues <- validate_timeset_input("issues", issues, required = FALSE)
  assert_integerish_param("lag", lag, len = 1, required = FALSE)

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
#' pvt_meta_norostat(auth = Sys.getenv("DELPHI_EPIDATA_KEY"))
#' }
#' @inheritParams .epidatr_shared_params
#' @return [`list`]
#' @seealso [`pvt_norostat()`]
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
pvt_meta_norostat <- function(auth, fetch_args = fetch_args_list()) {
  assert_character_param("auth", auth, len = 1)

  create_epidata_call(
    "meta_norostat/",
    list(auth = auth)
  ) %>% request_epidata(fetch_args = fetch_args, simplify = FALSE)
}

#' Metadata for the Delphi Epidata API
#' @description
#' API docs: <https://cmu-delphi.github.io/delphi-epidata/api/meta.html>
#'
#' @inheritParams .epidatr_shared_params
#'
#' @return [`list`]
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
pub_meta <- function(fetch_args = fetch_args_list()) {
  create_epidata_call("meta/", list()) %>% request_epidata(fetch_args = fetch_args, simplify = FALSE)
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
#' @examplesIf curl::has_internet() && Sys.getenv("DELPHI_EPIDATA_KEY") != ""
#'
#' pub_nidss_dengue(locations = "taipei", epiweeks = epirange(201201, 201301))
#'
#' @inheritParams .epidatr_shared_params
#' @param locations character. List of locations to fetch.
#'   See [Taiwan's geographic codes](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#nidss)
#'   for details.
#'
#' @return [`tibble::tibble`]
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
pub_nidss_dengue <- function(
  locations,
  epiweeks = "*",
  fetch_args = fetch_args_list()
) {
  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("locations", locations)
  epiweeks <- validate_timeset_input("epiweeks", epiweeks)

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
#' @examplesIf curl::has_internet() && Sys.getenv("DELPHI_EPIDATA_KEY") != ""
#'
#' pub_nidss_flu(regions = "taipei", epiweeks = epirange(201501, 201601))
#'
#' @inheritParams .epidatr_shared_params
#' @param regions character. List of regions to fetch.
#'   See [Taiwan's geographic codes](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#nidss)
#'   for details.
#' @return [`tibble::tibble`]
#'
#' @inheritSection .epidatr_shared_params Data Versioning
#'
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
pub_nidss_flu <- function(
  regions,
  epiweeks = "*",
  ...,
  issues = NULL,
  lag = NULL,
  fetch_args = fetch_args_list()
) {
  rlang::check_dots_empty()

  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("regions", regions)
  epiweeks <- validate_timeset_input("epiweeks", epiweeks)
  issues <- validate_timeset_input("issues", issues, required = FALSE)
  assert_integerish_param("lag", lag, len = 1, required = FALSE)

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
#'   auth = Sys.getenv("DELPHI_EPIDATA_KEY"),
#'   locations = "Minnesota, Ohio, Oregon, Tennessee, and Wisconsin",
#'   epiweeks = 201233
#' )
#' }
#'
#' @inheritParams .epidatr_shared_params
#' @param locations character. Locations to fetch. Only a specific list of
#' full state names are permitted. See the `locations` column in the
#' output of `pvt_meta_norostat()` for the allowed values.
#' @return [`tibble::tibble`]
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
pvt_norostat <- function(
  auth,
  locations,
  epiweeks = "*",
  fetch_args = fetch_args_list()
) {
  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("auth", auth, len = 1)
  assert_character_param("locations", locations, len = 1)
  epiweeks <- validate_timeset_input("epiweeks", epiweeks)

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
#' <https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#us-regions-and-states>
#' and <https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#fluview-cities>.
#' @examplesIf curl::has_internet() && Sys.getenv("DELPHI_EPIDATA_KEY") != ""
#'
#' pub_nowcast(locations = "ca", epiweeks = epirange(201201, 201301))
#'
#' @inheritParams .epidatr_shared_params
#' @return [`tibble::tibble`]
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
pub_nowcast <- function(
  locations,
  epiweeks = "*",
  fetch_args = fetch_args_list()
) {
  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("locations", locations)
  epiweeks <- validate_timeset_input("epiweeks", epiweeks)

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
#' @examplesIf curl::has_internet() && Sys.getenv("DELPHI_EPIDATA_KEY") != ""
#'
#' pub_paho_dengue(regions = "ca", epiweeks = epirange(201401, 201501))
#'
#' @inheritParams .epidatr_shared_params
#' @param regions character. List of regions to fetch.
#'   See [Americas' geographic codes](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#countries-and-territories-in-the-americas) # nolint
#'   for details.
#' @return [`tibble::tibble`]
#'
#' @inheritSection .epidatr_shared_params Data Versioning
#'
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
pub_paho_dengue <- function(
  regions,
  epiweeks = "*",
  ...,
  issues = NULL,
  lag = NULL,
  fetch_args = fetch_args_list()
) {
  rlang::check_dots_empty()

  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("regions", regions)
  epiweeks <- validate_timeset_input("epiweeks", epiweeks)
  issues <- validate_timeset_input("issues", issues, required = FALSE)
  assert_integerish_param("lag", lag, len = 1, required = FALSE)

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
#'   auth = Sys.getenv("DELPHI_EPIDATA_KEY"),
#'   epiweeks = epirange(201201, 202001),
#'   locations = "hhs1"
#' )
#' }
#'
#' @inheritParams .epidatr_shared_params
#' @param locations character. List of locations to fetch.
#'   See [HHS regions' codes](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#hhs-regions)
#'   for details.
#' @return [`tibble::tibble`]
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
pvt_quidel <- function(
  auth,
  locations,
  epiweeks = "*",
  fetch_args = fetch_args_list()
) {
  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("auth", auth, len = 1)
  assert_character_param("locations", locations)
  epiweeks <- validate_timeset_input("epiweeks", epiweeks)

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
#'   auth = Sys.getenv("DELPHI_EPIDATA_KEY"),
#'   names = "sar3",
#'   locations = "nat",
#'   epiweeks = epirange(201501, 202001)
#' )
#' }
#'
#' @inheritParams .epidatr_shared_params
#' @param names character. List of sensor names to fetch.
#'   See the [data sources available](https://cmu-delphi.github.io/delphi-epidata/api/sensors.html#data-sources)
#'   for details.
#' @param locations character. List of locations to fetch.
#'   See the codes of the [US regions and states](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#us-regions-and-states) # nolint
#'   for details.
#' @return [`tibble::tibble`]
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
pvt_sensors <- function(
  auth,
  names,
  locations,
  epiweeks = "*",
  fetch_args = fetch_args_list()
) {
  epiweeks <- get_wildcard_equivalent_dates(epiweeks, "week")

  assert_character_param("auth", auth, len = 1)
  assert_character_param("names", names)
  assert_character_param("locations", locations)
  epiweeks <- validate_timeset_input("epiweeks", epiweeks)

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
#'   auth = Sys.getenv("DELPHI_EPIDATA_KEY"),
#'   locations = "CA",
#'   time_type = "week",
#'   time_values = epirange(201501, 202001)
#' )
#' }
#'
#' @inheritParams .epidatr_shared_params
#' @param locations character. List of locations to fetch.
#'   See the codes of the [US regions and states](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#us-regions-and-states) # nolint
#'   for details.
#' @return [`tibble::tibble`]
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
pvt_twitter <- function(
  auth,
  locations,
  ...,
  time_type = c("day", "week"),
  time_values = "*",
  fetch_args = fetch_args_list()
) {
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
  time_values <- validate_timeset_input("time_values", time_values)
  dates <- validate_timeset_input("dates", dates, required = FALSE)
  epiweeks <- validate_timeset_input("epiweeks", epiweeks, required = FALSE)

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
#' @examplesIf curl::has_internet() && Sys.getenv("DELPHI_EPIDATA_KEY") != ""
#'
#' pub_wiki(
#'   articles = "avian_influenza",
#'   time_type = "week",
#'   time_values = epirange(201501, 201601)
#' )
#'
#' @inheritParams .epidatr_shared_params
#' @param articles character. Articles to fetch.
#'   See [available articles](https://cmu-delphi.github.io/delphi-epidata/api/wiki.html#available-articles)
#'   for details.
#' @param language string. Language to fetch.
#' @param hours integer. Optionally, the hours to fetch.
#' @return [`tibble::tibble`]
#' @inheritSection .epidatr_shared_params See also
#' @keywords endpoint
#' @export
pub_wiki <- function(
  articles,
  ...,
  time_type = c("day", "week"),
  time_values = "*",
  hours = NULL,
  language = "en",
  fetch_args = fetch_args_list()
) {
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
  time_values <- validate_timeset_input("time_values", time_values)
  dates <- validate_timeset_input("dates", dates, required = FALSE)
  epiweeks <- validate_timeset_input("epiweeks", epiweeks, required = FALSE)
  assert_integerish_param("hours", hours, required = FALSE)
  assert_character_param("language", language, len = 1, required = FALSE)

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
