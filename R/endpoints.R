

#' fetch AFHSB data (point data, no min/max)
#'
#' @param auth authenfication token
#' @param locations locations to fetch
#' @param epiweeks epiweeks to fetch
#' @param flu_types flu_types to fetch
#' @return an instance of EpiDataCall
#'
#' @export
afhsb <- function(auth, locations, epiweeks, flu_types) {
  check_single_string_param('auth', auth)
  check_string_param('locations', locations)
  check_epirange_param('epiweeks', epiweeks)
  check_string_param('flu_types', flu_types)

  create_epidata_call("afhsb/", list(auth=auth, locations=locations, epiweeks=epiweeks, flu_types=flu_types))
}

#'
#' fetch CDC page hits
#'
#' @param auth authenfication token
#' @param epiweeks epiweeks to fetch
#' @param locations locations to fetch
#' @return an instance of EpiDataCall
#'
#' @export
cdc <- function(auth, epiweeks, locations) {
  check_single_string_param('auth', auth)
  check_epirange_param('epiweeks', epiweeks)
  check_string_param('locations', locations)

  create_epidata_call("cdc/", list(auth=auth, epiweeks=epiweeks, locations=locations))
}

#'
#' fetch COVID hospitalization facility identifiers
#'
#' @param state optional state
#' @param ccn optional ccn
#' @param city optional city
#' @param zip optional zip code
#' @param fips_code optional fips code
#' @return an instance of EpiDataCall
#'
#' @export
covid_hosp_facility_lookup <- function(state = NULL, ccn = NULL, city = NULL, zip = NULL, fips_code = NULL) {
  check_single_string_param('state', state, FALSE)
  check_single_string_param('ccn', ccn, FALSE)
  check_single_string_param('city', city, FALSE)
  check_single_int_param('zip', zip, FALSE)
  check_single_int_param('fips_code', fips_code, FALSE)
  if(missing(state) && missing(ccn) && missing(city) && missing(zip) && missing(fips_code)) {
    stop('one of `state`, `ccn`, `city`, `zip`, or `fips_code` is required')
  }
  create_epidata_call("covid_hosp_facility_lookup/", list(state=state,ccn=ccn,city=city,zip=zip,fips_code=fips_code))
}

#'
#' fetch COVID hospitalization data for specific facilities
#'
#' @param hospital_pks hospitals to fetch
#' @param collection_weeks weeks to fetch
#' @param publication_dates publication dates to fetch
#' @return an instance of EpiDataCall
#'
#' @export
#
covid_hosp_facility <- function(hospital_pks, collection_weeks, publication_dates = NULL) {
  check_string_param('hospital_pks', hospital_pks)
  check_epirange_param('collection_weeks', collection_weeks)
  check_epirange_param('publication_dates', publication_dates)

  create_epidata_call("covid_hosp_facility/", list(hospital_pks=hospital_pks, collection_weeks=collection_weeks, publication_dates=publication_dates))
}

#'
#' fetch COVID hospitalization data
#'
#' @param states states to fetch
#' @param dates dates to fetch
#' @param issues issues to fetch
#' @return an instance of EpiDataCall
#'
#' @export
#
covid_hosp_state_timeseries <- function(states, dates, issues = NULL) {
  check_string_param('states', states)
  check_epirange_param('dates', dates)
  check_epirange_param('issues', issues, FALSE)

  create_epidata_call("covid_hosp_state_timeseries/", list(states=states, dates=dates, issues=issues))
}

#'
#' fetch covidcast meta data
#'
#' @return an instance of EpiDataCall
#'
#' @export
covidcast_meta <- function() {
  create_epidata_call("covidcast_meta/", list())
}


# TODO covidcast_nowcast

#'
#' fetch covidcast data
#'
#' @param data_source data source to fetch
#' @param signals data source to fetch
#' @param time_type data source to fetch
#' @param time_values data source to fetch
#' @param geo_type geo_type to fetch
#' @param geo_values data source to fetch
#' @param as_of data source to fetch
#' @param issues data source to fetch
#' @param lag data source to fetch
#' @return an instance of EpiDataCall
#'
#' @export
covidcast <- function(data_source, signals, time_type, geo_type, time_values, geo_values, as_of = NULL, issues = NULL, lag = NULL) {
  # Check parameters
  if(missing(data_source) || missing(signals) || missing(time_type) || missing(geo_type) || missing(time_values) || missing(geo_values)) {
    stop('`data_source`, `signals`, `time_type`, `geo_type`, `time_values`, and `geo_value` are all required')
  }
  if(!missing(issues) && !missing(lag)) {
    stop('`issues` and `lag` are mutually exclusive')
  }
  check_single_string_param('data_source', data_source)
  check_string_param('signals', signals)
  check_single_string_param('time_type', time_type)
  check_single_string_param('geo_type', geo_type)
  check_epirange_param('time_values', time_values)
  check_string_param('geo_values', geo_values)
  check_single_epirange_param('as_of', as_of, FALSE)
  check_epirange_param('issues', issues, FALSE)
  check_single_int_param('lag', lag, FALSE)

  create_epidata_call("covidcast/", list(data_source=data_source, signals=signals, time_type=time_type, geo_type=geo_type, time_values=time_values, geo_values=geo_values, as_of=as_of, issues=issues, lag=lag))
}

#'
#' fetch Delphi's forecast
#' @param system system to fetch
#' @param epiweek epiweek to fetch
#' @return an instance of EpiDataCall
#'
#' @export
delphi <- function(system, epiweek) {
  check_single_string_param('system', system)
  check_single_epirange_param('epiweek', epiweek)

  create_epidata_call("delphi/", list(system=system, epiweek=epiweek))
}

#'
#' fetch Delphi's PAHO Dengue nowcast
#' @param locations locations to fetch
#' @param epiweeks epiweeks to fetch
#' @return an instance of EpiDataCall
#'
#' @export
dengue_nowcast <- function(locations, epiweeks) {
  check_string_param('locations', locations)
  check_epirange_param('epiweeks', epiweeks)

  create_epidata_call("dengue_nowcast/", list(locations=locations, epiweeks=epiweeks))
}

#'
#' fetch Delphi's digital surveillance sensors
#' @param auth authenfication token
#' @param names names to fetch
#' @param locations locations to fetch
#' @param epiweeks epiweeks to fetch
#' @return an instance of EpiDataCall
#'
#' @export
dengue_sensors <- function(auth, names, locations, epiweeks) {
  check_single_string_param('auth', auth)
  check_string_param('names', names)
  check_string_param('locations', locations)
  check_epirange_param('epiweeks', epiweeks)

  create_epidata_call("dengue_sensors/", list(auth=auth, names=names, locations=locations, epiweeks=epiweeks))
}


#'
#' fetch ECDC data
#'
#' @param regions regions to fetch
#' @param epiweeks epiweeks to fetch
#' @param issues optionally specify the exact issues to fetch
#' @param lag optionally specify the issue lag
#' @return an instance of EpiDataCall
#'
#' @export
ecdc_ili <- function(regions, epiweeks, issues=NULL, lag=NULL) {
  check_string_param('regions', regions)
  check_epirange_param('epiweeks', epiweeks)
  check_epirange_param('issues', issues, FALSE)
  check_single_int_param('lag', lag, FALSE)
  if(!missing(issues) && !missing(lag)) {
    stop('`issues` and `lag` are mutually exclusive')
  }
  create_epidata_call("ecdc_ili/", list(regions=regions, epiweeks=epiweeks, issues=issues, lag=lag))
}

#'
#' fetch FluSurv virological data
#'
#' @param locations locations to fetch
#' @param epiweeks epiweeks to fetch
#' @param issues optionally specify the exact issues to fetch
#' @param lag optionally specify the issue lag
#' @return an instance of EpiDataCall
#'
#' @export
flusurv <- function(locations, epiweeks, issues=NULL, lag=NULL) {
  check_string_param('locations', locations)
  check_epirange_param('epiweeks', epiweeks)
  check_epirange_param('issues', issues, FALSE)
  check_single_int_param('lag', lag, FALSE)
  if(!missing(issues) && !missing(lag)) {
    stop('`issues` and `lag` are mutually exclusive')
  }
  create_epidata_call("flusurv/", list(locations=locations, epiweeks=epiweeks, issues=issues, lag=lag))
}

#'
#' fetch FluView virological data
#'
#' @param regions regions to fetch
#' @param epiweeks epiweeks to fetch
#' @param issues optionally specify the exact issues to fetch
#' @param lag optionally specify the issue lag
#' @return an instance of EpiDataCall
#'
#' @export
fluview_clinical <- function(regions, epiweeks, issues = NULL, lag = NULL) {
  check_string_param('regions', regions)
  check_epirange_param('epiweeks', epiweeks)
  check_epirange_param('issues', issues, FALSE)
  check_single_int_param('lag', lag, FALSE)
  if(!missing(issues) && !missing(lag)) {
    stop('`issues` and `lag` are mutually exclusive')
  }
  create_epidata_call("fluview_clinical/", list(regions=regions, epiweeks=epiweeks, issues=issues, lag=lag))
}

#'
#' fetch fluview meta data
#'
#' @return an instance of EpiDataCall
#'
#' @export
fluview_meta <- function() {
  create_epidata_call("fluview_meta/", list())
}

#'
#' fetch fluview data
#'
#' @param regions regions to fetch
#' @param epiweeks epiweeks to fetch
#' @param issues optionally specify the exact issues to fetch
#' @param lag optionally specify the issue lag
#' @param auth optional authentication
#' @return an instance of EpiDataCall
#'
#' @export
fluview <- function(regions, epiweeks, issues = NULL, lag = NULL, auth = NULL) {
  check_string_param('regions', regions)
  check_epirange_param('epiweeks', epiweeks)
  check_epirange_param('issues', issues, FALSE)
  check_single_int_param('lag', lag, FALSE)
  check_single_string_param('auth', auth, FALSE)
  if(!is.null(issues) && !is.null(lag)) {
    stop('`issues` and `lag` are mutually exclusive')
  }

  create_epidata_call("fluview/", list(regions=regions, epiweeks=epiweeks, issues=issues, lag=lag, auth=auth))
}

#'
#' fetch Google Flu Trends data
#'
#' @param locations locations to fetch
#' @param epiweeks epiweeks to fetch
#' @return an instance of EpiDataCall
#'
#' @export
gft <- function(locations, epiweeks) {
  check_string_param('locations', locations)
  check_epirange_param('epiweeks', epiweeks)
  create_epidata_call("gft/", list(locations=locations, epiweeks=epiweeks))
}

#'
#' fetch Google Health Trends data
#'
#' @param auth autentification
#' @param locations locations to fetch
#' @param epiweeks epiweeks to fetch
#' @param query query
#' @return an instance of EpiDataCall
#'
#' @export
ght <- function(auth, locations, epiweeks, query) {
  check_single_string_param('auth', auth)
  check_string_param('locations', locations)
  check_epirange_param('epiweeks', epiweeks)
  check_single_string_param('query', query)
  create_epidata_call("ght/", list(auth=auth, locations=locations, epiweeks=epiweeks, query=query))
}

#'
#' fetch KCDC data
#'
#' @param regions regions to fetch
#' @param epiweeks epiweeks to fetch
#' @param issues optionally specify the exact issues to fetch
#' @param lag optionally specify the issue lag
#' @return an instance of EpiDataCall
#'
#' @export
kcdc_ili <- function(regions, epiweeks, issues=NULL, lag=NULL) {
  check_string_param('regions', regions)
  check_epirange_param('epiweeks', epiweeks)
  check_epirange_param('issues', issues, FALSE)
  check_single_int_param('lag', lag, FALSE)
  if(!missing(issues) && !missing(lag)) {
    stop('`issues` and `lag` are mutually exclusive')
  }
  create_epidata_call("kcdc_ili/", list(regions=regions, epiweeks=epiweeks, issues=issues, lag=lag))
}


#' fetch AFHSB meta data
#'
#' @param auth authenfication token
#' @return an instance of EpiDataCall
#'
#' @export
meta_afhsb <- function(auth) {
  check_single_string_param('auth', auth)

  create_epidata_call("meta_afhsb/", list(auth=auth))
}

#' fetch NoroSTAT meta data
#'
#' @param auth authenfication token
#' @return an instance of EpiDataCall
#'
#' @export
meta_norostat <- function(auth) {
  check_single_string_param('auth', auth)

  create_epidata_call("meta_norostat/", list(auth=auth))
}

#'
#' fetch api meta data
#'
#' @return an instance of EpiDataCall
#'
#' @export
meta <- function() {
  create_epidata_call("meta/", list())
}

#'
#' fetch NIDSS dengue data
#' @param locations locations to fech
#' @param epiweeks epiweeks to fetch
#' @return an instance of EpiDataCall
#'
#' @export
nidss_dengue <- function(locations, epiweeks) {
  check_string_param('locations', locations)
  check_epirange_param('epiweeks', epiweeks)

  create_epidata_call("nidss_dengue/", list(locations=locations, epiweeks=epiweeks))
}

#'
#' fetch NIDSS dengue data
#' @param regions regions to fetch
#' @param epiweeks epiweeks to fetch
#' @param issues optional issues
#' @param lag optional lag
#' @return an instance of EpiDataCall
#'
#' @export
nidss_flu <- function(regions, epiweeks, issues = NULL, lag = NULL) {
  check_string_param('regions', regions)
  check_epirange_param('epiweeks', epiweeks)
  check_epirange_param('issues', issues, FALSE)
  check_single_int_param('lag', lag, FALSE)

  if(!is.null(issues) && !is.null(lag)) {
    stop('`issues` and `lag` are mutually exclusive')
  }

  create_epidata_call("nidss_flu/", list(regions=regions, epiweeks=epiweeks, issues=issues, lag=lag))
}


#' fetch NoroSTAT data (point data, no min/max)
#'
#' @param auth authenfication token
#' @param location location to fetch
#' @param epiweeks epiweeks to fetch
#' @return an instance of EpiDataCall
#'
#' @export
norostat <- function(auth, location, epiweeks) {
  check_single_string_param('auth', auth)
  check_single_string_param('locations', location)
  check_epirange_param('epiweeks', epiweeks)

  create_epidata_call("norostat/", list(auth=auth, location=location, epiweeks=epiweeks))
}

#'
#' fetch Delphi's wILI nowcast
#' @param locations locations to fetch
#' @param epiweeks epiweeks to fetch
#' @return an instance of EpiDataCall
#'
#' @export
nowcast <- function(locations, epiweeks) {
  check_string_param('locations', locations)
  check_epirange_param('epiweeks', epiweeks)

  create_epidata_call("nowcast/", list(locations=locations, epiweeks=epiweeks))
}

#' fetch Paho Dengue
#'
#' @param regions regions to fetch
#' @param epiweeks epiweeks to fetch
#' @param issues issues to fetch
#' @param lag lag to fetch
#' @return an instance of EpiDataCall
#'
#' @export
quidel <- function(regions, epiweeks, issues=None, lag=None) {
  check_string_param('regions', egions)
  check_epirange_param('epiweeks', epiweeks)
  check_epirange_param('issues', issues, FALSE)
  check_single_int_param('lag', lag, FALSE)

  create_epidata_call("quidel/", list(regions=regions, epiweeks=epiweeks, issues=issues, lag=lag`))
}


#' fetch Quidel data
#'
#' @param auth authenfication token
#' @param epiweeks epiweeks to fetch
#' @param locations locations to fetch
#' @return an instance of EpiDataCall
#'
#' @export
quidel <- function(auth, epiweeks, locations) {
  check_single_string_param('auth', auth)
  check_epirange_param('epiweeks', epiweeks)
  check_string_param('locations', locations)

  create_epidata_call("quidel/", list(auth=auth, epiweeks=epiweeks, locations=locations))
}

#'
#' fetch Delphi's digital surveillance sensors
#' @param auth authenfication token
#' @param names names to fetch
#' @param locations locations to fetch
#' @param epiweeks epiweeks to fetch
#' @return an instance of EpiDataCall
#'
#' @export
sensors <- function(auth, names, locations, epiweeks) {
  check_single_string_param('auth', auth)
  check_string_param('names', names)
  check_string_param('locations', locations)
  check_epirange_param('epiweeks', epiweeks)

  create_epidata_call("sensors/", list(auth=auth, names=names, locations=locations, epiweeks=epiweeks))
}

#'
#' fetch HealthTweets data
#'
#' @param auth autentification
#' @param locations locations to fetch
#' @param dates epiweeks to fetch
#' @param epiweeks epiweeks to fetch
#' @return an instance of EpiDataCall
#'
#' @export
twitter <- function(auth, locations, dates = NULL, epiweeks = NULL) {
  check_single_string_param('auth', auth)
  check_string_param('locations', locations)
  check_epirange_param('dates', dates, FALSE)
  check_epirange_param('epiweeks', epiweeks, FALSE)
  if(!xor(missing(dates), missing(epiweeks))) {
    stop('exactly one of `dates` and `epiweeks` is required')
  }
  create_epidata_call("twitter/", list(auth=auth, locations=locations, dates=dates, epiweeks=epiweeks))
}

#'
#' fetch Wikipedia access data
#'
#' @param articles articles to fetch
#' @param dates dates to fetch
#' @param epiweeks epiweeks to fetch
#' @param hours hours to fetch
#' @param language language
#' @return an instance of EpiDataCall
#'
#' @export
wiki <- function(articles, dates = NULL, epiweeks = NULL, hours = NULL, language='en') {
  check_string_param('articles', articles)
  check_epirange_param('dates', dates, FALSE)
  check_epirange_param('epiweeks', epiweeks, FALSE)
  check_int_param('hours', hours, FALSE)
  check_single_string_param('language', language, FALSE)
  if(!xor(missing(dates), missing(epiweeks))) {
    stop('exactly one of `dates` and `epiweeks` is required')
  }
  create_epidata_call("wiki/", list(articles=articles, dates=dates, epiweeks=epiweeks, hours=hours, language=language))
}