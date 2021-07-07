
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
#' fetch fluview meta data
#'
#' @return an instance of EpiDataCall
#'
#' @export
fluview_meta <- function() {
  create_epidata_call("fluview_meta/", list())
}

# # Fetch FluView virological data
# fluview_clinical <- function(regions, epiweeks, issues, lag) {
#   # Check parameters
#   if(missing(regions) || missing(epiweeks)) {
#     stop('`regions` and `epiweeks` are both required')
#   }
#   if(!missing(issues) && !missing(lag)) {
#     stop('`issues` and `lag` are mutually exclusive')
#   }
#   # Set up request
#   params <- list(
#     endpoint = 'fluview_clinical',
#     regions = .list(regions),
#     epiweeks = .list(epiweeks)
#   )
#   if(!missing(issues)) {
#     params$issues <- .list(issues)
#   }
#   if(!missing(lag)) {
#     params$lag <- lag
#   }
#   # Make the API call
#   return(.request(params))
# }

# # Fetch FluSurv data
# flusurv <- function(locations, epiweeks, issues, lag) {
#   # Check parameters
#   if(missing(locations) || missing(epiweeks)) {
#     stop('`locations` and `epiweeks` are both required')
#   }
#   if(!missing(issues) && !missing(lag)) {
#     stop('`issues` and `lag` are mutually exclusive')
#   }
#   # Set up request
#   params <- list(
#     endpoint = 'flusurv',
#     locations = .list(locations),
#     epiweeks = .list(epiweeks)
#   )
#   if(!missing(issues)) {
#     params$issues <- .list(issues)
#   }
#   if(!missing(lag)) {
#     params$lag <- lag
#   }
#   # Make the API call
#   return(.request(params))
# }

# # Fetch ECDC data
# ecdc_ili <- function(regions, epiweeks, issues, lag) {
#   # Check parameters
#   if(missing(regions) || missing(epiweeks)) {
#     stop('`regions` and `epiweeks` are both required')
#   }
#   if(!missing(issues) && !missing(lag)) {
#     stop('`issues` and `lag` are mutually exclusive')
#   }
#   # Set up request
#   params <- list(
#     endpoint = 'ecdc_ili',
#     regions = .list(regions),
#     epiweeks = .list(epiweeks)
#   )
#   if(!missing(issues)) {
#     params$issues <- .list(issues)
#   }
#   if(!missing(lag)) {
#     params$lag <- lag
#   }
#   # Make the API call
#   return(.request(params))
# }

# # Fetch KCDC data
# kcdc_ili <- function(regions, epiweeks, issues, lag) {
#   # Check parameters
#   if(missing(regions) || missing(epiweeks)) {
#     stop('`regions` and `epiweeks` are both required')
#   }
#   if(!missing(issues) && !missing(lag)) {
#     stop('`issues` and `lag` are mutually exclusive')
#   }
#   # Set up request
#   params <- list(
#     endpoint = 'kcdc_ili',
#     regions = .list(regions),
#     epiweeks = .list(epiweeks)
#   )
#   if(!missing(issues)) {
#     params$issues <- .list(issues)
#   }
#   if(!missing(lag)) {
#     params$lag <- lag
#   }
#   # Make the API call
#   return(.request(params))
# }


# # Fetch Google Flu Trends data
# gft <- function(locations, epiweeks) {
#   # Check parameters
#   if(missing(locations) || missing(epiweeks)) {
#     stop('`locations` and `epiweeks` are both required')
#   }
#   # Set up request
#   params <- list(
#     endpoint = 'gft',
#     locations = .list(locations),
#     epiweeks = .list(epiweeks)
#   )
#   # Make the API call
#   return(.request(params))
# }

# # Fetch Google Health Trends data
# ght <- function(auth, locations, epiweeks, query) {
#   # Check parameters
#   if(missing(auth) || missing(locations) || missing(epiweeks) || missing(query)) {
#     stop('`auth`, `locations`, `epiweeks`, and `query` are all required')
#   }
#   # Set up request
#   params <- list(
#     endpoint = 'ght',
#     auth = auth,
#     locations = .list(locations),
#     epiweeks = .list(epiweeks),
#     query = query
#   )
#   # Make the API call
#   return(.request(params))
# }

# # Fetch HealthTweets data
# twitter <- function(auth, locations, dates, epiweeks) {
#   # Check parameters
#   if(missing(auth) || missing(locations)) {
#     stop('`auth` and `locations` are both required')
#   }
#   if(!xor(missing(dates), missing(epiweeks))) {
#     stop('exactly one of `dates` and `epiweeks` is required')
#   }
#   # Set up request
#   params <- list(
#     endpoint = 'twitter',
#     auth = auth,
#     locations = .list(locations)
#   )
#   if(!missing(dates)) {
#     params$dates <- .list(dates)
#   }
#   if(!missing(epiweeks)) {
#     params$epiweeks <- .list(epiweeks)
#   }
#   # Make the API call
#   return(.request(params))
# }

# # Fetch Wikipedia access data
# wiki <- function(articles, dates, epiweeks, hours, language='en') {
#   # Check parameters
#   if(missing(articles)) {
#     stop('`articles` is required')
#   }
#   if(!xor(missing(dates), missing(epiweeks))) {
#     stop('exactly one of `dates` and `epiweeks` is required')
#   }
#   # Set up request
#   params <- list(
#     endpoint = 'wiki',
#     articles = .list(articles),
#     language = language
#   )
#   if(!missing(dates)) {
#     params$dates <- .list(dates)
#   }
#   if(!missing(epiweeks)) {
#     params$epiweeks <- .list(epiweeks)
#   }
#   if(!missing(hours)) {
#     params$hours <- .list(hours)
#   }
#   # Make the API call
#   return(.request(params))
# }

# # Fetch CDC page hits
# cdc <- function(auth, epiweeks, locations) {
#   # Check parameters
#   if(missing(auth) || missing(epiweeks) || missing(locations)) {
#     stop('`auth`, `epiweeks`, and `locations` are all required')
#   }
#   # Set up request
#   params <- list(
#     endpoint = 'cdc',
#     auth = auth,
#     epiweeks = .list(epiweeks),
#     locations = .list(locations)
#   )
#   # Make the API call
#   return(.request(params))
# }

# # Fetch Quidel data
# quidel <- function(auth, epiweeks, locations) {
#   # Check parameters
#   if(missing(auth) || missing(epiweeks) || missing(locations)) {
#     stop('`auth`, `epiweeks`, and `locations` are all required')
#   }
#   # Set up request
#   params <- list(
#     endpoint = 'quidel',
#     auth = auth,
#     epiweeks = .list(epiweeks),
#     locations = .list(locations)
#   )
#   # Make the API call
#   return(.request(params))
# }

# # Fetch NoroSTAT data (point data, no min/max)
# norostat <- function(auth, location, epiweeks) {
#   # Check parameters
#   if(missing(auth) || missing(location) || missing(epiweeks)) {
#     stop('`auth`, `location`, and `epiweeks` are all required')
#   }
#   # Set up request
#   params <- list(
#       endpoint = 'norostat',
#       auth = auth,
#       location = location,
#       epiweeks = .list(epiweeks)
#   )
#   # Make the API call
#   return(.request(params))
# }

# # Fetch NoroSTAT metadata
# meta_norostat <- function(auth) {
#   # Check parameters
#   if(missing(auth)) {
#     stop('`auth` is required')
#   }
#   # Set up request
#   params <- list(
#     endpoint = 'meta_norostat',
#     auth = auth
#   )
#   # Make the API call
#   return(.request(params))
# }

# # Fetch AFHSB data (point data, no min/max)
# afhsb <- function(auth, locations, epiweeks, flu_types) {
#   # Check parameters
#   if(missing(auth) || missing(locations) || missing(epiweeks) || missing(flu_types)) {
#     stop('`auth`, `locations`, `epiweeks` and `flu_types` are all required')
#   }
#   # Set up request
#   params <- list(
#       endpoint = 'afhsb',
#       auth = auth,
#       locations = .list(locations),
#       epiweeks = .list(epiweeks),
#       flu_types = .list(flu_types)
#   )
#   # Make the API call
#   return(.request(params))
# }

# # Fetch AFHSB metadata
# meta_afhsb <- function(auth) {
#   # Check parameters
#   if(missing(auth)) {
#     stop('`auth` is required')
#   }
#   # Set up request
#   params <- list(
#     endpoint = 'meta_afhsb',
#     auth = auth
#   )
#   # Make the API call
#   return(.request(params))
# }

# # Fetch NIDSS flu data
# nidss.flu <- function(regions, epiweeks, issues, lag) {
#   # Check parameters
#   if(missing(regions) || missing(epiweeks)) {
#     stop('`regions` and `epiweeks` are both required')
#   }
#   if(!missing(issues) && !missing(lag)) {
#     stop('`issues` and `lag` are mutually exclusive')
#   }
#   # Set up request
#   params <- list(
#     endpoint = 'nidss_flu',
#     regions = .list(regions),
#     epiweeks = .list(epiweeks)
#   )
#   if(!missing(issues)) {
#     params$issues <- .list(issues)
#   }
#   if(!missing(lag)) {
#     params$lag <- lag
#   }
#   # Make the API call
#   return(.request(params))
# }

# # Fetch NIDSS dengue data
# nidss.dengue <- function(locations, epiweeks) {
#   # Check parameters
#   if(missing(locations) || missing(epiweeks)) {
#     stop('`locations` and `epiweeks` are both required')
#   }
#   # Set up request
#   params <- list(
#     endpoint = 'nidss_dengue',
#     locations = .list(locations),
#     epiweeks = .list(epiweeks)
#   )
#   # Make the API call
#   return(.request(params))
# }

# # Fetch Delphi's forecast
# delphi <- function(system, epiweek) {
#   # Check parameters
#   if(missing(system) || missing(epiweek)) {
#     stop('`system` and `epiweek` are both required')
#   }
#   # Set up request
#   params <- list(
#     endpoint = 'delphi',
#     system = system,
#     epiweek = epiweek
#   )
#   # Make the API call
#   return(.request(params))
# }

# # Fetch Delphi's digital surveillance sensors
# sensors <- function(auth, names, locations, epiweeks) {
#   # Check parameters
#   if(missing(auth) || missing(names) || missing(locations) || missing(epiweeks)) {
#     stop('`auth`, `names`, `locations`, and `epiweeks` are all required')
#   }
#   # Set up request
#   params <- list(
#     endpoint = 'sensors',
#     auth = auth,
#     names = .list(names),
#     locations = .list(locations),
#     epiweeks = .list(epiweeks)
#   )
#   # Make the API call
#   return(.request(params))
# }

# # Fetch Delphi's digital surveillance sensors
# dengue_sensors <- function(auth, names, locations, epiweeks) {
#   # Check parameters
#   if(missing(auth) || missing(names) || missing(locations) || missing(epiweeks)) {
#     stop('`auth`, `names`, `locations`, and `epiweeks` are all required')
#   }
#   # Set up request
#   params <- list(
#     endpoint = 'dengue_sensors',
#     auth = auth,
#     names = .list(names),
#     locations = .list(locations),
#     epiweeks = .list(epiweeks)
#   )
#   # Make the API call
#   return(.request(params))
# }

# # Fetch Delphi's wILI nowcast
# nowcast <- function(locations, epiweeks) {
#   # Check parameters
#   if(missing(locations) || missing(epiweeks)) {
#     stop('`locations` and `epiweeks` are both required')
#   }
#   # Set up request
#   params <- list(
#     endpoint = 'nowcast',
#     locations = .list(locations),
#     epiweeks = .list(epiweeks)
#   )
#   # Make the API call
#   return(.request(params))
# }

# # Fetch Delphi's PAHO Dengue nowcast
# dengue_nowcast <- function(locations, epiweeks) {
#   # Check parameters
#   if(missing(locations) || missing(epiweeks)) {
#     stop('`locations` and `epiweeks` are both required')
#   }
#   # Set up request
#   params <- list(
#     endpoint = 'dengue_nowcast',
#     locations = .list(locations),
#     epiweeks = .list(epiweeks)
#   )
#   # Make the API call
#   return(.request(params))
# }

# # Fetch API metadata
# meta <- function() {
#   return(.request(list(endpoint='meta')))
# }

#'
#' fetch covidcast data
#'
#' @param data_source data source to fetch
#' @param signals data source to fetch
#' @param time_type data source to fetch
#' @param time_values data source to fetch
#' @param geo_values data source to fetch
#' @param as_of data source to fetch
#' @param issues data source to fetch
#' @param lag data source to fetch
#' @return an instance of EpiDataCall
#'
#' @export
covidcast <- function(data_source, signals, time_type, geo_type, time_values, geo_values, as_of = NULL, issues = NULL, lag = NULL) {
  # Check parameters
  if(missing(data_source) || (missing(signals) && missing(signal)) || missing(time_type) || missing(geo_type) || missing(time_values) || missing(geo_values)) {
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

# # Fetch Delphi's COVID-19 Surveillance Streams metadata
covidcast_meta <- function() {
  create_epidata_call("covidcast_meta/", list())
}

# # Fetch COVID hospitalization data
# covid_hosp <- function(states, dates, issues) {
#   # Check parameters
#   if(missing(states) || missing(dates)) {
#     stop('`states` and `dates` are both required')
#   }
#   # Set up request
#   params <- list(
#     endpoint = 'covid_hosp',
#     states = .list(states),
#     dates = .list(dates)
#   )
#   if(!missing(issues)) {
#     params$issues <- .list(issues)
#   }
#   # Make the API call
#   return(.request(params))
# }

# # Fetch COVID hospitalization data for specific facilities
# covid_hosp_facility <- function(hospital_pks, collection_weeks, publication_dates) {
#   # Check parameters
#   if(missing(hospital_pks) || missing(collection_weeks)) {
#     stop('`hospital_pks` and `collection_weeks` are both required')
#   }
#   # Set up request
#   params <- list(
#     source = 'covid_hosp_facility',
#     hospital_pks = .list(hospital_pks),
#     collection_weeks = .list(collection_weeks)
#   )
#   if(!missing(publication_dates)) {
#     params$publication_dates <- .list(publication_dates)
#   }
#   # Make the API call
#   return(.request(params))
# }

# # Lookup COVID hospitalization facility identifiers
# covid_hosp_facility_lookup <- function(state, ccn, city, zip, fips_code) {
#   # Set up request
#   params <- list(source = 'covid_hosp_facility_lookup')
#   if(!missing(state)) {
#     params$state <- state
#   } else if(!missing(ccn)) {
#     params$ccn <- ccn
#   } else if(!missing(city)) {
#     params$city <- city
#   } else if(!missing(zip)) {
#     params$zip <- zip
#   } else if(!missing(fips_code)) {
#     params$fips_code <- fips_code
#   } else {
#     stop('one of `state`, `ccn`, `city`, `zip`, or `fips_code` is required')
#   }
#   # Make the API call
#   return(.request(params))
# }