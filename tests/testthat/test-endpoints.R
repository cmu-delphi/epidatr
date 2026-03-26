test_that("basic_epidata_call", {
  expect_no_error(pvt_cdc(
    auth = "yourkey",
    "fl,ca",
    epirange(201501, 201601),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_covid_hosp_facility_lookup(
    state = "fl",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_covid_hosp_facility(
    hospital_pks = "100075",
    collection_weeks = epirange(20200101, 20200501),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_covid_hosp_state_timeseries(
    states = "fl",
    dates = epirange(20200101, 20200501),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_covidcast_meta(
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_covidcast(
    source = "jhu-csse",
    signals = "confirmed_7dav_incidence_prop",
    time_type = "day",
    geo_type = "state",
    time_values = epirange(20200601, 20200801),
    geo_values = "ca,fl",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_delphi(
    system = "ec",
    epiweek = 202006,
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_dengue_nowcast(
    locations = "ca",
    epiweeks = epirange(201501, 202001),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pvt_dengue_sensors(
    auth = "yourkey",
    names = "ght",
    locations = "ag",
    epiweeks = epirange(201501, 202001),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_ecdc_ili(
    regions = "austria",
    epiweeks = epirange(201201, 202001),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_flusurv(
    locations = "CA",
    epiweeks = epirange(201201, 202001),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_fluview_clinical(
    regions = "nat",
    epiweeks = epirange(201601, 201701),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_fluview_meta(
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_fluview(
    regions = "nat",
    epiweeks = epirange(201601, 201701),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_gft(
    locations = "hhs1",
    epiweeks = epirange(201201, 202001),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pvt_ght(
    auth = "yourkey",
    locations = "ca",
    epiweeks = epirange(201201, 202001),
    query = "how to get over the flu",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_kcdc_ili(
    regions = "ROK",
    epiweeks = epirange(201201, 202001),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pvt_meta_norostat(
    auth = "yourkey",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_meta(
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_nidss_dengue(
    locations = "taipei",
    epiweeks = epirange(201201, 202001),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_nidss_flu(
    regions = "taipei",
    epiweeks = epirange(201201, 202001),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pvt_norostat(
    auth = "yourkey",
    locations = "Minnesota, Ohio, Oregon, Tennessee, and Wisconsin",
    epiweeks = epirange(201201, 202001),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_nowcast(
    locations = "ca",
    epiweeks = epirange(201201, 202001),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_paho_dengue(
    regions = "ca",
    epiweeks = epirange(201201, 202001),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pvt_quidel(
    auth = "yourkey",
    locations = "hhs1",
    epiweeks = epirange(201201, 202001),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pvt_sensors(
    auth = "yourkey",
    names = "sar3",
    locations = "nat",
    epiweeks = epirange(201501, 202001),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pvt_twitter(
    auth = "yourkey",
    locations = "CA",
    time_type = "week",
    time_values = epirange(201501, 202001),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pvt_twitter(
    auth = "yourkey",
    locations = "CA",
    time_type = "day",
    time_values = epirange(20150101, 20200101),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_wiki(
    articles = "avian_influenza",
    time_type = "week",
    time_values = epirange(201501, 202001),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_wiki(
    articles = "avian_influenza",
    time_type = "day",
    time_values = epirange(20150101, 20200101),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_cast(
    source = "nssp",
    signals = "sig1",
    geo_type = "state",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_cast_meta(
    source = "nssp",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
})

test_that("endoints accept wildcard for date parameter", {
  expect_no_error(call <- pvt_cdc(
    auth = "yourkey",
    "fl,ca",
    "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_match(call$request$url, "epiweeks=100001-300001")

  expect_no_error(call <- pub_covid_hosp_facility(
    hospital_pks = "100075",
    collection_weeks = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_match(call$request$url, "collection_weeks=10000101-30000101")

  expect_no_error(call <- pub_covid_hosp_state_timeseries(
    states = "fl",
    dates = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_match(call$request$url, "dates=10000101-30000101")

  expect_no_error(call <- pub_covidcast(
    source = "jhu-csse",
    signals = "confirmed_7dav_incidence_prop",
    time_type = "day",
    geo_type = "state",
    time_values = "*",
    geo_values = "ca,fl",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_match(call$request$url, "time_values=%2A")

  expect_no_error(call <- pub_dengue_nowcast(
    locations = "ca",
    epiweeks = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_match(call$request$url, "epiweeks=100001-300001")

  expect_no_error(call <- pvt_dengue_sensors(
    auth = "yourkey",
    names = "ght",
    locations = "ag",
    epiweeks = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_match(call$request$url, "epiweeks=100001-300001")

  expect_no_error(call <- pub_ecdc_ili(
    regions = "austria",
    epiweeks = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_match(call$request$url, "epiweeks=100001-300001")

  expect_no_error(call <- pub_flusurv(
    locations = "CA",
    epiweeks = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_match(call$request$url, "epiweeks=100001-300001")

  expect_no_error(call <- pub_fluview_clinical(
    regions = "nat",
    epiweeks = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_match(call$request$url, "epiweeks=100001-300001")

  expect_no_error(call <- pub_fluview(
    regions = "nat",
    epiweeks = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_match(call$request$url, "epiweeks=100001-300001")

  expect_no_error(call <- pub_gft(
    locations = "hhs1",
    epiweeks = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_match(call$request$url, "epiweeks=100001-300001")

  expect_no_error(call <- pvt_ght(
    auth = "yourkey",
    locations = "ca",
    epiweeks = "*",
    query = "how to get over the flu",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_match(call$request$url, "epiweeks=100001-300001")

  expect_no_error(call <- pub_kcdc_ili(
    regions = "ROK",
    epiweeks = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_match(call$request$url, "epiweeks=100001-300001")

  expect_no_error(call <- pub_nidss_dengue(
    locations = "taipei",
    epiweeks = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_match(call$request$url, "epiweeks=100001-300001")

  expect_no_error(call <- pub_nidss_flu(
    regions = "taipei",
    epiweeks = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_match(call$request$url, "epiweeks=100001-300001")

  expect_no_error(call <- pvt_norostat(
    auth = "yourkey",
    locations = "Minnesota, Ohio, Oregon, Tennessee, and Wisconsin",
    epiweeks = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_match(call$request$url, "epiweeks=100001-300001")

  expect_no_error(call <- pub_nowcast(
    locations = "ca",
    epiweeks = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_match(call$request$url, "epiweeks=100001-300001")

  expect_no_error(call <- pub_paho_dengue(
    regions = "ca",
    epiweeks = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_match(call$request$url, "epiweeks=100001-300001")

  expect_no_error(call <- pvt_quidel(
    auth = "yourkey",
    locations = "hhs1",
    epiweeks = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_match(call$request$url, "epiweeks=100001-300001")

  expect_no_error(call <- pvt_sensors(
    auth = "yourkey",
    names = "sar3",
    locations = "nat",
    epiweeks = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_match(call$request$url, "epiweeks=100001-300001")

  expect_no_error(call <- pvt_twitter(
    auth = "yourkey",
    locations = "CA",
    time_type = "week",
    time_values = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_match(call$request$url, "epiweeks=100001-300001")

  expect_no_error(call <- pvt_twitter(
    auth = "yourkey",
    locations = "CA",
    time_type = "day",
    time_values = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_match(call$request$url, "dates=10000101-30000101")

  expect_no_error(call <- pub_wiki(
    articles = "avian_influenza",
    time_type = "week",
    time_values = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_match(call$request$url, "epiweeks=100001-300001")

  expect_no_error(call <- pub_wiki(
    articles = "avian_influenza",
    time_type = "day",
    time_values = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_match(call$request$url, "dates=10000101-30000101")
})

test_that("endpoints fail when given args via dots", {
  dots_error <- "`...` must be empty"

  # time value/epiweek arg is passed erroneously as `date_range`
  expect_error(
    pub_covid_hosp_facility_lookup(
      state = "fl",
      date_range = 20200101
    ),
    regexp = dots_error
  )
  expect_error(
    pub_covid_hosp_facility(
      hospital_pks = "100075",
      date_range = epirange(20200101, 20200501)
    ),
    regexp = dots_error
  )
  expect_error(
    pub_covid_hosp_state_timeseries(
      states = "fl",
      date_range = epirange(20200101, 20200501)
    ),
    regexp = dots_error
  )
  expect_error(
    pub_covidcast(
      source = "jhu-csse",
      signals = "confirmed_7dav_incidence_prop",
      time_type = "day",
      geo_type = "state",
      date_range = epirange(20200601, 20200801),
      geo_values = "ca,fl"
    ),
    regexp = dots_error
  )
  expect_error(
    pub_ecdc_ili(
      regions = "austria",
      date_range = epirange(201201, 202001)
    ),
    regexp = dots_error
  )
  expect_error(
    pub_flusurv(
      locations = "CA",
      date_range = epirange(201201, 202001)
    ),
    regexp = dots_error
  )
  expect_error(
    pub_fluview_clinical(
      regions = "nat",
      date_range = epirange(201601, 201701)
    ),
    regexp = dots_error
  )
  expect_error(
    pub_fluview(
      regions = "nat",
      date_range = epirange(201601, 201701)
    ),
    regexp = dots_error
  )
  expect_error(
    pub_kcdc_ili(
      regions = "ROK",
      date_range = epirange(201201, 202001)
    ),
    regexp = dots_error
  )
  expect_error(
    pub_nidss_flu(
      regions = "taipei",
      date_range = epirange(201201, 202001)
    ),
    regexp = dots_error
  )
  expect_error(
    pub_paho_dengue(
      regions = "ca",
      date_range = epirange(201201, 202001)
    ),
    regexp = dots_error
  )
  expect_error(
    pvt_twitter(
      auth = "yourkey",
      locations = "CA",
      time_type = "week",
      time_range = epirange(201501, 202001)
    ),
    regexp = dots_error
  )
  expect_error(
    pub_wiki(
      articles = "avian_influenza",
      time_type = "week",
      date_range = epirange(201501, 202001)
    ),
    regexp = dots_error
  )
})

test_that("pub_covid_hosp_state_timeseries supports versioned queries", {
  epidata_call <- pub_covid_hosp_state_timeseries(
    "ut", epirange(12340101, 34560101),
    issues = 20220101,
    fetch_args = fetch_args_list(
      fields = c(
        "state", "geocoded_state", "date", "issue",
        "previous_day_admission_influenza_confirmed",
        "previous_day_admission_influenza_confirmed_coverage"
      ),
      disable_date_parsing = TRUE,
      dry_run = TRUE
    )
  )
  expect_match(epidata_call$request$url, "issues=20220101")
  expect_no_match(epidata_call$request$url, "as_of=")
  expect_no_match(epidata_call$request$url, "lag=")

  epidata_call <- pub_covid_hosp_state_timeseries(
    "ut", epirange(12340101, 34560101),
    as_of = 20220101,
    fetch_args = fetch_args_list(
      fields = c(
        "state", "geocoded_state", "date", "issue",
        "previous_day_admission_influenza_confirmed",
        "previous_day_admission_influenza_confirmed_coverage"
      ),
      disable_date_parsing = TRUE,
      dry_run = TRUE
    )
  )
  expect_no_match(epidata_call$request$url, "issues=")
  expect_match(epidata_call$request$url, "as_of=20220101")
  expect_no_match(epidata_call$request$url, "lag=")
})

test_that("nchs-mortality call fails if time_type not week", {
  expect_error(pub_covidcast(
    source = "nchs-mortality",
    signals = "signal",
    time_type = "day",
    geo_type = "state",
    time_values = "*",
    geo_values = "*"
  ), class = "epidatr__nchs_week_only")
})

test_that("pub_covidcast catches missing args for args without defaults", {
  expect_no_error(pub_covidcast(
    source = "jhu-csse",
    signals = "confirmed_7dav_incidence_prop",
    time_type = "day",
    geo_type = "state",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_error(
    pub_covidcast(
      signals = "confirmed_7dav_incidence_prop",
      time_type = "day",
      geo_type = "state"
    ),
    class = "epidatr__pub_covidcast__missing_required_args"
  )
  expect_error(
    pub_covidcast(
      source = "jhu-csse",
      time_type = "day",
      geo_type = "state"
    ),
    class = "epidatr__pub_covidcast__missing_required_args"
  )
  expect_error(
    pub_covidcast(
      source = "jhu-csse",
      signals = "confirmed_7dav_incidence_prop",
      geo_type = "state"
    ),
    class = "epidatr__pub_covidcast__missing_required_args"
  )
  expect_error(
    pub_covidcast(
      source = "jhu-csse",
      signals = "confirmed_7dav_incidence_prop",
      time_type = "day"
    ),
    class = "epidatr__pub_covidcast__missing_required_args"
  )
})

test_that("pub_covid_hosp_state_timeseries catches missing args for args without defaults", {
  expect_no_error(pub_covid_hosp_state_timeseries(
    states = "fl",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ))
  expect_error(
    pub_covid_hosp_state_timeseries(),
    class = "epidatr__pub_covid_hosp_state_timeseries__missing_required_args"
  )
})

test_that("pub_cast and pub_cast_meta work as expected", {
  local_mocked_bindings(
    do_request = function(epidata_call, format_type, ...) {
      if (format_type == "json") {
        expect_match(epidata_call$request$url, "metadata/")
        json_resp <- list(nssp = list(signals = c("sig1", "sig2"), geo_types = c("state", "nation")))
        return(to_httr2_response(jsonlite::toJSON(json_resp, auto_unbox = TRUE)))
      } else {
        expect_equal(epidata_call$api_version, "cast")
        csv_data <- "signal,geo_value,time_value,value\nsig1,ca,2024-01-01,10.5\nsig1,fl,2024-01-01,20.0"
        return(to_httr2_response(csv_data))
      }
    },
    .package = "epidatr"
  )

  # Test pub_cast_meta
  res_meta <- pub_cast_meta(source = "nssp")
  expect_type(res_meta, "list")
  expect_equal(res_meta$nssp$signals, c("sig1", "sig2"))

  # Test pub_cast basic fetch
  res <- pub_cast(source = "nssp", signals = "sig1", geo_type = "state")
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)

  # Test pub_cast filtering
  res_filtered <- pub_cast(source = "nssp", signals = "sig1", geo_type = "state", geo_values = "ca")
  expect_equal(nrow(res_filtered), 1)

  res_time_filtered <- pub_cast(
    source = "nssp", signals = "sig1", geo_type = "state",
    time_values = as.Date("2024-01-01")
  )
  expect_equal(nrow(res_time_filtered), 2)

  # Test EpiRange mapping in issues
  res_range <- pub_cast(
    source = "nssp",
    signals = "sig1",
    geo_type = "state",
    issues = epirange("2024-01-01", "2024-01-05")
  )
  expect_s3_class(res_range, "tbl_df")

  # Test issues = "*" mapping in pub_cast
  call_wildcard <- pub_cast(
    source = "nssp",
    signals = "sig1",
    geo_type = "state",
    issues = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
  expect_match(call_wildcard$request$url, "archive/")
  expect_no_match(call_wildcard$request$url, "version_query=")

  # Test as_of = "*" mapping in pub_cast
  call_as_of_wildcard <- pub_cast(
    source = "nssp",
    signals = "sig1",
    geo_type = "state",
    as_of = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
  expect_match(call_as_of_wildcard$request$url, "archive/")
  expect_no_match(call_as_of_wildcard$request$url, "snapshot_date=")
})

test_that("pub_cast validations", {
  # Missing required args
  expect_error(
    pub_cast(source = "nssp", signals = "sig1"),
    class = "epidatr__pub_cast__missing_required_args"
  )

  # Mutually exclusive as_of and issues
  expect_error(
    pub_cast(
      source = "nssp",
      signals = "sig1",
      geo_type = "state",
      as_of = "2024-01-01",
      issues = "<2024-01-01"
    ),
    class = "epidatr__pub_cast__too_many_issue_params"
  )
})
