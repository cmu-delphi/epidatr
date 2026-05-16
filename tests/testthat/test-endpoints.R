test_that("endpoints reject unknown args via dots (pub_covidcast smoke)", {
  # Representative smoke: rlang::check_dots_empty is called from the shared
  # endpoint entry path, so one endpoint covers the contract for all.
  expect_error(
    pub_covidcast(
      source = "jhu-csse",
      signals = "confirmed_7dav_incidence_prop",
      time_type = "day",
      geo_type = "state",
      date_range = epirange(20200601, 20200801),
      geo_values = "ca,fl"
    ),
    regexp = "`...` must be empty"
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

test_that("epidata* and epidata_meta work as expected", {
  meta_json <- jsonlite::toJSON(
    list(nssp = list(signals = c("sig1", "sig2"), geo_types = c("state", "nation"))),
    auto_unbox = TRUE
  )
  csv_data <- "signal,geo_value,time_value,value\nsig1,ca,2024-01-01,10.5\nsig1,fl,2024-01-01,20.0"
  local_mocked_bindings(
    req_perform = function(req, ...) {
      if (grepl("metadata/", req$url)) {
        to_httr2_response(as.character(meta_json))
      } else {
        to_httr2_response(csv_data)
      }
    },
    .package = "httr2"
  )

  # Test epidata_meta
  res_meta <- epidata_meta(source = "nssp")
  expect_type(res_meta, "list")
  expect_equal(res_meta$nssp$signals, c("sig1", "sig2"))

  # Test epidata_snapshot basic fetch
  res <- epidata_snapshot(source = "nssp", signals = "sig1", geo_type = "state")
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)

  # Test epidata_snapshot filtering
  res_filtered <- epidata_snapshot(source = "nssp", signals = "sig1", geo_type = "state", geo_values = "ca")
  expect_equal(nrow(res_filtered), 1)

  res_time_filtered <- epidata_snapshot(
    source = "nssp", signals = "sig1", geo_type = "state",
    time_values = as.Date("2024-01-01")
  )
  expect_equal(nrow(res_time_filtered), 2)

  # Test EpiRange mapping in version
  res_range <- epidata_archive(
    source = "nssp",
    signals = "sig1",
    geo_type = "state",
    version = epirange("2024-01-01", "2024-01-05")
  )
  expect_s3_class(res_range, "tbl_df")

  # Test version = "*" mapping in epidata
  call_wildcard <- epidata(
    source = "nssp",
    signals = "sig1",
    geo_type = "state",
    version = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
  expect_match(call_wildcard$request$url, "archive/")
  expect_no_match(call_wildcard$request$url, "version_query=")

  # Test as_of = "*" mapping in epidata
  call_as_of_wildcard <- epidata(
    source = "nssp",
    signals = "sig1",
    geo_type = "state",
    as_of = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
  expect_match(call_as_of_wildcard$request$url, "archive/")
  expect_no_match(call_as_of_wildcard$request$url, "snapshot_date=")

  # Test time_values = epirange(...) mapping in epidata_snapshot
  res_time_range <- epidata_snapshot(
    source = "nssp",
    signals = "sig1",
    geo_type = "state",
    time_values = epirange("2024-01-01", "2024-01-01")
  )
  expect_equal(nrow(res_time_range), 2)
})

test_that("epidata_snapshot validations", {
  # Missing required args
  expect_error(
    epidata_snapshot(source = "nssp", signals = "sig1"),
    class = "epidatr__epidata__missing_required_args"
  )

  # Mutually exclusive as_of and version
  expect_error(
    epidata(
      source = "nssp",
      signals = "sig1",
      geo_type = "state",
      as_of = "2024-01-01",
      version = "<2024-01-01"
    ),
    class = "epidatr__epidata__version_and_as_of_exclusive"
  )
})

test_that("epidata_archive local EpiRange filtering for version works", {
  csv_data <- paste0(
    "signal,geo_value,time_value,value,version\n",
    "sig1,ca,2024-01-01,10.0,2024-01-01\n",
    "sig1,ca,2024-01-01,11.0,2024-01-02\n",
    "sig1,ca,2024-01-01,12.0,2024-01-03"
  )
  local_mocked_bindings(
    req_perform = function(req, ...) to_httr2_response(csv_data),
    .package = "httr2"
  )

  # Filter with a range that excludes the first and last dates
  res <- epidata_archive(
    source = "nssp",
    signals = "sig1",
    geo_type = "state",
    version = epirange("2024-01-02", "2024-01-02")
  )
  expect_equal(nrow(res), 1)
  expect_equal(as.character(res$version), "2024-01-02")

  # Filter with a wider range
  res_wide <- epidata_archive(
    source = "nssp",
    signals = "sig1",
    geo_type = "state",
    version = epirange("2024-01-01", "2024-01-02")
  )
  expect_equal(nrow(res_wide), 2)
  expect_true(all(res_wide$version %in% as.Date(c("2024-01-01", "2024-01-02"))))
})
