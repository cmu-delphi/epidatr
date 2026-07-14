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
  csv_data <- "signal,geo_value,reference_time,value\nsig1,ca,2024-01-01,10.5\nsig1,fl,2024-01-01,20.0"
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
  expect_equal(attr(res, "cast_source"), "nssp") # tag lets epidata_aux() recover source

  # Test epidata_snapshot filtering
  res_filtered <- epidata_snapshot(source = "nssp", signals = "sig1", geo_type = "state", geo_values = "ca")
  expect_equal(nrow(res_filtered), 1)

  res_time_filtered <- epidata_snapshot(
    source = "nssp", signals = "sig1", geo_type = "state",
    reference_time = as.Date("2024-01-01")
  )
  expect_equal(nrow(res_time_filtered), 2)

  # Test EpiRange mapping in report_time
  res_range <- epidata_archive(
    source = "nssp",
    signals = "sig1",
    geo_type = "state",
    report_time = epirange("2024-01-01", "2024-01-05")
  )
  expect_s3_class(res_range, "tbl_df")

  # Test report_time = "*" mapping in epidata
  call_wildcard <- epidata(
    source = "nssp",
    signals = "sig1",
    geo_type = "state",
    report_time = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
  expect_match(call_wildcard$request$url, "archive/")
  expect_no_match(call_wildcard$request$url, "version_query=")

  # Test snapshot_date = "*" mapping in epidata routes to archive
  call_as_of_wildcard <- epidata(
    source = "nssp",
    signals = "sig1",
    geo_type = "state",
    snapshot_date = "*",
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
  expect_match(call_as_of_wildcard$request$url, "archive/")
  expect_no_match(call_as_of_wildcard$request$url, "snapshot_date=")

  # Test reference_time = epirange(...) mapping in epidata_snapshot
  res_time_range <- epidata_snapshot(
    source = "nssp",
    signals = "sig1",
    geo_type = "state",
    reference_time = epirange("2024-01-01", "2024-01-01")
  )
  expect_equal(nrow(res_time_range), 2)
})

test_that("epidata validations and deprecations", {
  # Missing required args
  expect_error(
    epidata_snapshot(source = "nssp", signals = "sig1"),
    class = "epidatr__epidata__missing_required_args"
  )

  # Mutually exclusive snapshot_date and report_time
  expect_error(
    epidata(
      source = "nssp",
      signals = "sig1",
      geo_type = "state",
      snapshot_date = "2024-01-01",
      report_time = "<2024-01-01"
    ),
    class = "epidatr__epidata__version_and_as_of_exclusive"
  )

  # Deprecation warnings in epidata
  expect_warning(
    epidata(
      source = "nssp",
      signals = "sig1",
      geo_type = "state",
      issues = "2024-01-01",
      fetch_args = fetch_args_list(dry_run = TRUE)
    ),
    regexp = "Use `report_time` instead"
  )

  expect_warning(
    epidata(
      source = "nssp",
      signals = "sig1",
      geo_type = "state",
      time_values = "2024-01-01",
      fetch_args = fetch_args_list(dry_run = TRUE)
    ),
    regexp = "Use `reference_time` instead"
  )

  expect_warning(
    epidata_snapshot(
      source = "nssp",
      signals = "sig1",
      geo_type = "state",
      as_of = "2024-01-01",
      fetch_args = fetch_args_list(dry_run = TRUE)
    ),
    regexp = "Use `snapshot_date` instead"
  )
})

test_that("epidata_archive local EpiRange filtering for report_time works", {
  csv_data <- paste0(
    "signal,geo_value,reference_time,value,report_time\n",
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
    report_time = epirange("2024-01-02", "2024-01-02")
  )
  expect_equal(nrow(res), 1)
  expect_equal(as.character(res$report_time), "2024-01-02")

  # Filter with a wider range
  res_wide <- epidata_archive(
    source = "nssp",
    signals = "sig1",
    geo_type = "state",
    report_time = epirange("2024-01-01", "2024-01-02")
  )
  expect_equal(nrow(res_wide), 2)
  expect_true(all(res_wide$report_time %in% as.Date(c("2024-01-01", "2024-01-02"))))
})

# ---- epidata_aux ----

test_that("epidata_aux base-pull builds the call, serializes filtered_keys, deprecates aliases", {
  call <- epidata_aux(
    "nwss",
    report_time = "<2024-06-01",
    filtered_keys = list(pcr_target = "SARS-CoV-2", reference_time = as.Date("2024-01-01")),
    columns = c("geo_value", "population_served"),
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
  expect_match(call$request$url, "aux_data/")
  expect_match(call$request$url, "source=nwss")
  expect_match(call$request$url, "report_time_query=%3C2024-06-01") # "<" url-encoded
  expect_match(call$request$url, "population_served") # columns
  expect_match(call$request$url, "pcr_target") # filtered_keys serialized
  expect_match(call$request$url, "2024-01-01") # Date value -> ISO, not day-count

  # one value per key
  expect_error(
    epidata_aux("nwss", filtered_keys = list(geo_value = c("ca", "ny")), fetch_args = fetch_args_list(dry_run = TRUE)),
    class = "epidatr__epidata__multivalue_filtered_key"
  )
  # deprecated aliases map to their replacements
  expect_warning(
    epidata_aux("nwss", issues = "2024-01-01", fetch_args = fetch_args_list(dry_run = TRUE)),
    regexp = "Use `report_time` instead"
  )
  expect_warning(
    epidata_aux("nwss", time_values = "2024-01-01", fetch_args = fetch_args_list(dry_run = TRUE)),
    regexp = "Use `reference_time` instead"
  )
})

test_that("epidata_aux rejects a non-source, non-tagged input", {
  expect_error(epidata_aux(tibble::tibble(a = 1)), class = "epidatr__epidata__untagged_base")
})

# aux_schema (key columns) + aux_data (CSV)
mock_aux_connected <- function(keys, aux_csv) {
  key_json <- paste0('"', keys, '"', collapse = ",")
  function(req, ...) {
    if (grepl("aux_schema", req$url)) {
      to_httr2_response(sprintf('{"nwss":{"key_columns":[%s],"value_columns":[]}}', key_json))
    } else {
      to_httr2_response(aux_csv)
    }
  }
}

test_that("epidata_aux connected merge attaches the latest aux version per key, preserving the base", {
  base <- tibble::tibble(
    geo_value = "162",
    reference_time = as.Date(c("2024-01-01", "2024-01-08")),
    report_time = as.Date("2024-03-10"),
    county_fips = "999", # shared name but NOT a key -> must not be clobbered
    value = c(1, 2)
  )
  attr(base, "cast_source") <- "nwss"
  aux_csv <- paste(
    "report_time,geo_value,reference_time,county_fips,population_served",
    "2024-01-05,162,2024-01-01,001,100",
    "2024-02-15,162,2024-01-01,001,200", # newer version for the 01-01 key -> wins
    "2024-01-05,162,2024-01-08,001,300",
    sep = "\n"
  )
  local_mocked_bindings(
    req_perform = mock_aux_connected(c("report_time", "geo_value", "reference_time"), aux_csv),
    .package = "httr2"
  )
  out <- epidata_aux(base)
  expect_equal(nrow(out), 2) # base rows kept, order preserved
  expect_equal(out$population_served, c("200", "300")) # latest version per key
  expect_equal(out$county_fips, c("999", "999")) # shared non-key column not clobbered
})

test_that("epidata_aux connected path edge cases: empty base, dry_run, no shared keys", {
  tag <- function(df) {
    attr(df, "cast_source") <- "nwss"
    df
  }

  # empty base returns unchanged without any fetch
  empty <- tag(tibble::tibble(
    geo_value = character(), reference_time = as.Date(character()),
    report_time = as.Date(character()), value = numeric()
  ))
  expect_identical(epidata_aux(empty), empty)

  # dry_run surfaces the call, forwards explicit keys, and never fetches the schema
  base <- tag(tibble::tibble(
    geo_value = "ca", reference_time = as.Date("2024-01-01"),
    report_time = as.Date("2024-02-01"), value = 1
  ))
  call <- epidata_aux(base, filtered_keys = list(pcr_target = "x"), fetch_args = fetch_args_list(dry_run = TRUE))
  expect_s3_class(call, "epidata_call")
  expect_match(call$request$url, "pcr_target")

  # no shared keys between base and aux -> abort
  nokeys <- tag(tibble::tibble(report_time = as.Date("2024-02-01"), value = 1))
  local_mocked_bindings(
    req_perform = mock_aux_connected(
      c("report_time", "geo_value", "reference_time"),
      "report_time,geo_value,reference_time,foo\n2024-01-01,ca,2024-01-01,1"
    ),
    .package = "httr2"
  )
  expect_error(epidata_aux(nokeys), class = "epidatr__epidata__no_merge_keys")
})

test_that("disable_missing_meta_warning suppresses the unspecified-fields warning", {
  local_mocked_bindings(
    req_perform = function(req, ...) to_httr2_response("report_time,geo_value,extra_col\n2024-06-01,ca,foo"),
    .package = "httr2"
  )
  call <- create_epidata_call(
    "aux_data/", list(source = "nwss"),
    meta = list(
      create_epidata_field_info("report_time", "date"),
      create_epidata_field_info("geo_value", "text")
    ),
    api_version = "cast", response_format = "csv"
  )
  expect_warning(fetch(call, fetch_args_list()), class = "epidatr__missing_meta_fields")
  expect_no_warning(fetch(call, fetch_args_list(disable_missing_meta_warning = TRUE)))
})
