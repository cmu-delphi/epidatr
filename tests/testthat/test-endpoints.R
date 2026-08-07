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
  expect_equal(attr(res, "cast_kind"), "snapshot") # drives uniform vs as-of aux merge

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
  expect_equal(attr(res_range, "cast_kind"), "archive") # per-row as-of aux merge

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

test_that("snapshot/archive/epidata send ... key filters server-side as extra_keys", {
  snap <- epidata_snapshot(
    source = "nwss", signals = "sig1", geo_type = "county",
    pcr_target = c("sars-cov-2", "influenza"),
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
  expect_match(snap$request$url, "extra_keys=")
  expect_match(snap$request$url, "pcr_target%3Asars-cov-2") # "pcr_target:sars-cov-2"
  expect_match(snap$request$url, "pcr_target%3Ainfluenza")

  arch <- epidata_archive(
    source = "nwss", signals = "sig1", geo_type = "county",
    sample_index = "92012",
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
  expect_match(arch$request$url, "extra_keys=")
  expect_match(arch$request$url, "sample_index%3A92012")

  # dispatcher forwards ... to the routed endpoint
  disp <- epidata(
    source = "nwss", signals = "sig1", geo_type = "county",
    pcr_target = "sars-cov-2",
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
  expect_match(disp$request$url, "extra_keys=")
  expect_match(disp$request$url, "pcr_target%3Asars-cov-2")
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

test_that("epidata_aux base-pull builds the call, serializes key filters via ..., deprecates aliases", {
  call <- epidata_aux(
    "nwss",
    report_time = "<2024-06-01",
    pcr_target = "SARS-CoV-2",
    ref_date = as.Date("2024-01-01"), # a typed key value -> ISO, not day-count
    columns = c("geo_value", "population_served"),
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
  expect_match(call$request$url, "aux_data/")
  expect_match(call$request$url, "source=nwss")
  expect_match(call$request$url, "report_time_query=%3C2024-06-01") # "<" url-encoded
  expect_match(call$request$url, "population_served") # columns
  expect_match(call$request$url, "pcr_target") # ... key filter serialized
  expect_match(call$request$url, "2024-01-01") # Date value -> ISO, not day-count

  # multiple values per key are allowed and serialized as repeated key:value terms
  multi <- epidata_aux("nwss", geo_value = c("ca", "ny"), fetch_args = fetch_args_list(dry_run = TRUE))
  expect_match(multi$request$url, "geo_value%3Aca") # "geo_value:ca" url-encoded
  expect_match(multi$request$url, "geo_value%3Any") # "geo_value:ny"

  # unnamed ... filters are rejected
  expect_error(
    epidata_aux("nwss", "ca", fetch_args = fetch_args_list(dry_run = TRUE)),
    class = "epidatr__epidata__unnamed_filter"
  )
  # more than the cap warns about URL length (still serializes)
  expect_warning(
    epidata_aux("nwss", geo_value = as.character(1:11), fetch_args = fetch_args_list(dry_run = TRUE)),
    class = "epidatr__epidata__many_filtered_values"
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

# aux_schema (key columns) + aux_data (CSV)
mock_aux_connected <- function(keys, aux_csv) {
  key_json <- paste0('"', keys, '"', collapse = ",")
  function(req, ...) {
    schema_json <- sprintf('{"nwss":{"key_columns":[%s],"value_columns":[]}}', key_json)
    if (grepl("aux_schema", req$url)) {
      to_httr2_response(schema_json) # nolint: object_usage_linter.
    } else {
      to_httr2_response(aux_csv) # nolint: object_usage_linter.
    }
  }
}

# Shared aux table for the merge test: three keys, some with two revisions whose
# dates straddle the base report times (so as-of vs uniform differ), plus a
# non-key `county_fips` to check shared columns aren't clobbered.
aux_versions_keys <- c("report_time", "geo_value", "reference_time", "sample_index")
aux_versions_csv <- paste(
  "report_time,geo_value,reference_time,sample_index,county_fips,population_served,label",
  "2024-02-01,ca,2024-01-01,A,001,100,p1",
  "2024-05-01,ca,2024-01-01,A,001,200,p2", # newer ca/01-01/A revision
  "2024-02-01,ca,2024-01-08,A,001,300,p3",
  "2024-01-01,ny,2024-01-01,B,001,350,p0",
  "2024-04-01,ny,2024-01-01,B,001,400,p4", # newer ny revision (after 03-01)
  sep = "\n"
)

test_that("epidata_aux merge is version-aware: archive per-row vs snapshot uniform", {
  local_mocked_bindings(
    req_perform = mock_aux_connected(aux_versions_keys, aux_versions_csv),
    .package = "httr2"
  )

  # Archive: each row as-of its own report_time, never a newer version; too-early
  # or unknown keys -> NA; a shared non-key column (county_fips) is preserved.
  archive <- tibble::tibble(
    geo_value = c("ca", "ca", "ca", "ny", "tx"),
    reference_time = as.Date(c("2024-01-01", "2024-01-01", "2024-01-08", "2024-01-01", "2024-01-01")),
    sample_index = c("A", "A", "A", "B", "C"),
    report_time = as.Date(c("2024-01-15", "2024-03-01", "2024-06-01", "2024-03-01", "2024-03-01")),
    county_fips = "999",
    value = 1:5
  )
  attr(archive, "cast_source") <- "nwss"
  attr(archive, "cast_kind") <- "archive"
  out <- epidata_aux(archive)
  # 01-15 predates aux -> NA; 03-01 -> 02-01 (not newer 05-01); 06-01 -> 300;
  # ny 03-01 -> 01-01 (not newer 04-01); tx key absent -> NA
  expect_equal(out$population_served, c(NA, "100", "300", "350", NA))
  expect_equal(out$label, c(NA, "p1", "p3", "p0", NA)) # a second value column
  expect_equal(out$county_fips, rep("999", 5)) # shared non-key not clobbered
  expect_equal(out$value, 1:5) # base rows and order preserved

  # Snapshot: single-version view -> every row as-of the cutoff (max = 06-01),
  # a different result than the archive on the same aux.
  snapshot <- tibble::tibble(
    geo_value = c("ca", "ca", "ny"),
    reference_time = as.Date(c("2024-01-01", "2024-01-08", "2024-01-01")),
    sample_index = c("A", "A", "B"),
    report_time = as.Date(c("2024-03-01", "2024-06-01", "2024-03-01")),
    value = 1:3
  )
  attr(snapshot, "cast_source") <- "nwss"
  attr(snapshot, "cast_kind") <- "snapshot"
  out <- epidata_aux(snapshot)
  # ca/01-01/A -> 200, ca/01-08/A -> 300, ny/01-01/B -> 400
  expect_equal(out$population_served, c("200", "300", "400"))
})

test_that("epidata_aux infers multi-value key filters from the base (<= cap)", {
  seen <- new.env()
  recorder <- function(req, ...) {
    if (grepl("aux_schema", req$url)) {
      to_httr2_response('{"nwss":{"key_columns":["report_time","geo_value"],"value_columns":[]}}')
    } else {
      seen$url <- req$url # capture the forwarded aux_data request
      to_httr2_response("report_time,geo_value,population_served\n2024-02-01,ca,100\n2024-02-01,ny,200")
    }
  }
  local_mocked_bindings(req_perform = recorder, .package = "httr2")

  base <- tibble::tibble(
    geo_value = c("ca", "ny"), # two distinct values, under the cap -> both pinned
    report_time = as.Date(c("2024-03-01", "2024-03-01")), value = 1:2
  )
  attr(base, "cast_source") <- "nwss"
  attr(base, "cast_kind") <- "snapshot"
  epidata_aux(base)
  expect_match(seen$url, "geo_value%3Aca") # "geo_value:ca"
  expect_match(seen$url, "geo_value%3Any") # "geo_value:ny"
})

test_that("epidata_aux connected path: validation, empty base, dry_run cap/forwarding, key errors", {
  tag <- function(df) {
    attr(df, "cast_source") <- "nwss"
    df
  }

  # untagged data frame -> reject
  expect_error(epidata_aux(tibble::tibble(a = 1)), class = "epidatr__epidata__untagged_base")

  # empty base returns unchanged without any fetch
  empty <- tag(tibble::tibble(
    geo_value = character(), reference_time = as.Date(character()),
    report_time = as.Date(character()), value = numeric()
  ))
  expect_identical(epidata_aux(empty), empty)

  # dry_run surfaces the call: forwards explicit keys AND caps the pull at the
  # base's newest report_time (max + 1 day); no schema fetch (would fail unmocked)
  base <- tag(tibble::tibble(
    geo_value = "ca", reference_time = as.Date("2024-01-01"),
    report_time = as.Date(c("2024-01-10", "2024-05-20")), value = c(1, 2)
  ))
  call <- epidata_aux(base, pcr_target = "x", fetch_args = fetch_args_list(dry_run = TRUE))
  expect_s3_class(call, "epidata_call")
  expect_match(call$request$url, "pcr_target") # explicit ... keys forwarded
  expect_match(call$request$url, "report_time_query=%3C2024-05-21") # capped, "<" -> %3C

  # remaining cases share one mocked schema (keys: report_time/geo_value/reference_time)
  local_mocked_bindings(
    req_perform = mock_aux_connected(
      c("report_time", "geo_value", "reference_time"),
      "report_time,geo_value,reference_time,foo\n2024-01-01,ca,2024-01-01,1"
    ),
    .package = "httr2"
  )
  # no key shared between base and aux -> abort
  expect_error(
    epidata_aux(tag(tibble::tibble(report_time = as.Date("2024-02-01"), value = 1))),
    class = "epidatr__epidata__no_merge_keys"
  )
  # `columns` excluding a key column -> abort before the aux data is fetched
  expect_error(
    epidata_aux(
      tag(tibble::tibble(
        geo_value = "ca", reference_time = as.Date("2024-01-01"),
        report_time = as.Date("2024-02-01"), value = 1
      )),
      columns = "population_served" # drops the reference_time key
    ),
    class = "epidatr__epidata__missing_aux_keys"
  )
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
