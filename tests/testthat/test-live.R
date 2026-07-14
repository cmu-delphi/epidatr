# Live API tests. Each test_that calls skip_unless_live(); the suite only runs
# under `make test-live` (which sets NOT_CRAN=true). DELPHI_EPIDATA_KEY must
# be in the environment for the pvt_* endpoints.
#
# Each endpoint that takes a date-like parameter has two tests: one with a
# concrete range, one with "*" to confirm the wildcard path works end-to-end.

auth <- function() Sys.getenv("DELPHI_EPIDATA_KEY")

# ---- pvt_cdc ----
test_that("pvt_cdc", {
  skip_unless_live()
  result <- pvt_cdc(auth = auth(), locations = "fl,ca", epiweeks = epirange(201501, 201601))
  expect_gt(nrow(result), 0)
})
test_that("pvt_cdc wildcard", {
  skip_unless_live()
  result <- pvt_cdc(auth = auth(), locations = "fl,ca", epiweeks = "*")
  expect_gt(nrow(result), 0)
})

# ---- pub_covid_hosp_facility_lookup ----
test_that("pub_covid_hosp_facility_lookup", {
  skip_unless_live()
  result <- pub_covid_hosp_facility_lookup(state = "fl")
  expect_gt(nrow(result), 0)
})

# ---- pub_covid_hosp_facility ----
test_that("pub_covid_hosp_facility", {
  skip_unless_live()
  result <- pub_covid_hosp_facility(hospital_pks = "100075", collection_weeks = epirange(20200101, 20200501))
  expect_gt(nrow(result), 0)
})
test_that("pub_covid_hosp_facility wildcard", {
  skip_unless_live()
  result <- pub_covid_hosp_facility(hospital_pks = "100075", collection_weeks = "*")
  expect_gt(nrow(result), 0)
})

# ---- pub_covid_hosp_state_timeseries ----
test_that("pub_covid_hosp_state_timeseries", {
  skip_unless_live()
  result <- pub_covid_hosp_state_timeseries(states = "fl", dates = epirange(20200101, 20200501))
  expect_gt(nrow(result), 0)
})
test_that("pub_covid_hosp_state_timeseries wildcard", {
  skip_unless_live()
  result <- pub_covid_hosp_state_timeseries(states = "fl", dates = "*")
  expect_gt(nrow(result), 0)
})

# ---- pub_covidcast_meta ----
test_that("pub_covidcast_meta", {
  skip_unless_live()
  result <- pub_covidcast_meta()
  expect_gt(nrow(result), 0)
})

# ---- pub_covidcast ----
test_that("pub_covidcast", {
  skip_unless_live()
  result <- pub_covidcast(
    source = "jhu-csse", signals = "confirmed_7dav_incidence_prop",
    geo_type = "state", time_type = "day",
    geo_values = c("ca", "fl"), time_values = epirange(20200601, 20200801)
  )
  expect_gt(nrow(result), 0)
})
test_that("pub_covidcast wildcard", {
  skip_unless_live()
  result <- pub_covidcast(
    source = "jhu-csse", signals = "confirmed_7dav_incidence_prop",
    geo_type = "state", time_type = "day",
    geo_values = "ca,fl", time_values = "*"
  )
  expect_gt(nrow(result), 0)
})

# ---- pub_delphi ----
test_that("pub_delphi", {
  skip_unless_live()
  result <- pub_delphi(system = "ec", epiweek = 201501)
  expect_gt(length(result), 0)
})

# ---- pub_dengue_nowcast ----
test_that("pub_dengue_nowcast", {
  skip_unless_live()
  result <- pub_dengue_nowcast(locations = "pr", epiweeks = epirange(201401, 202301))
  expect_gt(nrow(result), 0)
})
test_that("pub_dengue_nowcast wildcard", {
  skip_unless_live()
  result <- pub_dengue_nowcast(locations = "ca", epiweeks = "*")
  expect_gt(nrow(result), 0)
})

# ---- pvt_dengue_sensors ----
test_that("pvt_dengue_sensors", {
  skip_unless_live()
  result <- pvt_dengue_sensors(auth = auth(), names = "ght", locations = "ag", epiweeks = epirange(201501, 202001))
  expect_gt(nrow(result), 0)
})
test_that("pvt_dengue_sensors wildcard", {
  skip_unless_live()
  result <- pvt_dengue_sensors(auth = auth(), names = "ght", locations = "ag", epiweeks = "*")
  expect_gt(nrow(result), 0)
})

# ---- pub_ecdc_ili ----
test_that("pub_ecdc_ili", {
  skip_unless_live()
  result <- pub_ecdc_ili(regions = "austria", epiweeks = epirange(201901, 202001))
  expect_gt(nrow(result), 0)
})
test_that("pub_ecdc_ili wildcard", {
  skip_unless_live()
  result <- pub_ecdc_ili(regions = "austria", epiweeks = "*")
  expect_gt(nrow(result), 0)
})

# ---- pub_flusurv ----
test_that("pub_flusurv", {
  skip_unless_live()
  result <- pub_flusurv(locations = "ca", epiweeks = epirange(201701, 201801))
  expect_gt(nrow(result), 0)
})
test_that("pub_flusurv wildcard", {
  skip_unless_live()
  result <- pub_flusurv(locations = "CA", epiweeks = "*")
  expect_gt(nrow(result), 0)
})

# ---- pub_fluview_clinical ----
test_that("pub_fluview_clinical", {
  skip_unless_live()
  result <- pub_fluview_clinical(regions = "nat", epiweeks = epirange(201601, 201701))
  expect_gt(nrow(result), 0)
})
test_that("pub_fluview_clinical wildcard", {
  skip_unless_live()
  result <- pub_fluview_clinical(regions = "nat", epiweeks = "*")
  expect_gt(nrow(result), 0)
})

# ---- pub_fluview_meta ----
test_that("pub_fluview_meta", {
  skip_unless_live()
  result <- pub_fluview_meta()
  expect_gt(nrow(result), 0)
})

# ---- pub_fluview ----
test_that("pub_fluview", {
  skip_unless_live()
  result <- pub_fluview(regions = "nat", epiweeks = epirange(201201, 202005))
  expect_gt(nrow(result), 0)
})
test_that("pub_fluview wildcard", {
  skip_unless_live()
  result <- pub_fluview(regions = "nat", epiweeks = "*")
  expect_gt(nrow(result), 0)
})

# ---- pub_gft ----
test_that("pub_gft", {
  skip_unless_live()
  result <- pub_gft(locations = "hhs1", epiweeks = epirange(201201, 202001))
  expect_gt(nrow(result), 0)
})
test_that("pub_gft wildcard", {
  skip_unless_live()
  result <- pub_gft(locations = "hhs1", epiweeks = "*")
  expect_gt(nrow(result), 0)
})

# ---- pvt_ght ----
test_that("pvt_ght", {
  skip_unless_live()
  result <- pvt_ght(auth = auth(), locations = "ma", epiweeks = epirange(199301, 202304), query = "how to get over the flu")
  expect_gt(nrow(result), 0)
})
test_that("pvt_ght wildcard", {
  skip_unless_live()
  result <- pvt_ght(auth = auth(), locations = "ca", epiweeks = "*", query = "how to get over the flu")
  expect_gt(nrow(result), 0)
})

# ---- pub_kcdc_ili ----
test_that("pub_kcdc_ili", {
  skip_unless_live()
  result <- pub_kcdc_ili(regions = "ROK", epiweeks = 200436)
  expect_gt(nrow(result), 0)
})
test_that("pub_kcdc_ili wildcard", {
  skip_unless_live()
  result <- pub_kcdc_ili(regions = "ROK", epiweeks = "*")
  expect_gt(nrow(result), 0)
})

# ---- pvt_meta_norostat ----
test_that("pvt_meta_norostat", {
  skip_unless_live()
  result <- pvt_meta_norostat(auth = auth())
  expect_gt(length(result), 0)
})

# ---- pub_meta ----
test_that("pub_meta", {
  skip_unless_live()
  result <- pub_meta()
  expect_gt(length(result), 0)
})

# ---- pub_nidss_dengue ----
test_that("pub_nidss_dengue", {
  skip_unless_live()
  result <- pub_nidss_dengue(locations = "taipei", epiweeks = epirange(201201, 201301))
  expect_gt(nrow(result), 0)
})
test_that("pub_nidss_dengue wildcard", {
  skip_unless_live()
  result <- pub_nidss_dengue(locations = "taipei", epiweeks = "*")
  expect_gt(nrow(result), 0)
})

# ---- pub_nidss_flu ----
test_that("pub_nidss_flu", {
  skip_unless_live()
  result <- pub_nidss_flu(regions = "taipei", epiweeks = epirange(201501, 201601))
  expect_gt(nrow(result), 0)
})
test_that("pub_nidss_flu wildcard", {
  skip_unless_live()
  result <- pub_nidss_flu(regions = "taipei", epiweeks = "*")
  expect_gt(nrow(result), 0)
})

# ---- pvt_norostat ----
test_that("pvt_norostat", {
  skip_unless_live()
  result <- pvt_norostat(auth = auth(), locations = "Minnesota, Ohio, Oregon, Tennessee, and Wisconsin", epiweeks = 201233)
  expect_gt(nrow(result), 0)
})
test_that("pvt_norostat wildcard", {
  skip_unless_live()
  result <- pvt_norostat(auth = auth(), locations = "Minnesota, Ohio, Oregon, Tennessee, and Wisconsin", epiweeks = "*")
  expect_gt(nrow(result), 0)
})

# ---- pub_nowcast ----
test_that("pub_nowcast", {
  skip_unless_live()
  result <- pub_nowcast(locations = "ca", epiweeks = epirange(201201, 201301))
  expect_gt(nrow(result), 0)
})
test_that("pub_nowcast wildcard", {
  skip_unless_live()
  result <- pub_nowcast(locations = "ca", epiweeks = "*")
  expect_gt(nrow(result), 0)
})

# ---- pub_paho_dengue ----
test_that("pub_paho_dengue", {
  skip_unless_live()
  result <- pub_paho_dengue(regions = "ca", epiweeks = epirange(201401, 201501))
  expect_gt(nrow(result), 0)
})
test_that("pub_paho_dengue wildcard", {
  skip_unless_live()
  result <- pub_paho_dengue(regions = "ca", epiweeks = "*")
  expect_gt(nrow(result), 0)
})

# ---- pvt_quidel ----
test_that("pvt_quidel", {
  skip_unless_live()
  result <- pvt_quidel(auth = auth(), epiweeks = epirange(201201, 202001), locations = "hhs1")
  expect_gt(nrow(result), 0)
})
test_that("pvt_quidel wildcard", {
  skip_unless_live()
  result <- pvt_quidel(auth = auth(), epiweeks = "*", locations = "hhs1")
  expect_gt(nrow(result), 0)
})

# ---- pvt_sensors ----
test_that("pvt_sensors", {
  skip_unless_live()
  result <- pvt_sensors(auth = auth(), names = "sar3", locations = "nat", epiweeks = epirange(201501, 202001))
  expect_gt(nrow(result), 0)
})
test_that("pvt_sensors wildcard", {
  skip_unless_live()
  result <- pvt_sensors(auth = auth(), names = "sar3", locations = "nat", epiweeks = "*")
  expect_gt(nrow(result), 0)
})

# ---- pvt_twitter ----
test_that("pvt_twitter week", {
  skip_unless_live()
  result <- pvt_twitter(auth = auth(), locations = "CA", time_type = "week", time_values = epirange(201501, 202001))
  expect_gt(nrow(result), 0)
})
test_that("pvt_twitter week wildcard", {
  skip_unless_live()
  result <- pvt_twitter(auth = auth(), locations = "CA", time_type = "week", time_values = "*")
  expect_gt(nrow(result), 0)
})
test_that("pvt_twitter day", {
  skip_unless_live()
  result <- pvt_twitter(auth = auth(), locations = "CA", time_type = "day", time_values = epirange(20150101, 20200101))
  expect_gt(nrow(result), 0)
})
test_that("pvt_twitter day wildcard", {
  skip_unless_live()
  result <- pvt_twitter(auth = auth(), locations = "CA", time_type = "day", time_values = "*")
  expect_gt(nrow(result), 0)
})

# ---- pub_wiki ----
test_that("pub_wiki week", {
  skip_unless_live()
  result <- pub_wiki(articles = "avian_influenza", time_type = "week", time_values = epirange(201501, 201601))
  expect_gt(nrow(result), 0)
})
test_that("pub_wiki week wildcard", {
  skip_unless_live()
  result <- pub_wiki(articles = "avian_influenza", time_type = "week", time_values = "*")
  expect_gt(nrow(result), 0)
})
test_that("pub_wiki day", {
  skip_unless_live()
  result <- pub_wiki(articles = "avian_influenza", time_type = "day", time_values = epirange(20150101, 20200101))
  expect_gt(nrow(result), 0)
})
test_that("pub_wiki day wildcard", {
  skip_unless_live()
  result <- pub_wiki(articles = "avian_influenza", time_type = "day", time_values = "*")
  expect_gt(nrow(result), 0)
})

# ---- epidata_* (cast API) ----
# TODO: Happy to add more, this is a starting point.
cast_queries <- tibble::tribble(
  ~source,    ~signal,    ~geo_type,
  "nssp",     "pct_ed_visits_influenza",         "state",
  "nssp",     "pct_ed_visits_influenza",         "hhs",
  # TODO: Ignore county until row limits are in-place server side.
  # "nssp",     "pct_ed_visits_influenza",         "county",
  # TODO: Nhsn is currently without data.
  # "nhsn",     "confirmed_admissions_flu_ew",         "state",
  # "nhsn",     "confirmed_admissions_flu_ew",         "hhs",
  # "nhsn",     "confirmed_admissions_flu_ew",         "national",
  "pophive",  "flu_pct_ed",         "state",
  "pophive",  "flu_pct_ed",         "hhs",
  "pophive",  "flu_n_ed",         "state",
  "pophive",  "flu_n_ed",         "hhs",
  "pophive",  "flu_n_ed",         "nation",
  "nwss",     "covid_avg_conc",         "sewershed",
)

test_that("epidata_meta returns signals + geo_types for each cast source", {
  skip_unless_live()
  for (src in unique(cast_queries$source)) {
    source_meta <- epidata_meta(source = src)[[src]]
    expect_type(source_meta, "list")
    expect_true(length(source_meta$signals) > 0)
    expect_true(length(source_meta$geo_types) > 0)
  }
})

for (i in seq_len(nrow(cast_queries))) {
  local({
    row <- cast_queries[i, ]
    test_that(sprintf("epidata_snapshot + epidata_archive for source=%s signal=%s geo_type=%s",
                      row$source, row$signal, row$geo_type), {
      skip_unless_live()
      snapshot <- epidata_snapshot(source = row$source, signals = row$signal, geo_type = row$geo_type)
      expect_s3_class(snapshot, "tbl_df")
      expect_s3_class(snapshot$reference_time, "Date")
      expect_s3_class(snapshot$report_time, "Date")
      expect_gt(nrow(snapshot), 0)

      archive <- epidata_archive(source = row$source, signals = row$signal, geo_type = row$geo_type)
      expect_s3_class(archive, "tbl_df")
      expect_s3_class(archive$report_time, "Date")
      expect_gt(nrow(archive), 0)

      # aux: only for sources that expose an aux schema (currently just nwss)
      keys <- tryCatch(.aux_key_columns(row$source, fetch_args_list()), error = function(e) NULL)
      if (length(keys) > 0) {
        small <- head(snapshot, 1) # one row -> auto-inferred filtered_keys keep the pull tiny
        attr(small, "cast_source") <- attr(snapshot, "cast_source") # head() drops the tag
        merged <- epidata_aux(small)
        expect_s3_class(merged, "tbl_df")
        expect_equal(nrow(merged), 1) # base row preserved
        expect_gt(ncol(merged), ncol(small)) # aux columns appended
      }
    })
  })
}
