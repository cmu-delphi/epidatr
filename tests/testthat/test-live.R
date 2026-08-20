# Live API tests. Each test calls skip_unless_live(); the suite only runs
# under `make test-live` (which sets EPIDATR_LIVE_TEST=TRUE).
# DELPHI_EPIDATA_KEY must be in the environment for the pvt_* endpoints.
#
# The classic endpoints are driven by the endpoint_calls() table in
# helper-endpoints.R (shared with the URL snapshot tests). Each row asserts,
# via expect_live_call_parses(): non-empty results, a warning-free fetch, and
# column classes matching the endpoint's field metadata.

for (endpoint_row in endpoint_calls(auth = Sys.getenv("DELPHI_EPIDATA_KEY"))) {
  local({
    row <- endpoint_row
    if (!row$live) {
      return()
    }
    test_that(paste0("live: ", row$name), {
      skip_unless_live()
      expect_live_call_parses(row$call)
    })
  })
}

# ---- epidata_* (cast API) ----
# TODO: Happy to add more, this is a starting point.
cast_queries <- tibble::tribble(
  ~source   , ~signal                   , ~geo_type   ,
  "nssp"    , "pct_ed_visits_influenza" , "state"     ,
  "nssp"    , "pct_ed_visits_influenza" , "hhs"       ,
  # TODO: Ignore county until row limits are in-place server side.
  # "nssp",     "pct_ed_visits_influenza",         "county",
  # TODO: Nhsn is currently without data.
  # "nhsn",     "confirmed_admissions_flu_ew",         "state",
  # "nhsn",     "confirmed_admissions_flu_ew",         "hhs",
  # "nhsn",     "confirmed_admissions_flu_ew",         "national",
  "pophive" , "flu_pct_ed"              , "state"     ,
  "pophive" , "flu_pct_ed"              , "hhs"       ,
  "pophive" , "flu_n_ed"                , "state"     ,
  "pophive" , "flu_n_ed"                , "hhs"       ,
  "pophive" , "flu_n_ed"                , "nation"    ,
  "nwss"    , "covid_avg_conc"          , "sewershed" ,
)

test_that("cast versioning args reach the server (snapshot_date, report_time_query)", {
  skip_unless_live()
  snap <- epidata_snapshot(
    source = "nssp",
    signals = "pct_ed_visits_influenza",
    geo_type = "state",
    geo_values = "pa",
    snapshot_date = "2025-01-01"
  )
  expect_gt(nrow(snap), 0)
  expect_true(all(snap$report_time <= as.Date("2025-01-01")))

  arch_lt <- epidata_archive(
    source = "nssp",
    signals = "pct_ed_visits_influenza",
    geo_type = "state",
    geo_values = "pa",
    report_time = "<2025-06-01"
  )
  expect_gt(nrow(arch_lt), 0)
  expect_true(all(arch_lt$report_time < as.Date("2025-06-01")))

  one_day <- max(arch_lt$report_time)
  arch_eq <- epidata_archive(
    source = "nssp",
    signals = "pct_ed_visits_influenza",
    geo_type = "state",
    geo_values = "pa",
    report_time = one_day
  )
  expect_gt(nrow(arch_eq), 0)
  expect_true(all(arch_eq$report_time == one_day))

  # epirange: upper bound goes server-side, lower bound is filtered locally
  arch_range <- epidata_archive(
    source = "nssp",
    signals = "pct_ed_visits_influenza",
    geo_type = "state",
    geo_values = "pa",
    report_time = epirange("2025-01-01", "2025-06-01")
  )
  expect_gt(nrow(arch_range), 0)
  expect_true(all(arch_range$report_time >= as.Date("2025-01-01")))
  expect_true(all(arch_range$report_time <= as.Date("2025-06-01")))
})

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
    test_that(
      sprintf(
        "epidata_snapshot + epidata_archive for source=%s signal=%s geo_type=%s",
        row$source,
        row$signal,
        row$geo_type
      ),
      {
        skip_unless_live()
        snapshot <- epidata_snapshot(
          source = row$source,
          signals = row$signal,
          geo_type = row$geo_type
        )
        expect_s3_class(snapshot, "tbl_df")
        expect_s3_class(snapshot$reference_time, "Date")
        expect_s3_class(snapshot$report_time, "Date")
        expect_gt(nrow(snapshot), 0)

        archive <- epidata_archive(
          source = row$source,
          signals = row$signal,
          geo_type = row$geo_type
        )
        expect_s3_class(archive, "tbl_df")
        expect_s3_class(archive$report_time, "Date")
        expect_gt(nrow(archive), 0)

        # aux: only for sources that expose an aux schema (currently just nwss)
        keys <- tryCatch(
          .aux_key_columns(row$source, fetch_args_list()),
          error = function(e) NULL
        )
        if (length(keys) > 0) {
          small <- head(snapshot, 1) # one row -> auto-inferred filtered_keys keep the pull tiny
          attr(small, "cast_source") <- attr(snapshot, "cast_source") # head() drops the tag
          merged <- epidata_aux(small)
          expect_s3_class(merged, "tbl_df")
          expect_equal(nrow(merged), 1) # base row preserved
          expect_gt(ncol(merged), ncol(small)) # aux columns appended
        }
      }
    )
  })
}
