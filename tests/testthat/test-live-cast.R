# Live integration tests for the cast (v5) API endpoints.
# Runs under `make test-live-cast` (sets EPIDATR_LIVE_TEST=TRUE).
# Set EPIDATR_CAST_BASE_URL to point at a non-default server, e.g.:
#   make test-live-cast cast_url=http://localhost:8005/epidata/v5/

cast_queries <- tibble::tribble(
  ~source, ~signal, ~geo_type,
  "nssp", "pct_ed_visits_influenza", "state",
  # TODO: Ignore county until row limits are in-place server side.
  # "nssp",     "pct_ed_visits_influenza",         "county",
  # TODO: Nhsn is currently without data.
  # "nhsn",     "confirmed_admissions_flu_ew",         "state",
  "pophive", "flu_pct_ed", "state",
  "pophive", "flu_n_ed", "nation",
  "nwss", "covid_avg_conc", "sewershed",
)

test_that("cast versioning args reach the server (snapshot_date, report_time_query)", {
  skip_unless_live()
  fa <- live_cast_fetch_args()
  snap <- epidata_snapshot(
    source = "nssp",
    signals = "pct_ed_visits_influenza",
    geo_type = "state",
    snapshot_date = "2025-01-01",
    fetch_args = fa
  )
  expect_gt(nrow(snap), 0)
  expect_s3_class(snap$report_time, "POSIXct")
  expect_true(all(snap$report_time <= as.POSIXct("2025-01-01", tz = "UTC")))

  arch_lt <- epidata_archive(
    source = "nssp",
    signals = "pct_ed_visits_influenza",
    geo_type = "state",
    report_time = "<2025-06-01",
    fetch_args = fa
  )
  expect_gt(nrow(arch_lt), 0)
  expect_s3_class(arch_lt$report_time, "POSIXct")
  expect_true(all(arch_lt$report_time < as.POSIXct("2025-06-01", tz = "UTC")))

  one_day <- as.Date(max(arch_lt$report_time), tz = "UTC")
  arch_eq <- epidata_archive(
    source = "nssp",
    signals = "pct_ed_visits_influenza",
    geo_type = "state",
    report_time = epirange(one_day, one_day),
    fetch_args = fa
  )
  expect_gt(nrow(arch_eq), 0)
  expect_true(all(as.Date(arch_eq$report_time, tz = "UTC") == one_day))

  # epirange: both bounds go server-side as a "from:to" inclusive range
  arch_range <- epidata_archive(
    source = "nssp",
    signals = "pct_ed_visits_influenza",
    geo_type = "state",
    report_time = epirange("2025-01-01", "2025-06-01"),
    fetch_args = fa
  )
  expect_gt(nrow(arch_range), 0)
  expect_true(all(arch_range$report_time >= as.POSIXct("2025-01-01", tz = "UTC")))
  expect_true(all(arch_range$report_time <= as.POSIXct("2025-06-01", tz = "UTC")))
})

test_that("epidata_meta returns signals + geo_types for each cast source", {
  skip_unless_live()
  fa <- live_cast_fetch_args()
  for (src in unique(cast_queries$source)) {
    source_meta <- epidata_meta(source = src, fetch_args = fa)
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
        fa <- live_cast_fetch_args()
        snapshot <- epidata_snapshot(
          source = row$source,
          signals = row$signal,
          geo_type = row$geo_type,
          fetch_args = fa
        )
        expect_s3_class(snapshot, "tbl_df")
        expect_s3_class(snapshot$reference_time, "Date")
        expect_s3_class(snapshot$report_time, "POSIXct")
        expect_gt(nrow(snapshot), 0)

        archive <- epidata_archive(
          source = row$source,
          signals = row$signal,
          geo_type = row$geo_type,
          fetch_args = fa
        )
        expect_s3_class(archive, "tbl_df")
        expect_s3_class(archive$report_time, "POSIXct")
        expect_gt(nrow(archive), 0)

        # aux: only for sources that expose an aux schema (currently just nwss)
        keys <- tryCatch(
          .aux_key_columns(row$source, fa),
          error = function(e) NULL
        )
        if (length(keys) > 0) {
          small <- head(snapshot, 1) # one row -> auto-inferred filtered_keys keep the pull tiny
          attr(small, "cast_source") <- attr(snapshot, "cast_source") # head() drops the tag
          merged <- epidata_aux(small, fetch_args = fa)
          expect_s3_class(merged, "tbl_df")
          expect_equal(nrow(merged), 1) # base row preserved
          expect_gt(ncol(merged), ncol(small)) # aux columns appended
        }
      }
    )
  })
}
