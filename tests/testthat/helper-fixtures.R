# Recorded-response fixtures: one per API response shape, not per endpoint.
# The raw bytes live in tests/testthat/fixtures/ and are refreshed by
# data-raw/update_fixtures.R (make update-fixtures), which runs the same specs
# against the real API. test-fixtures.R replays each through the full parsing
# pipeline and snapshots the typed result, pinning the bytes -> tibble contract.
#
# Queries here should be small, version-pinned where the API allows (snapshot_date,
# report_time), and guaranteed non-empty.
fixture_specs <- function() {
  spec <- function(file, content_type, call) {
    list(file = file, content_type = content_type, call = call)
  }
  list(
    # classic JSON wrapper -> tibble (the shape used by most pub_*/pvt_* endpoints)
    spec("classic-covidcast.json", "application/json", function(fa) {
      pub_covidcast(
        source = "jhu-csse",
        signals = "confirmed_7dav_incidence_prop",
        geo_type = "state",
        time_type = "day",
        geo_values = "ca",
        time_values = epirange(20200601, 20200607),
        fetch_args = fa
      )
    }),
    # classic JSON with epiweek-typed columns and a Date issue column
    spec("classic-fluview.json", "application/json", function(fa) {
      pub_fluview(
        regions = "nat",
        epiweeks = epirange(202001, 202004),
        fetch_args = fa
      )
    }),
    # classic JSON, non-tabular list payload
    spec("classic-delphi.json", "application/json", function(fa) {
      pub_delphi(system = "ec", epiweek = 201501, fetch_args = fa)
    }),
    # cast CSV, snapshot endpoint (pinned snapshot_date keeps the pull stable)
    spec("cast-snapshot.csv", "text/csv", function(fa) {
      epidata_snapshot(
        source = "nssp",
        signals = "pct_ed_visits_influenza",
        geo_type = "nation",
        snapshot_date = "2025-01-01",
        fetch_args = fa
      )
    }),
    # cast CSV, archive endpoint (exact report_time -> one version slice)
    spec("cast-archive.csv", "text/csv", function(fa) {
      epidata_archive(
        source = "nssp",
        signals = "pct_ed_visits_influenza",
        geo_type = "nation",
        report_time = "2024-12-27",
        fetch_args = fa
      )
    }),
    # cast metadata JSON
    spec("cast-meta.json", "application/json", function(fa) {
      epidata_meta(source = "nssp", fetch_args = fa)
    }),
    # cast aux_data CSV (base-pull mode); nwss aux only has recent report_times
    # and sewershed geo_values are numeric ids
    spec("aux-data.csv", "text/csv", function(fa) {
      epidata_aux(
        "nwss",
        report_time = "<2026-08-01",
        geo_value = "10",
        sample_index = "5639533",
        fetch_args = fa
      )
    })
  )
}

# Replay a fixture's bytes through `spec$call` with the network mocked out.
replay_fixture <- function(spec, fetch_args = fetch_args_list()) {
  body <- readBin(
    testthat::test_path("fixtures", spec$file),
    what = "raw",
    n = file.size(testthat::test_path("fixtures", spec$file))
  )
  result <- NULL
  with_mocked_response(
    create_mock_response(
      body,
      headers = list("content-type" = spec$content_type)
    ),
    result <- spec$call(fetch_args)
  )
  result
}
