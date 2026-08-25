# Single source of truth for per-endpoint example calls. Each row's `call` is
# a thunk taking `fetch_args`, so the same table drives:
#   - request-URL snapshot tests (test-endpoint-urls.R, dry_run, no network)
#   - live fetch + parse checks (test-live.R, real requests)
# Rows with `live = FALSE` are URL-snapshot only (the cast API already has
# dedicated live coverage in test-live.R's cast_queries loop).
#
# To cover a new endpoint, add a row here: it gets URL and live coverage.
endpoint_calls <- function(auth = "test-auth-key") {
  row <- function(name, call, live = TRUE) {
    list(name = name, call = call, live = live)
  }
  list(
    row("pvt_cdc", function(fa) {
      pvt_cdc(
        auth = auth,
        locations = "fl,ca",
        epiweeks = epirange(201501, 201601),
        fetch_args = fa
      )
    }),
    row("pvt_cdc wildcard", function(fa) {
      pvt_cdc(auth = auth, locations = "fl,ca", epiweeks = "*", fetch_args = fa)
    }),
    row("pub_covid_hosp_facility_lookup", function(fa) {
      pub_covid_hosp_facility_lookup(state = "fl", fetch_args = fa)
    }),
    row("pub_covid_hosp_facility", function(fa) {
      pub_covid_hosp_facility(
        hospital_pks = "100075",
        collection_weeks = epirange(20200101, 20200501),
        fetch_args = fa
      )
    }),
    row("pub_covid_hosp_facility wildcard", function(fa) {
      pub_covid_hosp_facility(
        hospital_pks = "100075",
        collection_weeks = "*",
        fetch_args = fa
      )
    }),
    row("pub_covid_hosp_state_timeseries", function(fa) {
      pub_covid_hosp_state_timeseries(
        states = "fl",
        dates = epirange(20200101, 20200501),
        fetch_args = fa
      )
    }),
    row("pub_covid_hosp_state_timeseries wildcard", function(fa) {
      pub_covid_hosp_state_timeseries(
        states = "fl",
        dates = "*",
        fetch_args = fa
      )
    }),
    row("pub_covidcast_meta", function(fa) {
      pub_covidcast_meta(fetch_args = fa)
    }),
    row("pub_covidcast", function(fa) {
      pub_covidcast(
        source = "jhu-csse",
        signals = "confirmed_7dav_incidence_prop",
        geo_type = "state",
        time_type = "day",
        geo_values = c("ca", "fl"),
        time_values = epirange(20200601, 20200801),
        fetch_args = fa
      )
    }),
    row("pub_covidcast wildcard", function(fa) {
      pub_covidcast(
        source = "jhu-csse",
        signals = "confirmed_7dav_incidence_prop",
        geo_type = "state",
        time_type = "day",
        geo_values = "ca,fl",
        time_values = "*",
        fetch_args = fa
      )
    }),
    row("pub_delphi", function(fa) {
      pub_delphi(system = "ec", epiweek = 201501, fetch_args = fa)
    }),
    row("pub_dengue_nowcast", function(fa) {
      pub_dengue_nowcast(
        locations = "pr",
        epiweeks = epirange(201401, 202301),
        fetch_args = fa
      )
    }),
    row("pub_dengue_nowcast wildcard", function(fa) {
      pub_dengue_nowcast(locations = "ca", epiweeks = "*", fetch_args = fa)
    }),
    row("pvt_dengue_sensors", function(fa) {
      pvt_dengue_sensors(
        auth = auth,
        names = "ght",
        locations = "ag",
        epiweeks = epirange(201501, 202001),
        fetch_args = fa
      )
    }),
    row("pvt_dengue_sensors wildcard", function(fa) {
      pvt_dengue_sensors(
        auth = auth,
        names = "ght",
        locations = "ag",
        epiweeks = "*",
        fetch_args = fa
      )
    }),
    row("pub_ecdc_ili", function(fa) {
      pub_ecdc_ili(
        regions = "austria",
        epiweeks = epirange(201901, 202001),
        fetch_args = fa
      )
    }),
    row("pub_ecdc_ili wildcard", function(fa) {
      pub_ecdc_ili(regions = "austria", epiweeks = "*", fetch_args = fa)
    }),
    row("pub_flusurv", function(fa) {
      pub_flusurv(
        locations = "ca",
        epiweeks = epirange(201701, 201801),
        fetch_args = fa
      )
    }),
    row("pub_flusurv wildcard", function(fa) {
      pub_flusurv(locations = "CA", epiweeks = "*", fetch_args = fa)
    }),
    row("pub_fluview_clinical", function(fa) {
      pub_fluview_clinical(
        regions = "nat",
        epiweeks = epirange(201601, 201701),
        fetch_args = fa
      )
    }),
    row("pub_fluview_clinical wildcard", function(fa) {
      pub_fluview_clinical(regions = "nat", epiweeks = "*", fetch_args = fa)
    }),
    row("pub_fluview_meta", function(fa) {
      pub_fluview_meta(fetch_args = fa)
    }),
    row("pub_fluview", function(fa) {
      pub_fluview(
        regions = "nat",
        epiweeks = epirange(201201, 202005),
        fetch_args = fa
      )
    }),
    row("pub_fluview wildcard", function(fa) {
      pub_fluview(regions = "nat", epiweeks = "*", fetch_args = fa)
    }),
    row("pub_gft", function(fa) {
      pub_gft(
        locations = "hhs1",
        epiweeks = epirange(201201, 202001),
        fetch_args = fa
      )
    }),
    row("pub_gft wildcard", function(fa) {
      pub_gft(locations = "hhs1", epiweeks = "*", fetch_args = fa)
    }),
    row("pvt_ght", function(fa) {
      pvt_ght(
        auth = auth,
        locations = "ma",
        epiweeks = epirange(199301, 202304),
        query = "how to get over the flu",
        fetch_args = fa
      )
    }),
    row("pvt_ght wildcard", function(fa) {
      pvt_ght(
        auth = auth,
        locations = "ca",
        epiweeks = "*",
        query = "how to get over the flu",
        fetch_args = fa
      )
    }),
    row("pub_kcdc_ili", function(fa) {
      pub_kcdc_ili(regions = "ROK", epiweeks = 200436, fetch_args = fa)
    }),
    row("pub_kcdc_ili wildcard", function(fa) {
      pub_kcdc_ili(regions = "ROK", epiweeks = "*", fetch_args = fa)
    }),
    row("pvt_meta_norostat", function(fa) {
      pvt_meta_norostat(auth = auth, fetch_args = fa)
    }),
    row("pub_meta", function(fa) {
      pub_meta(fetch_args = fa)
    }),
    row("pub_nidss_dengue", function(fa) {
      pub_nidss_dengue(
        locations = "taipei",
        epiweeks = epirange(201201, 201301),
        fetch_args = fa
      )
    }),
    row("pub_nidss_dengue wildcard", function(fa) {
      pub_nidss_dengue(locations = "taipei", epiweeks = "*", fetch_args = fa)
    }),
    row("pub_nidss_flu", function(fa) {
      pub_nidss_flu(
        regions = "taipei",
        epiweeks = epirange(201501, 201601),
        fetch_args = fa
      )
    }),
    row("pub_nidss_flu wildcard", function(fa) {
      pub_nidss_flu(regions = "taipei", epiweeks = "*", fetch_args = fa)
    }),
    row("pvt_norostat", function(fa) {
      pvt_norostat(
        auth = auth,
        locations = "Minnesota, Ohio, Oregon, Tennessee, and Wisconsin",
        epiweeks = 201233,
        fetch_args = fa
      )
    }),
    row("pvt_norostat wildcard", function(fa) {
      pvt_norostat(
        auth = auth,
        locations = "Minnesota, Ohio, Oregon, Tennessee, and Wisconsin",
        epiweeks = "*",
        fetch_args = fa
      )
    }),
    row("pub_nowcast", function(fa) {
      pub_nowcast(
        locations = "ca",
        epiweeks = epirange(201201, 201301),
        fetch_args = fa
      )
    }),
    row("pub_nowcast wildcard", function(fa) {
      pub_nowcast(locations = "ca", epiweeks = "*", fetch_args = fa)
    }),
    row("pub_paho_dengue", function(fa) {
      pub_paho_dengue(
        regions = "ca",
        epiweeks = epirange(201401, 201501),
        fetch_args = fa
      )
    }),
    row("pub_paho_dengue wildcard", function(fa) {
      pub_paho_dengue(regions = "ca", epiweeks = "*", fetch_args = fa)
    }),
    row("pvt_quidel", function(fa) {
      pvt_quidel(
        auth = auth,
        epiweeks = epirange(201201, 202001),
        locations = "hhs1",
        fetch_args = fa
      )
    }),
    row("pvt_quidel wildcard", function(fa) {
      pvt_quidel(
        auth = auth,
        epiweeks = "*",
        locations = "hhs1",
        fetch_args = fa
      )
    }),
    row("pvt_sensors", function(fa) {
      pvt_sensors(
        auth = auth,
        names = "sar3",
        locations = "nat",
        epiweeks = epirange(201501, 202001),
        fetch_args = fa
      )
    }),
    row("pvt_sensors wildcard", function(fa) {
      pvt_sensors(
        auth = auth,
        names = "sar3",
        locations = "nat",
        epiweeks = "*",
        fetch_args = fa
      )
    }),
    row("pvt_twitter week", function(fa) {
      pvt_twitter(
        auth = auth,
        locations = "CA",
        time_type = "week",
        time_values = epirange(201501, 202001),
        fetch_args = fa
      )
    }),
    row("pvt_twitter week wildcard", function(fa) {
      pvt_twitter(
        auth = auth,
        locations = "CA",
        time_type = "week",
        time_values = "*",
        fetch_args = fa
      )
    }),
    row("pvt_twitter day", function(fa) {
      pvt_twitter(
        auth = auth,
        locations = "CA",
        time_type = "day",
        time_values = epirange(20150101, 20200101),
        fetch_args = fa
      )
    }),
    row("pvt_twitter day wildcard", function(fa) {
      pvt_twitter(
        auth = auth,
        locations = "CA",
        time_type = "day",
        time_values = "*",
        fetch_args = fa
      )
    }),
    row("pub_wiki week", function(fa) {
      pub_wiki(
        articles = "avian_influenza",
        time_type = "week",
        time_values = epirange(201501, 201601),
        fetch_args = fa
      )
    }),
    row("pub_wiki week wildcard", function(fa) {
      pub_wiki(
        articles = "avian_influenza",
        time_type = "week",
        time_values = "*",
        fetch_args = fa
      )
    }),
    row("pub_wiki day", function(fa) {
      pub_wiki(
        articles = "avian_influenza",
        time_type = "day",
        time_values = epirange(20150101, 20200101),
        fetch_args = fa
      )
    }),
    row("pub_wiki day wildcard", function(fa) {
      pub_wiki(
        articles = "avian_influenza",
        time_type = "day",
        time_values = "*",
        fetch_args = fa
      )
    }),
    row(
      "epidata_snapshot",
      function(fa) {
        epidata_snapshot(
          source = "nssp",
          signals = "pct_ed_visits_influenza",
          geo_type = "state",
          geo_values = "pa",
          snapshot_date = "2025-01-01",
          fetch_args = fa
        )
      },
      live = FALSE
    ),
    row(
      "epidata_archive",
      function(fa) {
        epidata_archive(
          source = "nssp",
          signals = "pct_ed_visits_influenza",
          geo_type = "state",
          geo_values = "pa",
          report_time = "<2025-06-01",
          fetch_args = fa
        )
      },
      live = FALSE
    ),
    row(
      "epidata_aux",
      function(fa) {
        epidata_aux(
          "nwss",
          report_time = "<2025-06-01",
          pcr_target = "sars-cov-2",
          columns = c("geo_value", "population_served"),
          fetch_args = fa
        )
      },
      live = FALSE
    ),
    row(
      "epidata dispatcher",
      function(fa) {
        epidata(
          source = "nssp",
          signals = "pct_ed_visits_influenza",
          geo_type = "state",
          geo_values = "pa",
          report_time = "*",
          fetch_args = fa
        )
      },
      live = FALSE
    )
  )
}

# The R class each EpidataFieldInfo type should parse to.
meta_type_classes <- c(
  date = "Date",
  epiweek = "Date",
  timestamp = "POSIXct",
  int = "numeric",
  float = "numeric",
  bool = "logical",
  categorical = "factor",
  text = "character"
)

# Assert that a fetched data frame's columns have the classes promised by the
# endpoint's field metadata (from a dry_run call).
expect_meta_classes <- function(result, meta) {
  for (info in meta) {
    if (!info$name %in% names(result)) {
      next
    }
    expected <- meta_type_classes[[info$type]]
    testthat::expect_true(
      inherits(result[[info$name]], expected),
      label = sprintf(
        "column `%s` has class %s (expected %s for type '%s')",
        info$name,
        paste(class(result[[info$name]]), collapse = "/"),
        expected,
        info$type
      )
    )
  }
}
