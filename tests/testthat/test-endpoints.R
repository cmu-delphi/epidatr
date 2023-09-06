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
    locations = "?",
    epiweeks = epirange(201501, 202001),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pvt_dengue_sensors(
    auth = "yourkey",
    names = "?",
    locations = "?",
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
    query = "?",
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_kcdc_ili(
    regions = "?",
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
    epiweeks = epirange(201501, 202001),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
  expect_no_error(pub_wiki(
    articles = "avian_influenza",
    epiweeks = epirange(201501, 202001),
    fetch_args = fetch_args_list(dry_run = TRUE)
  ) %>% request_url())
})
