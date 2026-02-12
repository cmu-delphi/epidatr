# This script duplicates all the live API calls under every endpoint.
# Use it to make sure that every endpoint returns non-trivial data.
skip_on_cran()
skip_if_not(curl::has_internet())
# Generally we skip this. Run it manually.
skip()

auth <- Sys.getenv("DELPHI_EPIDATA_KEY")

test_that("pvt_cdc", {
  result <- pvt_cdc(
    auth = auth,
    locations = "fl,ca",
    epiweeks = epirange(201501, 201601)
  )
  expect_gt(nrow(result), 0)
})

test_that("pub_covid_hosp_facility_lookup", {
  result <- pub_covid_hosp_facility_lookup(state = "fl")
  expect_gt(nrow(result), 0)
})

test_that("pub_covid_hosp_facility", {
  result <- pub_covid_hosp_facility(
    hospital_pks = "100075",
    collection_weeks = epirange(20200101, 20200501)
  )
  expect_gt(nrow(result), 0)
})

test_that("pub_covid_hosp_state_timeseries", {
  result <- pub_covid_hosp_state_timeseries(
    states = "fl",
    dates = epirange(20200101, 20200501)
  )
  expect_gt(nrow(result), 0)
})

test_that("pub_covidcast_meta", {
  result <- pub_covidcast_meta()
  expect_gt(nrow(result), 0)
})

test_that("pub_covidcast", {
  result <- pub_covidcast(
    source = "jhu-csse",
    signals = "confirmed_7dav_incidence_prop",
    geo_type = "state",
    time_type = "day",
    geo_values = c("ca", "fl"),
    time_values = epirange(20200601, 20200801)
  )
  expect_gt(nrow(result), 0)
})

test_that("pub_delphi", {
  result <- pub_delphi(system = "ec", epiweek = 201501)
  expect_gt(length(result), 0)
})

test_that("pub_dengue_nowcast", {
  result <- pub_dengue_nowcast(
    locations = "pr",
    epiweeks = epirange(201401, 202301)
  )
  expect_gt(nrow(result), 0)
})

test_that("pvt_dengue_sensors", {
  result <- pvt_dengue_sensors(
    auth = auth,
    names = "ght",
    locations = "ag",
    epiweeks = epirange(201501, 202001)
  )
  expect_gt(nrow(result), 0)
})

test_that("pub_ecdc_ili", {
  result <- pub_ecdc_ili(
    regions = "austria",
    epiweeks = epirange(201901, 202001)
  )
  expect_gt(nrow(result), 0)
})

test_that("pub_flusurv", {
  result <- pub_flusurv(
    locations = "ca",
    epiweeks = epirange(201701, 201801)
  )
  expect_gt(nrow(result), 0)
})

test_that("pub_fluview_clinical", {
  result <- pub_fluview_clinical(
    regions = "nat",
    epiweeks = epirange(201601, 201701)
  )
  expect_gt(nrow(result), 0)
})

test_that("pub_fluview_meta", {
  result <- pub_fluview_meta()
  expect_gt(nrow(result), 0)
})

test_that("pub_fluview", {
  result <- pub_fluview(
    regions = "nat",
    epiweeks = epirange(201201, 202005)
  )
  expect_gt(nrow(result), 0)
})

test_that("pub_gft", {
  result <- pub_gft(
    locations = "hhs1",
    epiweeks = epirange(201201, 202001)
  )
  expect_gt(nrow(result), 0)
})

test_that("pvt_ght", {
  result <- pvt_ght(
    auth = auth,
    locations = "ma",
    epiweeks = epirange(199301, 202304),
    query = "how to get over the flu"
  )
  expect_gt(nrow(result), 0)
})

test_that("pub_kcdc_ili", {
  result <- pub_kcdc_ili(
    regions = "ROK",
    epiweeks = 200436
  )
  expect_gt(nrow(result), 0)
})

test_that("pvt_meta_norostat", {
  result <- pvt_meta_norostat(auth = auth)
  expect_gt(length(result), 0)
})

test_that("pub_meta", {
  result <- pub_meta()
  expect_gt(length(result), 0)
})

test_that("pub_nidss_dengue", {
  result <- pub_nidss_dengue(
    locations = "taipei",
    epiweeks = epirange(201201, 201301)
  )
  expect_gt(nrow(result), 0)
})

test_that("pub_nidss_flu", {
  result <- pub_nidss_flu(
    regions = "taipei",
    epiweeks = epirange(201501, 201601)
  )
  expect_gt(nrow(result), 0)
})

test_that("pvt_norostat", {
  result <- pvt_norostat(
    auth = auth,
    locations = "Minnesota, Ohio, Oregon, Tennessee, and Wisconsin",
    epiweeks = 201233
  )
  expect_gt(nrow(result), 0)
})

test_that("pub_nowcast", {
  result <- pub_nowcast(
    locations = "ca",
    epiweeks = epirange(201201, 201301)
  )
  expect_gt(nrow(result), 0)
})

test_that("pub_paho_dengue", {
  result <- pub_paho_dengue(
    regions = "ca",
    epiweeks = epirange(201401, 201501)
  )
  expect_gt(nrow(result), 0)
})

test_that("pvt_quidel", {
  result <- pvt_quidel(
    auth = auth,
    epiweeks = epirange(201201, 202001),
    locations = "hhs1"
  )
  expect_gt(nrow(result), 0)
})

test_that("pvt_sensors", {
  result <- pvt_sensors(
    auth = auth,
    names = "sar3",
    locations = "nat",
    epiweeks = epirange(201501, 202001)
  )
  expect_gt(nrow(result), 0)
})

test_that("pvt_twitter week", {
  result <- pvt_twitter(
    auth = auth,
    locations = "CA",
    time_type = "week",
    time_values = epirange(201501, 202001)
  )
  expect_gt(nrow(result), 0)
})

test_that("pvt_twitter day", {
  result <- pvt_twitter(
    auth = auth,
    locations = "CA",
    time_type = "day",
    time_values = epirange(20150101, 20200101)
  )
  expect_gt(nrow(result), 0)
})

test_that("pub_wiki week", {
  result <- pub_wiki(
    articles = "avian_influenza",
    time_type = "week",
    time_values = epirange(201501, 201601)
  )
  expect_gt(nrow(result), 0)
})

test_that("pub_wiki day", {
  result <- pub_wiki(
    articles = "avian_influenza",
    time_type = "day",
    time_values = epirange(20150101, 20200101)
  )
  expect_gt(nrow(result), 0)
})
