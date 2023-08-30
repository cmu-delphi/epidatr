epidata_call %>%
  fetch_classic() %>%
  readr::write_rds(testthat::test_path("data/flusurv-epiweeks.rds"))

url <- full_url(epidata_call)
params <- request_arguments(epidata_call, "csv", NULL)
result <- do_request(url, params) %>% readr::write_rds(testthat::test_path("data/test-http401.rds"))

epidata_call <- pvt_afhsb(
  auth = Sys.getenv("SECRET_API_AUTH_AFHSB"),
  locations = "mn",
  epiweeks = epirange(202002, 202110),
  flu_types = "flu1"
)
url <- full_url(epidata_call)
params <- request_arguments(epidata_call, "csv", NULL)
response <- do_request(url, params) %>% readr::write_rds(testthat::test_path("data/test-http500.rds"))

epidata_call %>%
  fetch_debug(format_type = "classic") %>%
  readr::write_rds(testthat::test_path("data/test-classic.rds"))

epidata_call %>%
  fetch_debug(format_type = "classic", fields = c("time_value", "value")) %>%
  readr::write_rds(testthat::test_path("data/test-narrower-fields.rds"))

epidata_call %>%
  fetch_debug(format_type = "classic") %>%
  readr::write_rds(testthat::test_path("data/test-classic-only.rds"))

response <- httr::RETRY("GET",
  url = "https://httpbin.org/status/400",
  query = list(),
  terminate_on = c(400, 401, 403, 405, 414, 500),
  http_headers,
  httr::authenticate("epidata", get_auth_key())
) %>% readr::write_rds(testthat::test_path("data/test-do_request-httpbin.rds"))
