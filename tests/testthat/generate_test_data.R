library(httr2)

epidata_call %>%
  request_epidata() %>%
  readr::write_rds(testthat::test_path("data/flusurv-epiweeks.rds"))

url <- full_url(epidata_call)
params <- request_arguments(epidata_call, "csv", NULL)
result <- do_request(url, params, timeout_seconds = 10 * 60) %>%
  readr::write_rds(testthat::test_path("data/test-http401.rds"))

epidata_call <- pvt_afhsb(
  auth = Sys.getenv("SECRET_API_AUTH_AFHSB"),
  locations = "mn",
  epiweeks = epirange(202002, 202110),
  flu_types = "flu1"
)
url <- full_url(epidata_call)
params <- request_arguments(epidata_call, "csv", NULL)
response <- do_request(url, params, timeout_seconds = 10 * 60) %>%
  readr::write_rds(testthat::test_path("data/test-http500.rds"))

epidata_call %>%
  do_request(format_type = "classic") %>%
  readr::write_rds(testthat::test_path("data/test-classic.rds"))

epidata_call %>%
  do_request(format_type = "classic", fields = c("time_value", "value")) %>%
  readr::write_rds(testthat::test_path("data/test-narrower-fields.rds"))

epidata_call %>%
  do_request(format_type = "classic") %>%
  readr::write_rds(testthat::test_path("data/test-classic-only.rds"))

request("https://httpbin.org/status/400") |>
  req_error(is_error = \(resp) FALSE) |>
  req_perform() |>
  readr::write_rds(testthat::test_path("data/test-do_request-httpbin.rds"))
