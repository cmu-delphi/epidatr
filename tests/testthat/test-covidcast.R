test_that("covidcast", {
  covidcast_api <- covidcast_epidata()
  expect_identical(
    covidcast_api$sources$`fb-survey`$signals$smoothed_cli$call("nation", "us", epirange(20210405, 20210410)),
    covidcast("fb-survey", "smoothed_cli", "nation", "day", "us", epirange(20210405, 20210410))
  )
})

# quite minimal, could probably use some checks that the fields are as desired
test_that("dataframe converters", {
  res <- covidcast_epidata()$sources %>% as.data.frame()
  expect_identical(class(res), "data.frame")
  res <- covidcast_epidata()$signals %>% as.data.frame()
  expect_identical(class(res), "data.frame")
})

test_that("http errors", {
  local_mocked_bindings(
    do_request = function(...) {
      httr::RETRY("GET",
        url = "https://httpbin.org/status/400",
        query = list(),
        terminate_on = c(400, 401, 403, 405, 414, 500),
        http_headers,
        httr::authenticate("epidata", get_auth_key())
      )
    }
  )
  expect_error(covidcast_epidata(), class = "http_400")
})


test_that("name completion", {
  all_names <- names(covidcast_epidata()$signals)
  expect_identical(all_names, all_names)
})
