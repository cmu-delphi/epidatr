test_that("get_auth_key", {
  options(delphi.epidata.key = "epidata")
  expect_equal(get_auth_key(), "epidata")
  options(delphi.epidata.key = NULL)
  Sys.setenv(DELPHI_EPIDATA_KEY = "epidata")
  expect_equal(get_auth_key(), "epidata")
  Sys.setenv(DELPHI_EPIDATA_KEY = "")
})

test_that("authenticate", {
  response <- httr::RETRY("GET", "https://httpbin.org/headers", httr::authenticate("epidata", get_auth_key()))
  expect_equal(
    content(response)$headers$Authorization, 
    paste0("Basic ", base64enc::base64encode(charToRaw("epidata:")))
  )
  response <- httr::RETRY("GET", "https://httpbin.org/headers", httr::authenticate("epidata", "epidata"))
  expect_equal(
    content(response)$headers$Authorization, 
    paste0("Basic ", base64enc::base64encode(charToRaw("epidata:epidata")))
  )
})
