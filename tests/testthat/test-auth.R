test_that("get_auth_key", {
  options(delphi.epidata.key = "epidata")
  expect_identical(get_auth_key(), "epidata")
  options(delphi.epidata.key = NULL)
  Sys.setenv(DELPHI_EPIDATA_KEY = "epidata")
  expect_identical(get_auth_key(), "epidata")
  Sys.setenv(DELPHI_EPIDATA_KEY = "")
})
