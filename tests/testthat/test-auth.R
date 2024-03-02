test_that("get_api_key", {
  withr::with_envvar(c(DELPHI_EPIDATA_KEY = "epidata_key_from_envvar"), {
    expect_identical(get_api_key(), "epidata_key_from_envvar")
  })
})
