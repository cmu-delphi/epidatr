test_that("get_api_key", {
  withr::with_envvar(c(DELPHI_EPIDATA_KEY = "epidata_key_from_envvar"), {
    withr::with_options(list(delphi.epidata.key = "epidata_key_from_options"), {
      # option takes precedence over environment variable
      expect_identical(get_api_key(), "epidata_key_from_options")
    })
  })
  withr::with_envvar(c(DELPHI_EPIDATA_KEY = NA_character_), {
    withr::with_options(list(delphi.epidata.key = "epidata_key_from_options"), {
      # option by itself works:
      expect_identical(get_api_key(), "epidata_key_from_options")
    })
  })
})
