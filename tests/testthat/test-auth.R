test_that("get_auth_key", {
  withr::with_envvar(c(DELPHI_EPIDATA_KEY = "epidata_key_from_envvar"), {
    withr::with_options(list(delphi.epidata.key = "epidata_key_from_options"), {
      # envvar takes precedence over option (characterization test; we don't
      # guarantee this in docs):
      expect_identical(get_auth_key(), "epidata_key_from_envvar")
    })
  })
  withr::with_envvar(c(DELPHI_EPIDATA_KEY = NA_character_), {
    withr::with_options(list(delphi.epidata.key = "epidata_key_from_options"), {
      # option by itself works:
      expect_identical(get_auth_key(), "epidata_key_from_options")
    })
  })
})
