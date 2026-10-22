test_that("get_api_key", {
  withr::with_envvar(c(DELPHI_EPIDATA_KEY = "epidata_key_from_envvar"), {
    expect_identical(get_api_key(), "epidata_key_from_envvar")
  })

  withr::with_envvar(c(DELPHI_EPIDATA_KEY = ""), {
    expect_warning(
      res <- get_api_key(),
      class = "rlang_warning"
    )
    expect_identical(res, "")
  })
})

