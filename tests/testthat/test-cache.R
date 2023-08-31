# need to come up with some tests
# after everything else, make sure that epidatr_cache isn't defined in the global state
# use the existing examples to save, then load and compare the values
# make sure the md5 name is correct
# make sure that file/folder creation works as expected
# make sure the saves are in the right location, maybe load them the dumb way
test_that("check_is_recent", {
  expect_true(epidatr:::check_is_recent(format(Sys.Date() - 7, format = "%Y%m%d"), 7))
  expect_true(epidatr:::check_is_recent(format(Sys.Date(), format = "%Y%m%d"), 7))
  expect_true(epidatr:::check_is_recent(format(Sys.Date() + 12, format = "%Y%m%d"), 7))
  expect_false(epidatr:::check_is_recent(format(Sys.Date() - 12, format = "%Y%m%d"), 7))
  expect_false(epidatr:::check_is_recent(epirange(format(Sys.Date() - 12, format = "%Y%m%d"), format(Sys.Date() - 9, format = "%Y%m%d")), 7))
  expect_true(epidatr:::check_is_recent(epirange(format(Sys.Date() - 12, format = "%Y%m%d"), format(Sys.Date(), format = "%Y%m%d")), 7))
  expect_true(epidatr:::check_is_recent(epirange(format(Sys.Date() - 2, format = "%Y%m%d"), format(Sys.Date(), format = "%Y%m%d")), 7))
  expect_true(epidatr:::check_is_recent(epirange(format(Sys.Date(), format = "%Y%m%d"), format(Sys.Date() + 5, format = "%Y%m%d")), 7))
})
