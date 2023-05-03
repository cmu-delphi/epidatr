test_that("format_item", {
  expect_equal(format_item(5), "5")
  expect_equal(format_item("5"), "5")
  expect_equal(format_item("*"), "*")
  expect_equal(format_item(as.Date("2020-01-01")), "20200101")
  expect_equal(format_item(as.Date("2020-01-01") + 0:1), "20200101,20200102")
  expect_equal(format_item(epirange(201501, 201601)), "201501-201601")
})

test_that("format_list", {
  expect_equal(format_list(list(
    5,
    "6",
    epirange(201501, 201601),
    epirange("2015-01-01", "2016-01-01"),
    epirange(as.Date("2020-01-01"), as.Date("2020-01-02")),
    as.Date("2020-01-05"),
    c(2, 9)
  )), "5,6,201501-201601,20150101-20160101,20200101-20200102,20200105,2,9")
})
