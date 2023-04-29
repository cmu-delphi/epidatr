test_that("format_item", {
  expect_equal(format_item(5), "5")
  expect_equal(format_item("5"), "5")
  expect_equal(format_item("*"), "*")
  expect_equal(format_item(epirange(2, 8)), "2-8")
  expect_equal(format_item(as.Date("2020-01-01")), "20200101")
  expect_equal(format_item(c(2, 9)), "2,9")
  expect_equal(format_item(list(2, 9)), "2,9")
})

test_that("format_list", {
  expect_equal(format_list(list(
    5,
    "6",
    epirange(2, 8),
    epirange("2", "5"),
    epirange(as.Date("2020-01-01"), as.Date("2020-01-02")),
    as.Date("2020-01-05"),
    c(2, 9)
  )), "5,6,2-8,2-5,20200101-20200102,20200105,2,9")
})
