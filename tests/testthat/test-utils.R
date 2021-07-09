test_that('format_item', {
    expect_equal(format_item(5), '5')
    expect_equal(format_item('5'), '5')
    expect_equal(format_item(list(from=2, to=5)), '2-5')
    expect_equal(format_item(epirange(2, 8)), '2-8')
})
test_that('format_list', {
    expect_equal(format_list(5), '5')
    expect_equal(format_list('5'), '5')
    expect_equal(format_list(list(from=2, to=5)), '2-5')
    expect_equal(format_list(epirange(2, 8)), '2-8')
    expect_equal(format_list(list('5', '4')), '5,4')
    expect_equal(format_list(list(3, list(from=2, to=5))), '3,2-5')
})
