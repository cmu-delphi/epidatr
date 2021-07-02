test_that('hello', {
    expect_equal(hello(), 'Hello World')
    expect_equal(hello('Delphi'), 'Hello Delphi')
})