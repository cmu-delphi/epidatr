# Abort if more than one of the named arguments is non-NULL

Pass the arguments by name, e.g.
`assert_exclusive(issues = issues, lag = lag)`.

## Usage

``` r
assert_exclusive(..., class = NULL, call = rlang::caller_env())
```

## Arguments

- ...:

  named arguments; `NULL` means "not supplied".

- class:

  extra condition class to add to the error.

- call:

  the call shown in the error message.
