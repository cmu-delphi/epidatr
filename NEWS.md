# epidatr 0.6.0

- The `fetch_{tbl,classic,df,json,csv}` functions have been replaced by the
`fetch()` function, which almost always returns a tibble, except when used with
a limited number of older endpoints (such as `delphi()` and `meta()`), where it
will output a nested list structure.

# epidatr 0.5.0

- The package that this installs is being renamed from `delphi.epidata` to
`epidatr`. To migrate, run the installation command above, followed by
`remove.packages("delphi.epidata")`, and adjust all references to the package
name accordingly.