# epidatr 0.8.0

- Fix source name duplication bug in `covidcast_epidata`
- Mark `covidcast_epidata` as experimental and do not export it
- Change `covidcast` arg `data_source` to `source`
- Make `covidcast` args `issues`, `lag`, and `as_of` mutually exclusive
- Make `covid_hosp_facility_lookup` args `state,` `ccn`, `city`, `zip`, and
  `fips_code` mutually exclusive
- Update documentation to only refer to character or strings (not character
  vectors or character strings)

# epidatr 0.7.1

- Update README.md for better onboarding.
- Consolidate the vignettes into one. Clean up the code, do not eval most examples.

# epidatr 0.7.0

- Remove temporary code for API key transition.
- Add `timeout_seconds` and `return_empty` arguments to `fetch()`.

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
