# epidatr 1.1.0

## Changes
- `pub_covid_hosp_state_timeseries` now supports use of the `as_of` parameter (#209).
- `release_date` and `latest_update` fields are now parsed as `Date`, rather
  than as text. This change impacts several endpoints.
- `get_auth_key` renamed to `get_api_key` (#181).
- `get_api_key` no longer reads from R options and only uses environment variables (#217).
- `pvt_twitter` and `pub_wiki` now use `time_type` and `time_values` args instead of mutually exclusive `dates` and `epiweeks` (#236). This matches the interface of the `pub_covidcast` endpoint.
- Updated the default `timeout_seconds` to 15 minutes to allow large queries by default.
## Features
- Function reference now displays commonly-used functions first (#205).
- Support `Date` objects passed to version arguments `as_of` and `issues` in
  endpoints (#192, #194).
- `clear_cache` now handles positional arguments just like `set_cache` (#197).
- `set_api_key` now available to help persist API key environment variables (#181, #217).
- All endpoints now support the use of "\*" as a wildcard to fetch all dates or epiweeks (#234).
## Patches
- Endpoints now fail when passed misspelled arguments (#187, #201).
- `pub_fluview_meta` fixed to `fetch` the response automatically.
- `pub_covid_hosp_state_timeseries` now correctly parses the `issue` field,
  instead of returning a missing value (#202).
- In `pub_fluview_meta`, `latest_issue` field is now parsed as epiweek, rather
  than being parsed as `Date` and returning a missing value.
- `set_cache` cache size no longer runs into integer overflow (#189).
- Improve line-wrapping of warning messages (#191).
- Fix documentation related to CRAN submission.
- Fix some errors from passing "" as a key.
- Fixed bug with NAs when parsing ints (#243).

# epidatr 1.0.0

- Add `set_cache` and other caching functions.
- Prefix all non-private endpoints with `pub_`.
- Update printing of `avail_endpoints` to be more readable.
- Update printing of `covidcast_epidata()` to be more readable.
- Update landing docs to be more friendly, add plots.

# epidatr 0.9.0

- Major interface change: all endpoints now fetch by default.
- Make all `fetch` function internal.
- Change `fetch` and `fetch_*` function interfaces now rely on `fetch_args_list`.
- Added `fetch_args_list` which returns a list of arguments to be passed to `fetch`.

# epidatr 0.8.0

- Fix source name duplication bug in `covidcast_epidata`.
- Mark `covidcast_epidata` as experimental and do not export it.
- Change `covidcast` arg `data_source` to `source`.
- Make `covidcast` args `issues`, `lag`, and `as_of` mutually exclusive.
- Make `covid_hosp_facility_lookup` args `state,` `ccn`, `city`, `zip`, and
  `fips_code` mutually exclusive.
- Update documentation to only refer to character or strings (not character
  vectors or character strings).

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
