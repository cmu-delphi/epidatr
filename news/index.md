# Changelog

## epidatr 1.3.0

### Deprecations

- [`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/reference/pub_covidcast.md),
  [`pub_covidcast_meta()`](https://cmu-delphi.github.io/epidatr/reference/pub_covidcast_meta.md),
  [`pub_fluview()`](https://cmu-delphi.github.io/epidatr/reference/pub_fluview.md),
  [`pub_fluview_clinical()`](https://cmu-delphi.github.io/epidatr/reference/pub_fluview_clinical.md),
  [`pub_fluview_meta()`](https://cmu-delphi.github.io/epidatr/reference/pub_fluview_meta.md),
  [`pub_flusurv()`](https://cmu-delphi.github.io/epidatr/reference/pub_flusurv.md),
  [`pub_meta()`](https://cmu-delphi.github.io/epidatr/reference/pub_meta.md),
  and
  [`pvt_quidel()`](https://cmu-delphi.github.io/epidatr/reference/pvt_quidel.md)
  now warn that they use the V4 Epidata API. Starting in October 2026,
  it is tentatively deprecated in favor of the V5 API. See
  [`vignette("migration-guide")`](https://cmu-delphi.github.io/epidatr/articles/migration-guide.md)
  for the V5 replacements.
- The remaining V4 endpoints whose data sources are no longer updated
  ([`pvt_cdc()`](https://cmu-delphi.github.io/epidatr/reference/pvt_cdc.md),
  [`pub_covid_hosp_facility_lookup()`](https://cmu-delphi.github.io/epidatr/reference/pub_covid_hosp_facility_lookup.md),
  [`pub_covid_hosp_facility()`](https://cmu-delphi.github.io/epidatr/reference/pub_covid_hosp_facility.md),
  [`pub_covid_hosp_state_timeseries()`](https://cmu-delphi.github.io/epidatr/reference/pub_covid_hosp_state_timeseries.md),
  [`pub_delphi()`](https://cmu-delphi.github.io/epidatr/reference/pub_delphi.md),
  [`pub_dengue_nowcast()`](https://cmu-delphi.github.io/epidatr/reference/pub_dengue_nowcast.md),
  [`pvt_dengue_sensors()`](https://cmu-delphi.github.io/epidatr/reference/pvt_dengue_sensors.md),
  [`pub_ecdc_ili()`](https://cmu-delphi.github.io/epidatr/reference/pub_ecdc_ili.md),
  [`pub_gft()`](https://cmu-delphi.github.io/epidatr/reference/pub_gft.md),
  [`pvt_ght()`](https://cmu-delphi.github.io/epidatr/reference/pvt_ght.md),
  [`pub_kcdc_ili()`](https://cmu-delphi.github.io/epidatr/reference/pub_kcdc_ili.md),
  [`pvt_meta_norostat()`](https://cmu-delphi.github.io/epidatr/reference/pvt_meta_norostat.md),
  [`pub_nidss_dengue()`](https://cmu-delphi.github.io/epidatr/reference/pub_nidss_dengue.md),
  [`pub_nidss_flu()`](https://cmu-delphi.github.io/epidatr/reference/pub_nidss_flu.md),
  [`pvt_norostat()`](https://cmu-delphi.github.io/epidatr/reference/pvt_norostat.md),
  [`pub_nowcast()`](https://cmu-delphi.github.io/epidatr/reference/pub_nowcast.md),
  [`pub_paho_dengue()`](https://cmu-delphi.github.io/epidatr/reference/pub_paho_dengue.md),
  [`pvt_sensors()`](https://cmu-delphi.github.io/epidatr/reference/pvt_sensors.md),
  [`pvt_twitter()`](https://cmu-delphi.github.io/epidatr/reference/pvt_twitter.md),
  and
  [`pub_wiki()`](https://cmu-delphi.github.io/epidatr/reference/pub_wiki.md))
  now emit a quieter informational note (not a warning) explaining that
  they are frozen rather than deprecated. They are not moving to V5 and
  will keep working, but will not receive new data. See the “Endpoints
  kept for historical reference” section of
  [`vignette("migration-guide")`](https://cmu-delphi.github.io/epidatr/articles/migration-guide.md)
  for the full list.

### Patches

- [`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/reference/cast_api_queries.md)
  and
  [`epidata_archive()`](https://cmu-delphi.github.io/epidatr/reference/cast_api_queries.md)
  now issue one request per signal instead of joining `signals` with a
  comma, which the cast-API server silently matched against nothing
  ([\#354](https://github.com/cmu-delphi/epidatr/issues/354)).
- `epidata_archive(report_time = ...)` now sends the `report_time_query`
  API parameter; the server renamed it from `version_query`, which it
  now rejects.
- Add a migration guide vignette
  ([`vignette("migration-guide")`](https://cmu-delphi.github.io/epidatr/articles/migration-guide.md))
  mapping `pub_covidcast` arguments and columns to the new
  `epidata_snapshot`/`epidata_archive` functions, and update the README
  and pkgdown reference to lead with the new API.
- Parse `ci_lower` and `ci_upper` data columns for some signals.
- Improve CSV parsing performance by reading bytes directly.
- [`save_api_key()`](https://cmu-delphi.github.io/epidatr/reference/get_api_key.md)
  no longer errors when called outside a project; it falls back to the
  user `.Renviron`
  ([\#339](https://github.com/cmu-delphi/epidatr/issues/339)).
- Text fields are now always parsed as character; previously an all-NA
  column (e.g. `geocoded_state` in the `covid_hosp` endpoints) came back
  as logical.

## epidatr 1.2.4

CRAN release: 2026-06-02

### Changes

- Added
  [`epidata_aux()`](https://cmu-delphi.github.io/epidatr/reference/epidata_aux.md)
  to fetch V5-API auxiliary data, either directly by source or by
  merging it onto
  [`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/reference/cast_api_queries.md)/[`epidata_archive()`](https://cmu-delphi.github.io/epidatr/reference/cast_api_queries.md)
  output via a version-aware left join. When no key filters are
  supplied, it infers them from the base to keep the pull small.
- [`epidata_aux()`](https://cmu-delphi.github.io/epidatr/reference/epidata_aux.md),
  [`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/reference/cast_api_queries.md),
  and
  [`epidata_archive()`](https://cmu-delphi.github.io/epidatr/reference/cast_api_queries.md)
  now accept per-key filters as named `...` arguments. Each key takes
  one or more values, sent server-side to shrink the download.
- Improve documentation, including descriptions for
  [`save_api_key()`](https://cmu-delphi.github.io/epidatr/reference/get_api_key.md)
  and endpoint parameters, and standardize parameter information
  ([\#324](https://github.com/cmu-delphi/epidatr/issues/324) and
  [\#334](https://github.com/cmu-delphi/epidatr/issues/334)).
- Modernize internal API architecture by migrating from `httr` to
  `httr2`.
- Introduce a centralized validation system in `R/check.R` using
  `checkmate` to standardize input checking, date parsing, and API
  parameter formatting.
- Refactor `epidata_call` and `do_request` to use `httr2` request
  objects, implement automatic GET-to-POST fallback, and add robust
  retry logic.
- Improve file-level documentation by adding summary descriptors at the
  top of all R source files.
- Deprecate `debug` and `format_type` arguments in
  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md).

### Features

- Added new `epidata`, `epidata_meta`, `epidata_snapshot`, and
  `epidata_archive` for accessing the Delphi V5 API.
- [`pub_covidcast_meta()`](https://cmu-delphi.github.io/epidatr/reference/pub_covidcast_meta.md)
  gained `signals`, `time_type`, and `geo_type` arguments for
  server-side filtering of the metadata results.

### Patches

- `last_update` in `pub_covidcast_meta` is now returned as a `POSIXct`
  object instead of an integer.
- Improved efficiency of date parsing for API responses.
- Migrate HTTP requests from `httr` to `httr2`.
- Update endpoint examples to skip execution when required API keys or
  internet connectivity are unavailable, ensuring cleaner `R CMD check`
  results.
- Add `lifecycle` to `DESCRIPTION` to properly manage function
  deprecations.
- Extend `check_is_recent` to support numeric date formats
  ([\#320](https://github.com/cmu-delphi/epidatr/issues/320)).

## epidatr 1.2.2

CRAN release: 2025-11-17

### Changes

- Add `reference_week_day` argument to `fetch_args_list` and `fetch`
  functions.

### Patches

- Validate that `time_type` is one of “day” or “week” in
  `pub_covidcast`.
- Validate that `time_type` is “week” when source is “nssp” in
  `pub_covidcast`.
- Allow `hsa_nci` as a `geo_type` in `pub_covidcast`.
- Allow `hsa_nci` as a `geo_type` in `pub_covidcast_meta`.
- `pub_covidcast_meta` now returns `min_time`, `max_time`, `max_issue`
  as integers rather than Dates. Because these fields can mix YYYYMMDD
  and YYYYWW values, we recommend you parse them yourself.
- add new fields for `flusurv` endpoint.

## epidatr 1.2.1

CRAN release: 2025-03-20

### Patches

- Fix so that
  [`covidcast_epidata()`](https://cmu-delphi.github.io/epidatr/reference/covidcast_epidata.md)
  will still print if fields are missing.

## epidatr 1.2.0

CRAN release: 2024-06-20

### Changes

- Improve handling of the `EPIDATR_USE_CACHE` environment variable,
  allowing it to be any value convertable by
  [`as.logical()`](https://rdrr.io/r/base/logical.html) and handle the
  case when it can’t be converted.
- Support more date formats in function to convert dates to epiweeks.
  Use `parse_api_date` since it already supports both common formats.
  [\#276](https://github.com/cmu-delphi/epidatr/issues/276)
- `EPIDATR_USE_CACHE` only supported exactly “TRUE” before. Now it
  supports all logical values and includes a warning when any value that
  can’t be converted to logical is provided.
  [\#273](https://github.com/cmu-delphi/epidatr/issues/273)
- `missing` doesn’t count default values as non-missing. If a user
  doesn’t pass `geo_values` or `time_values` (both of which default to
  `"*"` in `pub_covidcast`), or `dates` (in
  `pub_covid_hosp_state_timeseries`), the missing check fails. To avoid
  this, just don’t check missingness of those two arguments.
- `fetch_args_list` now has an `refresh_cache` argument, which is
  `FALSE` by default.

## epidatr 1.1.1

CRAN release: 2024-03-04

### Changes

### Features

### Patches

- Fix failure when passing `as_of` values in `Date` format to
  `pub_covidcast` while caching is enabled
  ([\#259](https://github.com/cmu-delphi/epidatr/issues/259)).
- For `pub_covidcast` data source `nchs-mortality`, parse dates as
  `epiweek` and expect `epiweek` inputs from user
  ([\#260](https://github.com/cmu-delphi/epidatr/issues/260)).
- Fix failure in `pub_covidcast` when user doesn’t pass `geo_values` or
  `time_values`, even though those arguments have defaults
  ([\#268](https://github.com/cmu-delphi/epidatr/issues/268)).

## epidatr 1.1.0

CRAN release: 2024-02-29

### Changes

- `pub_covid_hosp_state_timeseries` now supports use of the `as_of`
  parameter ([\#209](https://github.com/cmu-delphi/epidatr/issues/209)).
- `release_date` and `latest_update` fields are now parsed as `Date`,
  rather than as text. This change impacts several endpoints.
- `get_auth_key` renamed to `get_api_key`
  ([\#181](https://github.com/cmu-delphi/epidatr/issues/181)).
- `get_api_key` no longer reads from R options and only uses environment
  variables ([\#217](https://github.com/cmu-delphi/epidatr/issues/217)).
- `pvt_twitter` and `pub_wiki` now use `time_type` and `time_values`
  args instead of mutually exclusive `dates` and `epiweeks`
  ([\#236](https://github.com/cmu-delphi/epidatr/issues/236)). This
  matches the interface of the `pub_covidcast` endpoint.
- Updated the default `timeout_seconds` to 15 minutes to allow large
  queries by default.

### Features

- Function reference now displays commonly-used functions first
  ([\#205](https://github.com/cmu-delphi/epidatr/issues/205)).
- Support `Date` objects passed to version arguments `as_of` and
  `issues` in endpoints
  ([\#192](https://github.com/cmu-delphi/epidatr/issues/192),
  [\#194](https://github.com/cmu-delphi/epidatr/issues/194)).
- `clear_cache` now handles positional arguments just like `set_cache`
  ([\#197](https://github.com/cmu-delphi/epidatr/issues/197)).
- `set_api_key` now available to help persist API key environment
  variables ([\#181](https://github.com/cmu-delphi/epidatr/issues/181),
  [\#217](https://github.com/cmu-delphi/epidatr/issues/217)).
- All endpoints now support the use of “\*” as a wildcard to fetch all
  dates or epiweeks
  ([\#234](https://github.com/cmu-delphi/epidatr/issues/234)).

### Patches

- Endpoints now fail when passed misspelled arguments
  ([\#187](https://github.com/cmu-delphi/epidatr/issues/187),
  [\#201](https://github.com/cmu-delphi/epidatr/issues/201)).
- `pub_fluview_meta` fixed to `fetch` the response automatically.
- `pub_covid_hosp_state_timeseries` now correctly parses the `issue`
  field, instead of returning a missing value
  ([\#202](https://github.com/cmu-delphi/epidatr/issues/202)).
- In `pub_fluview_meta`, `latest_issue` field is now parsed as epiweek,
  rather than being parsed as `Date` and returning a missing value.
- `set_cache` cache size no longer runs into integer overflow
  ([\#189](https://github.com/cmu-delphi/epidatr/issues/189)).
- Improve line-wrapping of warning messages
  ([\#191](https://github.com/cmu-delphi/epidatr/issues/191)).
- Fix documentation related to CRAN submission.
- Fix some errors from passing “” as a key.
- Fixed bug with NAs when parsing ints
  ([\#243](https://github.com/cmu-delphi/epidatr/issues/243)).

## epidatr 1.0.0

CRAN release: 2023-09-19

- Add `set_cache` and other caching functions.
- Prefix all non-private endpoints with `pub_`.
- Update printing of `avail_endpoints` to be more readable.
- Update printing of
  [`covidcast_epidata()`](https://cmu-delphi.github.io/epidatr/reference/covidcast_epidata.md)
  to be more readable.
- Update landing docs to be more friendly, add plots.

## epidatr 0.9.0

- Major interface change: all endpoints now fetch by default.
- Make all `fetch` function internal.
- Change `fetch` and `fetch_*` function interfaces now rely on
  `fetch_args_list`.
- Added `fetch_args_list` which returns a list of arguments to be passed
  to `fetch`.

## epidatr 0.8.0

- Fix source name duplication bug in `covidcast_epidata`.
- Mark `covidcast_epidata` as experimental and do not export it.
- Change `covidcast` arg `data_source` to `source`.
- Make `covidcast` args `issues`, `lag`, and `as_of` mutually exclusive.
- Make `covid_hosp_facility_lookup` args `state,` `ccn`, `city`, `zip`,
  and `fips_code` mutually exclusive.
- Update documentation to only refer to character or strings (not
  character vectors or character strings).

## epidatr 0.7.1

- Update README.md for better onboarding.
- Consolidate the vignettes into one. Clean up the code, do not eval
  most examples.

## epidatr 0.7.0

- Remove temporary code for API key transition.
- Add `timeout_seconds` and `return_empty` arguments to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/reference/epidata_call.md).

## epidatr 0.6.0

- The `fetch_{tbl,classic,df,json,csv}` functions have been replaced by
  the
  [`fetch()`](https://cmu-delphi.github.io/epidatr/reference/epidata_call.md)
  function, which almost always returns a tibble, except when used with
  a limited number of older endpoints (such as `delphi()` and `meta()`),
  where it will output a nested list structure.

## epidatr 0.5.0

- The package that this installs is being renamed from `delphi.epidata`
  to `epidatr`. To migrate, run the installation command above, followed
  by `remove.packages("delphi.epidata")`, and adjust all references to
  the package name accordingly.
