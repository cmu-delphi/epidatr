# Set custom API request parameters

Used to specify custom options when making API requests, such as to set
timeouts or change data formats. These options are used by
[`fetch()`](https://cmu-delphi.github.io/epidatr/reference/epidata_call.md)
when it makes calls to the Epidata API.

## Usage

``` r
fetch_args_list(
  ...,
  fields = NULL,
  disable_date_parsing = FALSE,
  disable_data_frame_parsing = FALSE,
  return_empty = FALSE,
  timeout_seconds = 15 * 60,
  base_url = NULL,
  dry_run = FALSE,
  debug = lifecycle::deprecated(),
  format_type = lifecycle::deprecated(),
  refresh_cache = FALSE,
  reference_week_day = 1
)
```

## Arguments

- ...:

  not used for values, forces later arguments to bind by name

- fields:

  a list of epidata fields to return, or `NULL` to return all fields
  (default). e.g. `c("time_value", "value")` to return only the
  `time_value` and `value` fields or `c("-direction")` to return
  everything except the direction field

- disable_date_parsing:

  disable automatic date parsing

- disable_data_frame_parsing:

  disable automatic conversion to data frame; this is only supported by
  endpoints that only support the 'classic' format (non-tabular)

- return_empty:

  boolean that allows returning an empty tibble if there is no data

- timeout_seconds:

  the maximum amount of time (in seconds) to wait for a response from
  the API server

- base_url:

  base URL to use; by default `NULL`, which means the global base URL
  `"https://api.delphi.cmu.edu/epidata/"`

- dry_run:

  if `TRUE`, skip the call to the API and instead return the
  `epidata_call` object (useful for debugging)

- debug:

  **\[deprecated\]** No longer supported. Use `dry_run = TRUE` instead.

- format_type:

  **\[deprecated\]** Now managed internally.

- refresh_cache:

  if `TRUE`, ignore the cache, fetch the data from the API, and update
  the cache, if it is enabled

- reference_week_day:

  the day of the week to use as the reference day when parsing epiweeks
  to dates (happens if `disable_date_parsing` is `FALSE`) Defaults to 1
  Sunday (the first day of the week).

## Value

A `fetch_args` object containing all the specified options
