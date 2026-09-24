# HealthTweets total and influenza-related tweets

**\[deprecated\]**

This endpoint was previously deprecated and remains available for
historical reference. The underlying data source is no longer updated
and no new data is being ingested. For more details, refer to the
"Endpoints kept for historical reference" section of
[`vignette("migration-guide")`](https://cmu-delphi.github.io/epidatr/dev/articles/migration-guide.md),
and visit the [V5 signals
documentation](https://cmu-delphi.github.io/delphi-epidata/api/v5_signals.html).

API docs: <https://cmu-delphi.github.io/delphi-epidata/api/twitter.html>

This is the API documentation for accessing the Twitter Stream endpoint
of Delphi’s epidemiological data. Sourced from
[Healthtweets](http://www.healthtweets.org/)

## Usage

``` r
pvt_twitter(
  auth,
  locations,
  ...,
  time_type = c("day", "week"),
  time_values = "*",
  fetch_args = fetch_args_list()
)
```

## Arguments

- auth:

  string. Your restricted access key (not the same as API key).

- locations:

  character. List of locations to fetch. See the codes of the [US
  regions and
  states](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#us-regions-and-states)
  \# nolint for details.

- ...:

  not used for values, forces later arguments to bind by name

- time_type:

  string. The temporal resolution of the data (either "day" or "week",
  depending on signal).

- time_values:

  [`timeset`](https://cmu-delphi.github.io/epidatr/dev/reference/timeset.md).
  Dates or epiweeks to fetch. Supports
  [`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md)
  and defaults to all ("\*") dates.

- fetch_args:

  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md).
  See
  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md)
  for details.

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## See also

For example queries showing how to discover signals and build calls, see
[`vignette("signal-discovery", package = "epidatr")`](https://cmu-delphi.github.io/epidatr/dev/articles/signal-discovery.md).

## Examples

``` r
if (FALSE) { # \dontrun{
pvt_twitter(
  auth = Sys.getenv("DELPHI_EPIDATA_KEY"),
  locations = "CA",
  time_type = "week",
  time_values = epirange(201501, 202001)
)
} # }
```
