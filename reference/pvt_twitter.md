# HealthTweets total and influenza-related tweets

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

  string. Restricted access key (not the same as API key).

- locations:

  character. Locations to fetch.

- ...:

  not used for values, forces later arguments to bind by name

- time_type:

  string. The temporal resolution of the data (either "day" or "week",
  depending on signal).

- time_values:

  [`timeset`](https://cmu-delphi.github.io/epidatr/reference/timeset.md).
  Dates or epiweeks to fetch. Defaults to all ("\*") dates.

- fetch_args:

  [`fetch_args`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/reference/epidata_call.md).

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## Examples

``` r
if (FALSE) { # \dontrun{
pvt_twitter(
  auth = Sys.getenv("SECRET_API_AUTH_TWITTER"),
  locations = "CA",
  time_type = "week",
  time_values = epirange(201501, 202001)
)
} # }
```
