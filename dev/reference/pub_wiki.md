# Wikipedia webpage counts by article

API docs: <https://cmu-delphi.github.io/delphi-epidata/api/wiki.html>
Number of page visits for selected English, Influenza-related wikipedia
articles.

- Source: Wikimedia

- Temporal Resolution: Hourly, daily, and weekly from 2007-12-09
  (2007w50)

- Spatial Resolution: N/A

- Other resolution: By article (54)

- Open access

## Usage

``` r
pub_wiki(
  articles,
  ...,
  time_type = c("day", "week"),
  time_values = "*",
  hours = NULL,
  language = "en",
  fetch_args = fetch_args_list()
)
```

## Arguments

- articles:

  character. Articles to fetch.

- ...:

  not used for values, forces later arguments to bind by name

- time_type:

  string. The temporal resolution of the data (either "day" or "week",
  depending on signal).

- time_values:

  [`timeset`](https://cmu-delphi.github.io/epidatr/dev/reference/timeset.md).
  Dates or epiweeks to fetch. Defaults to all ("\*") dates.

- hours:

  integer. Optionally, the hours to fetch.

- language:

  string. Language to fetch.

- fetch_args:

  [`fetch_args`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md).

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## Examples

``` r
if (FALSE) { # \dontrun{
pub_wiki(
  articles = "avian_influenza",
  time_type = "week",
  time_values = epirange(201501, 201601)
)
} # }
```
