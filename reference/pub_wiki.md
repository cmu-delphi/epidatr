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

  character. Articles to fetch. See [available
  articles](https://cmu-delphi.github.io/delphi-epidata/api/wiki.html#available-articles)
  for details.

- ...:

  not used for values, forces later arguments to bind by name

- time_type:

  string. The temporal resolution of the data (either "day" or "week",
  depending on signal).

- time_values:

  [`timeset`](https://cmu-delphi.github.io/epidatr/reference/timeset.md).
  Dates or epiweeks to fetch. Supports
  [`epirange()`](https://cmu-delphi.github.io/epidatr/reference/epirange.md)
  and defaults to all ("\*") dates.

- hours:

  integer. Optionally, the hours to fetch.

- language:

  string. Language to fetch.

- fetch_args:

  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/reference/epidata_call.md).
  See
  [`fetch_args_list()`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md)
  for details.

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## See also

For example queries showing how to discover signals and build calls, see
[`vignette("signal-discovery", package = "epidatr")`](https://cmu-delphi.github.io/epidatr/articles/signal-discovery.md).

## Examples

``` r

pub_wiki(
  articles = "avian_influenza",
  time_type = "week",
  time_values = epirange(201501, 201601)
)
#> `pub_wiki()` covers a data source that is no longer updated.
#> ℹ Historical data remains available, but no new data is being ingested.
#> ℹ See the "Endpoints kept for historical reference" section of
#>   `vignette("migration-guide")` (or
#>   <https://cmu-delphi.github.io/epidatr/articles/migration-guide.html#endpoints-kept-for-historical-reference>)
#>   for details.
#> This message is displayed once per session.
#> # A tibble: 53 × 6
#>    article         count      total  hour epiweek    value
#>    <chr>           <dbl>      <dbl> <dbl> <date>     <dbl>
#>  1 avian_influenza  4687 1422164068    -1 2015-01-04  3.30
#>  2 avian_influenza  5467 1348289071    -1 2015-01-11  4.05
#>  3 avian_influenza  7090 1402471472    -1 2015-01-18  5.06
#>  4 avian_influenza  6088 1401781736    -1 2015-01-25  4.34
#>  5 avian_influenza  5325 1390433062    -1 2015-02-01  3.83
#>  6 avian_influenza  4501 1396774810    -1 2015-02-08  3.22
#>  7 avian_influenza  4671 1373094504    -1 2015-02-15  3.40
#>  8 avian_influenza  5108 1443965380    -1 2015-02-22  3.54
#>  9 avian_influenza  4298 1466819824    -1 2015-03-01  2.93
#> 10 avian_influenza  4889 1424146607    -1 2015-03-08  3.43
#> # ℹ 43 more rows
```
