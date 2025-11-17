# Google Health Trends health topics search volume

API docs: <https://cmu-delphi.github.io/delphi-epidata/api/ght.html>

Estimate of influenza activity based on volume of certain search
queries. …

## Usage

``` r
pvt_ght(auth, locations, epiweeks = "*", query, fetch_args = fetch_args_list())
```

## Arguments

- auth:

  string. Restricted access key (not the same as API key).

- locations:

  character. Locations to fetch.

- epiweeks:

  [`timeset`](https://cmu-delphi.github.io/epidatr/reference/timeset.md).
  Epiweeks to fetch. Defaults to all ("\*") dates.

- query:

  string. The query to be fetched.

- fetch_args:

  [`fetch_args`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/reference/epidata_call.md).

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## Examples

``` r
if (FALSE) { # \dontrun{
pvt_ght(
  auth = Sys.getenv("SECRET_API_AUTH_GHT"),
  locations = "ma",
  epiweeks = epirange(199301, 202304),
  query = "how to get over the flu"
)
} # }
```
