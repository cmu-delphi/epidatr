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

  string. Your restricted access key (not the same as API key).

- locations:

  character. List of locations to fetch. See [geographic
  codes](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#us-states-and-territories)
  \# nolint for details.

- epiweeks:

  [`timeset`](https://cmu-delphi.github.io/epidatr/reference/timeset.md).
  Epiweeks to fetch. Supports
  [`epirange()`](https://cmu-delphi.github.io/epidatr/reference/epirange.md)
  and defaults to all ("\*") dates. Format as
  `epirange(startweek, endweek)`, where startweek and endweek are of the
  form YYYYWW (string or numeric).

- query:

  string. The query to be fetched.

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
if (FALSE) { # \dontrun{
pvt_ght(
  auth = Sys.getenv("DELPHI_EPIDATA_KEY"),
  locations = "ma",
  epiweeks = epirange(199301, 202304),
  query = "how to get over the flu"
)
} # }
```
