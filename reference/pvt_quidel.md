# Quidel COVID-19 and influenza testing data

This is a V4 endpoint. Starting in October 2026, it is tentatively
deprecated in favor of the V5 API. The new API can be accessed via the
[`epidata_snapshot()`](https://cmu-delphi.github.io/epidatr/reference/cast_api_queries.md),
[`epidata_archive()`](https://cmu-delphi.github.io/epidatr/reference/cast_api_queries.md),
and
[`epidata_meta()`](https://cmu-delphi.github.io/epidatr/reference/epidata_meta.md)
functions. For more details on the changes, refer to
[`vignette("migration-guide")`](https://cmu-delphi.github.io/epidatr/articles/migration-guide.md),
and visit the [V5 signals
documentation](https://cmu-delphi.github.io/delphi-epidata/api/v5_signals.html)
to see which sources are currently available.

API docs: <https://cmu-delphi.github.io/delphi-epidata/api/quidel.html>

Data provided by Quidel Corp., which contains flu lab test results.

## Usage

``` r
pvt_quidel(auth, locations, epiweeks = "*", fetch_args = fetch_args_list())
```

## Arguments

- auth:

  string. Your restricted access key (not the same as API key).

- locations:

  character. List of locations to fetch. See [HHS regions'
  codes](https://cmu-delphi.github.io/delphi-epidata/api/geographic_codes.html#hhs-regions)
  for details.

- epiweeks:

  [`timeset`](https://cmu-delphi.github.io/epidatr/reference/timeset.md).
  Epiweeks to fetch. Supports
  [`epirange()`](https://cmu-delphi.github.io/epidatr/reference/epirange.md)
  and defaults to all ("\*") dates. Format as
  `epirange(startweek, endweek)`, where startweek and endweek are of the
  form YYYYWW (string or numeric).

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
pvt_quidel(
  auth = Sys.getenv("DELPHI_EPIDATA_KEY"),
  epiweeks = epirange(201201, 202001),
  locations = "hhs1"
)
} # }
```
