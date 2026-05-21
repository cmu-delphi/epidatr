# CDC NoroSTAT norovirus outbreaks

This is point data only, and does not include minima or maxima.

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/norostat.html>

This is the documentation of the API for accessing the NoroSTAT endpoint
of the Delphi’s epidemiological data.

## Usage

``` r
pvt_norostat(auth, locations, epiweeks = "*", fetch_args = fetch_args_list())
```

## Arguments

- auth:

  string. Your restricted access key (not the same as API key).

- locations:

  character. Locations to fetch. Only a specific list of full state
  names are permitted. See the `locations` column in the output of
  [`pvt_meta_norostat()`](https://cmu-delphi.github.io/epidatr/dev/reference/pvt_meta_norostat.md)
  for the allowed values.

- epiweeks:

  [`timeset`](https://cmu-delphi.github.io/epidatr/dev/reference/timeset.md).
  Epiweeks to fetch. Supports
  [`epirange()`](https://cmu-delphi.github.io/epidatr/dev/reference/epirange.md)
  and defaults to all ("\*") dates. Format as
  `epirange(startweek, endweek)`, where startweek and endweek are of the
  form YYYYWW (string or numeric).

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
pvt_norostat(
  auth = Sys.getenv("DELPHI_EPIDATA_KEY"),
  locations = "Minnesota, Ohio, Oregon, Tennessee, and Wisconsin",
  epiweeks = 201233
)
} # }
```
