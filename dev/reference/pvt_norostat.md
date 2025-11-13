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

  string. Your authentication key.

- locations:

  character. Locations to fetch.

- epiweeks:

  [`timeset`](https://cmu-delphi.github.io/epidatr/dev/reference/timeset.md).
  Epiweeks to fetch. Defaults to all ("\*") dates.

- fetch_args:

  [`fetch_args`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md).

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## Examples

``` r
if (FALSE) { # \dontrun{
pvt_norostat(
  auth = Sys.getenv("SECRET_API_AUTH_NOROSTAT"),
  locations = "Minnesota, Ohio, Oregon, Tennessee, and Wisconsin",
  epiweeks = 201233
)
} # }
```
