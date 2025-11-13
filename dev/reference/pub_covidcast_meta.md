# Metadata for the COVIDcast endpoint

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/covidcast_meta.html>.

Fetch a summary of metadata for all sources and signals that are
available in the API, along with basic summary statistics such as the
dates they are available, the geographic levels at which they are
reported, and etc.

## Usage

``` r
pub_covidcast_meta(fetch_args = fetch_args_list())
```

## Arguments

- fetch_args:

  [`fetch_args`](https://cmu-delphi.github.io/epidatr/dev/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/dev/reference/epidata_call.md).

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## See also

[`pub_covidcast()`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md),[`covidcast_epidata()`](https://cmu-delphi.github.io/epidatr/dev/reference/covidcast_epidata.md)

## Examples

``` r
if (FALSE) { # \dontrun{
pub_covidcast_meta()
} # }
```
