# Delphi's PAHO dengue nowcasts (North and South America)

API docs:
<https://cmu-delphi.github.io/delphi-epidata/api/dengue_nowcast.html>

## Usage

``` r
pub_dengue_nowcast(locations, epiweeks = "*", fetch_args = fetch_args_list())
```

## Arguments

- locations:

  character. Locations to fetch.

- epiweeks:

  [`timeset`](https://cmu-delphi.github.io/epidatr/reference/timeset.md).
  Epiweeks to fetch. Defaults to all ("\*") dates.

- fetch_args:

  [`fetch_args`](https://cmu-delphi.github.io/epidatr/reference/fetch_args_list.md).
  Additional arguments to pass to
  [`fetch()`](https://cmu-delphi.github.io/epidatr/reference/epidata_call.md).

## Value

[`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)

## Examples

``` r
if (FALSE) { # \dontrun{
pub_dengue_nowcast(
  locations = "pr",
  epiweeks = epirange(201401, 202301)
)
} # }
```
