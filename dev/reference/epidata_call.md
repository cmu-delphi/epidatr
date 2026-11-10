# An abstraction that holds information needed to make an epidata request

`epidata_call` objects are generated internally by endpoint functions
like
[`pub_covidcast`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md);
by default, they are piped directly into the `fetch` function to fetch
and format the data. For most endpoints this will return a tibble, but a
few non-COVIDCAST endpoints will return a JSON-like list instead.

## Usage

``` r
create_epidata_call(
  endpoint,
  params,
  meta = NULL,
  api_version = c("classic", "cast"),
  response_format = c("classic", "json", "csv")
)

fetch(epidata_call, fetch_args = fetch_args_list())
```

## Arguments

- endpoint:

  the epidata endpoint to call

- params:

  the parameters to pass to the epidata endpoint

- meta:

  meta data to attach to the epidata call

- api_version:

  string. The API version to use. One of "classic" or "cast".

- response_format:

  string. The expected format of the response. One of "classic", "json",
  or "csv".

- epidata_call:

  an instance of `epidata_call`

- fetch_args:

  a `fetch_args` object

## Value

- For `create_epidata_call`: an `epidata_call` object

&nbsp;

- For `fetch`: a tibble

## Details

`create_epidata_call` is the constructor for `epidata_call` objects, but
you should not need to use it directly; instead, use an endpoint
function, e.g.,
[`pub_covidcast`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md),
to generate an `epidata_call` for the data of interest.

## Examples

``` r
# Build the request without sending it
call <- epidata_snapshot(
  source = "nssp",
  signals = "pct_ed_visits_influenza",
  geo_type = "state",
  geo_values = c("pa", "ca"),
  reference_time = epirange("2024-12-01", "2024-12-31"),
  fetch_args = fetch_args_list(dry_run = TRUE)
)
call
#> 
#> ── <epidata_call> object: ──────────────────────────────────────────────────────
#> • Pipe this object into `fetch()` to actually fetch the data
#> • Request URL:
#>   https://delphi.cmu.edu/epidata/v5/snapshot/?source=nssp&signal=pct_ed_visits_influenza&geo_type=state&geo_value=pa%2Cca&reference_times=2024-12-01%3A2024-12-31

# Send it and get the data
fetch(call)
#> # A tibble: 8 × 7
#>   signal report_time         geo_type geo_value fill_method reference_time value
#>   <chr>  <dttm>              <chr>    <chr>     <chr>       <date>         <dbl>
#> 1 pct_e… 2025-02-03 12:00:00 state    pa        source      2024-12-28     3.93 
#> 2 pct_e… 2025-01-17 12:00:00 state    pa        source      2024-12-07     0.570
#> 3 pct_e… 2025-01-24 12:00:00 state    pa        source      2024-12-21     1.81 
#> 4 pct_e… 2025-01-24 12:00:00 state    pa        source      2024-12-14     0.870
#> 5 pct_e… 2026-07-29 12:00:00 state    ca        source      2024-12-07     1.68 
#> 6 pct_e… 2026-08-05 12:00:00 state    ca        source      2024-12-14     2.55 
#> 7 pct_e… 2026-08-05 12:00:00 state    ca        source      2024-12-28     6.70 
#> 8 pct_e… 2026-08-05 12:00:00 state    ca        source      2024-12-21     4.48 
```
