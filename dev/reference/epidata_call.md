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
library(magrittr)

call <- pub_covidcast(
  source = "jhu-csse",
  signals = "confirmed_7dav_incidence_prop",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20200601, 20200801),
  geo_values = c("ca", "fl"),
  fetch_args = fetch_args_list(dry_run = TRUE)
)
#> Warning: `pub_covidcast()` uses the V4 Epidata API.
#> ℹ Starting in October 2026, V4 is tentatively deprecated in favor of the V5
#>   API.
#> ℹ See `vignette("migration-guide")` (or
#>   <https://cmu-delphi.github.io/epidatr/articles/migration-guide.html>) for the
#>   V5 endpoints and how to move to them. Old data will remain available for at
#>   least a year, but new ingestion will end.
#> This warning is displayed once every 8 hours.
call %>% fetch()
#> # A tibble: 124 × 15
#>    geo_value signal    source geo_type time_type time_value direction issue     
#>    <chr>     <chr>     <chr>  <fct>    <fct>     <date>         <dbl> <date>    
#>  1 ca        confirme… jhu-c… state    day       2020-06-01        NA 2023-03-10
#>  2 fl        confirme… jhu-c… state    day       2020-06-01        NA 2023-03-03
#>  3 ca        confirme… jhu-c… state    day       2020-06-02        NA 2023-03-10
#>  4 fl        confirme… jhu-c… state    day       2020-06-02        NA 2023-03-03
#>  5 ca        confirme… jhu-c… state    day       2020-06-03        NA 2023-03-10
#>  6 fl        confirme… jhu-c… state    day       2020-06-03        NA 2023-03-03
#>  7 ca        confirme… jhu-c… state    day       2020-06-04        NA 2023-03-10
#>  8 fl        confirme… jhu-c… state    day       2020-06-04        NA 2023-03-03
#>  9 ca        confirme… jhu-c… state    day       2020-06-05        NA 2023-03-10
#> 10 fl        confirme… jhu-c… state    day       2020-06-05        NA 2023-03-03
#> # ℹ 114 more rows
#> # ℹ 7 more variables: lag <dbl>, missing_value <dbl>, missing_stderr <dbl>,
#> #   missing_sample_size <dbl>, value <dbl>, stderr <dbl>, sample_size <dbl>
```
