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
  only_supports_classic = FALSE
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

- only_supports_classic:

  if true only classic format is supported

- epidata_call:

  an instance of `epidata_call`

- fetch_args:

  a `fetch_args` object

## Value

- For `create_epidata_call`: an `epidata_call` object

&nbsp;

- For `fetch`: a tibble or a JSON-like list

## Details

`create_epidata_call` is the constructor for `epidata_call` objects, but
you should not need to use it directly; instead, use an endpoint
function, e.g.,
[`pub_covidcast`](https://cmu-delphi.github.io/epidatr/dev/reference/pub_covidcast.md),
to generate an `epidata_call` for the data of interest.

There are some other functions available for debugging and advanced
usage: - `request_url` (for debugging): outputs the request URL from
which data would be fetched (note additional parameters below)

`fetch` usually returns the data in tibble format, but a few of the
endpoints only support the JSON classic format (`pub_delphi`,
`pvt_meta_norostat`, and `pub_meta`). In that case a JSON-like nested
list structure is returned instead.

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
call %>% fetch()
#> Waiting 3s for retry backoff ■■■■■■■■■■■■                    
#> Waiting 3s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■       
#> Waiting 3s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  
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
