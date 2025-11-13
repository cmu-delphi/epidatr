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
if (FALSE) { # \dontrun{
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
} # }
```
