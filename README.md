
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Delphi Epidata R client

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/license/mit)
[![Github
Actions](https://github.com/cmu-delphi/epidatr/workflows/ci/badge.svg)](https://github.com/cmu-delphi/epidatr/actions)
[![codecov](https://codecov.io/gh/dsweber2/epidatr/branch/dev/graph/badge.svg?token=jVHL9eHZNZ)](https://app.codecov.io/gh/dsweber2/epidatr)
<!-- badges: end -->

The [Delphi Epidata API](https://cmu-delphi.github.io/delphi-epidata/)
provides real-time access to epidemiological surveillance data for
influenza, COVID-19, and other diseases from both official government
sources such as the [Centers for Disease Control and Prevention
(CDC)](https://data.cdc.gov/), private partners
such as [Facebook (now
Meta)](https://delphi.cmu.edu/blog/2020/08/26/covid-19-symptom-surveys-through-facebook/)
and [Change Healthcare](https://www.changehealthcare.com/), and other
public datasets like [Google
Trends](https://console.cloud.google.com/marketplace/product/bigquery-public-datasets/covid19-search-trends).
It is built and maintained by the Carnegie Mellon University [Delphi
Research Group](https://delphi.cmu.edu/).

This package is designed to streamline the downloading and usage of data
from the Delphi Epidata API. It provides a simple R interface to the
API, including functions for downloading data, parsing the results, and
converting the data into a tidy format. The API stores a historical
record of all data, including corrections and updates, which is
particularly useful for accurately backtesting forecasting models. We
also provide packages for downstream data processing
([epiprocess](https://github.com/cmu-delphi/epiprocess)) and modeling
([epipredict](https://github.com/cmu-delphi/epipredict)).

## Usage

``` r
library(epidatr)
# Obtain the smoothed covid-like illness (CLI) signal from Delphi's US COVID-19
# Trends and Impact Survey (CTIS), in partnership with Facebook, as it was on
# April 10, 2021 for the US at the national level
epidata <- pub_covidcast(
  source = "fb-survey",
  signals = "smoothed_cli",
  geo_type = "nation",
  time_type = "day",
  geo_values = "us",
  time_values = epirange(20210101, 20210601),
  as_of = 20210601
)
epidata
#> # A tibble: 151 × 15
#>    geo_value signal    source geo_type time_type time_value direction issue     
#>    <chr>     <chr>     <chr>  <fct>    <fct>     <date>         <dbl> <date>    
#>  1 us        smoothed… fb-su… nation   day       2021-01-01        NA 2021-01-06
#>  2 us        smoothed… fb-su… nation   day       2021-01-02        NA 2021-01-07
#>  3 us        smoothed… fb-su… nation   day       2021-01-03        NA 2021-01-08
#>  4 us        smoothed… fb-su… nation   day       2021-01-04        NA 2021-01-09
#>  5 us        smoothed… fb-su… nation   day       2021-01-05        NA 2021-01-10
#>  6 us        smoothed… fb-su… nation   day       2021-01-06        NA 2021-01-29
#>  7 us        smoothed… fb-su… nation   day       2021-01-07        NA 2021-01-29
#>  8 us        smoothed… fb-su… nation   day       2021-01-08        NA 2021-01-29
#>  9 us        smoothed… fb-su… nation   day       2021-01-09        NA 2021-01-29
#> 10 us        smoothed… fb-su… nation   day       2021-01-10        NA 2021-01-29
#> # ℹ 141 more rows
#> # ℹ 7 more variables: lag <dbl>, missing_value <dbl>, missing_stderr <dbl>,
#> #   missing_sample_size <dbl>, value <dbl>, stderr <dbl>, sample_size <dbl>
```

## Installation

Installing the package is straightforward.

``` r
# Install the CRAN version
pak::pkg_install("epidatr")
# Install the development version from the GitHub dev branch
pak::pkg_install("cmu-delphi/epidatr@dev")
```

Our CRAN listing is
[here](https://CRAN.R-project.org/package=epidatr/index.html).

### API Keys

The Delphi API requires a (free) API key for full functionality. To
generate your key, register for a pseudo-anonymous account
[here](https://api.delphi.cmu.edu/epidata/admin/registration_form) and
see more discussion on the [general API
website](https://cmu-delphi.github.io/delphi-epidata/api/api_keys.html).
The `epidatr` client will automatically look for this key in the
environment variable `DELPHI_EPIDATA_KEY`. We recommend storing your key
in your `.Renviron` file, which R will read by default.

Note that for the time being, the private endpoints (i.e. those prefixed
with `pvt`) will require a separate key that needs to be passed as an
argument.

## For users of the covidcast R package

The `covidcast` package is deprecated and will no longer be updated. The
`epidatr` package is a complete rewrite of the [`covidcast`
package](https://cmu-delphi.github.io/covidcast/covidcastR/), with a
focus on speed, reliability, and ease of use. It also supports more
endpoints and data sources than `covidcast`. When migrating from that
package, you will need to use the
[`pub_covidcast`](https://cmu-delphi.github.io/epidatr/reference/pub_covidcast.html)
function in `epidatr`.

## Get updates

**You should consider subscribing to the [API mailing
list](https://lists.andrew.cmu.edu/mailman/listinfo/delphi-covidcast-api)**
to be notified of package updates, new data sources, corrections, and
other updates.

## Usage terms and citation

We request that if you use the `epidatr` package in your work, or use
any of the data provided by the Delphi Epidata API through
non-`covidcast` endpoints, that you cite us using the citation given by
[`citation("epidatr")`](https://cmu-delphi.github.io/epidatr/dev/authors.html#citation).
If you use any of the data from the `covidcast` endpoint, please use the
[COVIDcast
citation](https://cmu-delphi.github.io/covidcast/covidcastR/authors.html#citation)
as well. See the [COVIDcast licensing
documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_licensing.html)
and the [licensing documentation for other
endpoints](https://cmu-delphi.github.io/delphi-epidata/api/README.html#data-licensing)
for information about citing the datasets provided by the API.

**Warning:** If you use data from the Epidata API to power a product,
dashboard, app, or other service, please download the data you need and
store it centrally rather than making API requests for every user. Our
server resources are limited and cannot support high-volume interactive
use.

See also the [Terms of
Use](https://delphi.cmu.edu/covidcast/terms-of-use/), noting that the
data is a research product and not warranted for a particular purpose.
