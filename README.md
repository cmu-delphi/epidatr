
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
influenza, COVID-19, and other diseases from official government sources
such as the [Centers for Disease Control and Prevention
(CDC)](https://www.cdc.gov/) and [Google
Trends](https://console.cloud.google.com/marketplace/product/bigquery-public-datasets/covid19-search-trends),
and private partners such as [Meta](https://www.meta.com/about/) and [Change
Healthcare](https://business.optum.com/en/?src=chc). To learn more about
the Meta partnership, check out [this
article](https://delphi.cmu.edu/blog/2020/08/26/covid-19-symptom-surveys-through-facebook/).
It is built and maintained by the Carnegie Mellon University [Delphi
Research Group](https://delphi.cmu.edu/).

This package is designed to streamline the downloading and usage of data
from the Delphi Epidata API. It provides a simple R interface to the
API, including functions for downloading data, parsing the results, and
converting the data into a tidy format. The package can also fetch the
full historical record of a signal, including corrections and updates,
which is particularly useful for accurately backtesting forecasting
models. It can also fetch accessory data associated with a signal, such
as source-specific metadata, via `epidata_aux()`. We also provide
packages for downstream data processing
([epiprocess](https://github.com/cmu-delphi/epiprocess)) and modeling
([epipredict](https://github.com/cmu-delphi/epipredict)).

## Installation

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

Note that for the time being, any private endpoints (i.e. those prefixed
with `pvt`) will require a separate key that needs to be passed as an
argument.

## Usage

To get started with `epidatr`, see `vignette("epidatr")` or the [Getting
Started
guide](https://cmu-delphi.github.io/epidatr/articles/epidatr.html).

``` r
library(epidatr)
# Discover what a source offers: signals, geo types, and date ranges
meta <- epidata_meta(source = "nssp")
meta$signals
#> [1] "pct_ed_visits_ari"                "pct_ed_visits_combined"          
#> [3] "pct_ed_visits_covid"              "pct_ed_visits_influenza"         
#> [5] "pct_ed_visits_rsv"                "smoothed_pct_ed_visits_combined" 
#> [7] "smoothed_pct_ed_visits_covid"     "smoothed_pct_ed_visits_influenza"
#> [9] "smoothed_pct_ed_visits_rsv"

# Fetch the latest snapshot of NSSP influenza ED visit percentages by state
flu <- epidata_snapshot(
  source = "nssp",
  signals = "pct_ed_visits_influenza",
  geo_type = "state"
)
flu
#> # A tibble: 10,506 × 7
#>    signal        report_time geo_type geo_value fill_method reference_time value
#>    <chr>         <date>      <chr>    <chr>     <chr>       <date>         <dbl>
#>  1 pct_ed_visit… 2026-06-26  state    ak        source      2022-10-01     0.140
#>  2 pct_ed_visit… 2026-06-26  state    ak        source      2022-10-08     0.240
#>  3 pct_ed_visit… 2026-06-26  state    ak        source      2022-10-15     0.320
#>  4 pct_ed_visit… 2026-06-26  state    ak        source      2022-10-22     0.760
#>  5 pct_ed_visit… 2026-06-26  state    ak        source      2022-10-29     1.16 
#>  6 pct_ed_visit… 2026-06-26  state    ak        source      2022-11-05     1.94 
#>  7 pct_ed_visit… 2026-06-26  state    ak        source      2022-11-12     3.73 
#>  8 pct_ed_visit… 2026-06-26  state    ak        source      2022-11-19     6.82 
#>  9 pct_ed_visit… 2026-06-26  state    ak        source      2022-11-26     8.67 
#> 10 pct_ed_visit… 2026-06-26  state    ak        source      2022-12-03     9.85 
#> # ℹ 10,496 more rows
```

This is just a glimpse of what `epidatr` can do. See the
[Articles](https://cmu-delphi.github.io/epidatr/articles/) for
walkthroughs of specific tasks (finding signals, understanding versioned
data, migrating from `pub_covidcast()`), and the
[Reference](https://cmu-delphi.github.io/epidatr/reference/) for the
full list of functions and their arguments.

## Which endpoint has my data?

The Delphi Epidata API has three generations of endpoints, and this
package has client functions for all of them:

- **v5 (current):** `epidata_snapshot()`, `epidata_archive()`, and
  `epidata_meta()`. Start here; sources are moving to v5 one at a time.
- **v4 (covidcast):** `pub_covidcast()`. Still carries the sources that
  have not moved to v5 yet.
- **v3 (legacy):** the many other `pub_*` functions
  (e.g. `pub_fluview()`, `pub_gft()`), one per dataset. Most of these
  datasets are static or no longer updated; they remain available for
  historical work.

If you have existing `pub_covidcast()` code, see
`vignette("migration-guide")` for the argument and column mapping to the
v5 functions.

## Migrating from covidcast and to the V5 API

If you are migrating existing workflows, there are two transitions to
keep in mind:

- From the `covidcast` package to `epidatr`. The standalone [`covidcast`
  package](https://cmu-delphi.github.io/covidcast/covidcastR/) is
  deprecated and superseded by `epidatr`, which is a complete rewrite
  offering better speed, reliability, and broader endpoint support.
- From V3/V4 endpoints to the V5 API. Within `epidatr`,
  `pub_covidcast()` and other V3/V4 endpoints are being deprecated as of
  October 2026. See the [migration
  guide](https://cmu-delphi.github.io/epidatr/articles/migration-guide.html)
  (or `vignette("migration-guide")`). New code should use the current V5
  functions (`epidata_snapshot()`, `epidata_archive()`, and
  `epidata_meta()`), reserving `pub_covidcast()` only for sources that
  have not yet transitioned.

## Get updates

**You should consider subscribing to the [API mailing
list](https://lists.andrew.cmu.edu/mailman/listinfo/delphi-covidcast-api)**
to be notified of package updates, new data sources, corrections, and
more.

## Usage terms and citation

If you use `epidatr` or data from the Delphi Epidata API in your work,
please cite the package using
[`citation("epidatr")`](https://cmu-delphi.github.io/epidatr/authors.html#citation).
If you use data that originated from the COVIDcast project (whether
accessed via V5 endpoints or `pub_covidcast()`), please include the
[COVIDcast
citation](https://cmu-delphi.github.io/covidcast/covidcastR/authors.html#citation)
as well.

Certain data sources have specific attribution and licensing terms. See
the [Epidata data licensing
documentation](https://cmu-delphi.github.io/delphi-epidata/api/README.html#data-licensing)
and the [COVIDcast licensing
documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_licensing.html)
for information about citing specific datasets.

**Warning:** If you use data from the Epidata API to power a product,
dashboard, app, or other service, please download the data you need and
store it centrally rather than making API requests for every user. Our
server resources are limited and cannot support high-volume interactive
use.

See also the [Terms of
Use](https://delphi.cmu.edu/covidcast/terms-of-use/), noting that the
data is a research product and not warranted for a particular purpose.
