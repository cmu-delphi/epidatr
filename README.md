# Delphi Epidata R client

[![License: MIT][mit-image]][mit-url] [![Github Actions][github-actions-image]][github-actions-url]
[![codecov](https://codecov.io/gh/dsweber2/epidatr/branch/dev/graph/badge.svg?token=jVHL9eHZNZ)](https://codecov.io/gh/dsweber2/epidatr)

This package provides an R client for the [Delphi Epidata
API](https://cmu-delphi.github.io/delphi-epidata/).

## Usage

```r
library(epidatr)
epicall <- covidcast("fb-survey", "smoothed_cli", "nation", "day", "us", epirange(20210405, 20210410))
epicall %>% fetch()
```

```
# A tibble: 6 × 15
  geo_value signal       source   geo_type time_type time_value
  <chr>     <chr>        <chr>    <fct>    <fct>     <date>    
1 us        smoothed_cli fb-surv… nation   day       2021-04-05
2 us        smoothed_cli fb-surv… nation   day       2021-04-06
3 us        smoothed_cli fb-surv… nation   day       2021-04-07
4 us        smoothed_cli fb-surv… nation   day       2021-04-08
5 us        smoothed_cli fb-surv… nation   day       2021-04-09
6 us        smoothed_cli fb-surv… nation   day       2021-04-10
# ℹ 9 more variables: direction <dbl>, issue <date>,
#   lag <int>, missing_value <int>, missing_stderr <int>,
#   missing_sample_size <int>, value <dbl>, stderr <dbl>,
#   sample_size <dbl>
```

## Installation

Install the latest version using [`pak`](https://cran.r-project.org/package=pak)
package

```R
pak::pkg_install("cmu-delphi/epidatr")
```

CRAN version coming soon.

### API Keys

The Delphi API requires a (free) API key for full functionality. To generate
your key, register for a pseudo-anonymous account
[here](https://api.delphi.cmu.edu/epidata/admin/registration_form) and see more
discussion on the [general API
website](https://cmu-delphi.github.io/delphi-epidata/api/api_keys.html). The
`epidatr` client will automatically look for this key in the R option
`delphi.epidata.key` and then from the environment variable
`DELPHI_EPIDATA_KEY`. We recommend storing your key in `.Renviron` file, which R
will read by default.

Note that for the time being, the private endpoints (i.e. those prefixed with
`pvt`) will require a separate key that needs to be passed as an argument.


[mit-image]: https://img.shields.io/badge/License-MIT-yellow.svg
[mit-url]: https://opensource.org/licenses/MIT
[github-actions-image]: https://github.com/cmu-delphi/epidatr/workflows/ci/badge.svg
[github-actions-url]: https://github.com/cmu-delphi/epidatr/actions