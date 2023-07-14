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

## Development Environment

Relevant R commands

```r
install.packages(c('devtools', 'pkgdown', 'styler', 'lintr')) # install dev dependencies
devtools::install_deps(dependencies = TRUE) # install package dependencies
devtools::document() # generate package meta data and man files
devtools::build() # build package
styler::style_pkg() # format code
lintr::lint_package() # lint package
devtools::test() # test package
devtools::check() # check package for errors
```

## Release Process

The release consists of multiple steps which can be all done via the GitHub website:

1. Go to [create_release GitHub Action](https://github.com/cmu-delphi/epidatr/actions/workflows/create_release.yml) and click the `Run workflow` button. Enter the next version number or one of the magic keywords (patch, minor, major) and hit the green `Run workflow` button.
2. The action will prepare a new release and will end up with a new [Pull Request](https://github.com/cmu-delphi/epidatr/pulls)
3. Let the code owner review the PR and its changes and let the CI check whether everything builds successfully
4. Once approved and merged, another GitHub action job starts which automatically will
   1. create a git tag
   2. create another [Pull Request](https://github.com/cmu-delphi/epidatr/pulls) to merge the changes back to the `dev` branch
   3. create a [GitHub release](https://github.com/cmu-delphi/epidatr/releases) with automatically derived release notes
5. Release to CRAN

[mit-image]: https://img.shields.io/badge/License-MIT-yellow.svg
[mit-url]: https://opensource.org/licenses/MIT
[github-actions-image]: https://github.com/cmu-delphi/epidatr/workflows/ci/badge.svg
[github-actions-url]: https://github.com/cmu-delphi/epidatr/actions
