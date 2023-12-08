
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Delphi Epidata R client

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/license/mit/)
[![Github
Actions](https://github.com/cmu-delphi/epidatr/workflows/ci/badge.svg)](https://github.com/cmu-delphi/epidatr/actions)
[![codecov](https://codecov.io/gh/dsweber2/epidatr/branch/dev/graph/badge.svg?token=jVHL9eHZNZ)](https://app.codecov.io/gh/dsweber2/epidatr)
<!-- badges: end -->

The [Delphi `epidatr` package](https://cmu-delphi.github.io/epidatr/) is
an R front-end for the [Delphi Epidata
API](https://cmu-delphi.github.io/delphi-epidata/), which provides
real-time access to epidemiological surveillance data for influenza,
COVID-19, and other diseases. `epidatr` is built and maintained by the
Carnegie Mellon University [Delphi research
group](https://delphi.cmu.edu/).

Data is available for the United States and a handful of other countries
at various geographical resolutions, both from official government
sources such as the [US Center for Disease Control
(CDC)](https://www.cdc.gov/datastatistics/index.html), and private
partners such as
[Facebook](https://delphi.cmu.edu/blog/2020/08/26/covid-19-symptom-surveys-through-facebook/)
and [Change Healthcare](https://www.changehealthcare.com/). The API
stores a historical record of all data, including corrections and
updates, which is particularly useful for accurately backtesting
forecasting models.

`epidatr` is designed to streamline the downloading and usage of data
from the Epidata API. The package provides a simple R interface to the
API, including functions for downloading data, parsing the results, and
converting the data into a tidy format. We also provide the
[epiprocess](https://github.com/cmu-delphi/epiprocess) package for
downstream data processing and
[epipredict](https://github.com/cmu-delphi/epipredict) for modeling.

Consult the [Epidata API
documentation](https://cmu-delphi.github.io/delphi-epidata/) for details
on the data included in the API, API key registration, licensing, and
how to cite this data in your work. The documentation lists all the data
sources and signals available through this API for
[COVID-19](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html)
and for [other
diseases](https://cmu-delphi.github.io/delphi-epidata/api/README.html#source-specific-parameters).

**To get started** using this package, view the Getting Started guide at
`vignette("epidatr")`.

## Get updates

**You should consider subscribing to the [API mailing
list](https://lists.andrew.cmu.edu/mailman/listinfo/delphi-covidcast-api)**
to be notified of package updates, new data sources, corrections, and
other updates.

## For users of the `covidcast` R package

`epidatr` is a complete rewrite of the [`covidcast`
package](https://cmu-delphi.github.io/covidcast/covidcastR/), with a
focus on speed, reliability, and ease of use. The `covidcast` package is
deprecated and will no longer be updated.

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
