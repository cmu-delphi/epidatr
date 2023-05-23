# delphi Epidata R client

[![License: MIT][mit-image]][mit-url] [![Github Actions][github-actions-image]][github-actions-url]

## Documentation

- [Package website](https://cmu-delphi.github.io/epidatr/)

## Install

Install latest version using [`remotes`](https://cran.r-project.org/package=remotes) package

```R
remotes::install_github("cmu-delphi/epidatr")
```

Note (2023-05-23): since version 0.6.0, the `fetch_{tbl,classic,df,json,csv}`
functions have been replaced by the `fetch()` function, which almost always
returns a tibble, except when used with a limited number of older endpoints
(such as `delphi()` and `meta()`), where it will output a nested list structure.
If you encounter an error like
```{r}
Error in fetch_tbl(.) : could not find function "fetch_tbl"
```
please update to use `fetch()` instead.  If instead you encounter
```{r}
Error in fetch(.) : could not find function "fetch"
```
please update to the newest `epidatr`.

Note (2022-08-02): the package that this installs is being renamed from
`delphi.epidata` to `epidatr`. To migrate, run the installation command above,
followed by `remove.packages("delphi.epidata")`, and adjust all references to
the package name accordingly. For a short-term patch, a snapshot of the package
can be still be installed with the old name by specifying
`ref="delphi.epidata-before-rename"`.

## Usage

TODO

## API Keys

The Delphi API will soon require a (free) API key for full functionality.
The `epidatr` client will automatically look for this key in the R option `delphi.epidata.key`
and then from the environment variable `DELPHI_EPIDATA_KEY`.
We recommend storing your key in `.Renviron` file, which R will read by default.
You can do this with `usethis::edit_r_environ()` and there enter `DELPHI_EPIDATA_KEY=<your-key>`.
An alternative is [the dotenv package](https://github.com/gaborcsardi/dotenv/).

Note that for the time being, the private endpoints (i.e. those prefixed with `pvt`) will require
a separate key that needs to be passed as an argument.

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
1. The action will prepare a new release and will end up with a new [Pull Request](https://github.com/cmu-delphi/epidatr/pulls)
1. Let the code owner review the PR and its changes and let the CI check whether everything builds successfully
1. Once approved and merged, another GitHub action job starts which automatically will
   1. create a git tag
   1. create another [Pull Request](https://github.com/cmu-delphi/epidatr/pulls) to merge the changes back to the `dev` branch
   1. create a [GitHub release](https://github.com/cmu-delphi/epidatr/releases) with automatically derived release notes
1. Done

TODO release to CRAN

[mit-image]: https://img.shields.io/badge/License-MIT-yellow.svg
[mit-url]: https://opensource.org/licenses/MIT
[github-actions-image]: https://github.com/cmu-delphi/epidatr/workflows/ci/badge.svg
[github-actions-url]: https://github.com/cmu-delphi/epidatr/actions
