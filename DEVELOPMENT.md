## Setting up the development environment

```r
install.packages(c('devtools', 'pkgdown', 'styler', 'lintr')) # install dev dependencies
devtools::install_deps(dependencies = TRUE) # install package dependencies
devtools::document() # generate package meta data and man files
devtools::build() # build package
```

## Validating the package

```r
styler::style_pkg() # format code
lintr::lint_package() # lint code

devtools::test() # test package
devtools::check() # check package for errors
```

## Developing the documentation site

Our CI builds two version of the documentation:

- https://cmu-delphi.github.io/epidatr/ from the `main` branch and
- https://cmu-delphi.github.io/epidatr/dev from the `dev` branch.

The documentation site can be previewed locally by running in R:

```r
# Should automatically open a browser
pkgdown::build_site(preview=TRUE)
```

If the above does not open a browser, you can try using a Python server from the
command line:

```bash
R -e 'devtools::document()'
R -e 'pkgdown::build_site()'
python -m http.server -d docs
```

## Versioning

Please follow the guidelines in the [PR template document](.github/pull_request_template.md).

## Release process

TBD
