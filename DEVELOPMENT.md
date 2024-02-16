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

Our CI is setup to build the [main documentation site](https://cmu-delphi.github.io/epidatr/) off of the `main` branch, while the [`dev` version of the site](https://cmu-delphi.github.io/epidatr/dev) is built off the `dev` branch.

The documentation site can be previewed locally by running in R:

```r
# Should automatically open a browser
pkgdown::build_site(preview=TRUE)
```

If the above does not open a browser, you can try using a Python server from the command line:

```bash
R -e 'devtools::document()'
R -e 'pkgdown::build_site()'
python -m http.server -d docs
```

## Versioning

Please follow the guidelines in the PR template document (reproduced here):

- [ ] Make sure this PR is against "dev", not "main".
- [ ] Request a review from one of the current epiprocess main reviewers:
      brookslogan, nmdefries.
- [ ] Makes sure to bump the version number in `DESCRIPTION` and `NEWS.md`.
      Always increment the patch version number (the third number), unless you are
      making a release PR from dev to main, in which case increment the minor
      version number (the second number).
- [ ] Describe changes made in NEWS.md, making sure breaking changes
      (backwards-incompatible changes to the documented interface) are noted.
      Collect the changes under the next release number (e.g. if you are on
      0.7.2, then write your changes under the 0.8 heading).

## Release process

TBD
