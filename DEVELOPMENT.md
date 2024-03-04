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
First, there's a handy function that makes a github issue; for example, at the time of writing we were doing:
```R
usethis::use_release_issue(version = "1.0.2")
```
If you want to extend it, add to the `release_bullets` function in [utils.R](https://github.com/cmu-delphi/epidatr/blob/dev/R/utils.R).
 First, make sure that all the checks pass

```R
devtools::check(".", manual = TRUE, env_vars =c(NOT_CRAN = "false"))
```

Aim for 10/10, no notes. Generally, follow the issue. `revdep_check` is likely to fail but doesn't seem to be terribly important. So for now ignore it. Actually, the issue is off a bit: use the following instead:
Prepare for release:

* [ ] `git pull`
* [ ] Check [current CRAN check results](https://cran.rstudio.org/web/checks/check_results_epidatr.html)
* [ ] `devtools::check(remote = TRUE, manual = TRUE)`
* [ ] if check works well enough, merge to main. Otherwise open a PR to fix up.
* [ ] [Polish NEWS](https://github.com/cmu-delphi/epidatr/blob/dev/NEWS.md): some [Guidelines](https://style.tidyverse.org/news.html#news-release)
* [ ] `git checkout main`
* [ ] `git pull`
* [ ] `urlchecker::url_check()`. This may choke on the MIT license url, and that's ok.
* [ ] `devtools::build_readme()`
* [ ] `devtools::check_win_devel()`
* [ ] check email for problems
* [ ] `revdepcheck::revdep_check(num_workers = 4)`. This may choke, it is very sensitive to the binary versions of packages on a given system. Either bypass or ask someone else to run it if you're concerned.
* [ ] Update `cran-comments.md`
* [ ] PR with any changes

Submit to CRAN:

* [ ] `devtools::submit_cran()`
* [ ] Approve email

Wait for CRAN...

* [ ] Accepted :tada:
* [ ] `dev`
* [ ] `usethis::use_github_release(publish = FALSE)` (publish off, otherwise it won't push). 
* [ ] check the release notes and publish the branch on github
