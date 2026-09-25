## Setting up the development environment

```r
pak::pkg_install(c('devtools', 'pkgdown', 'styler', 'lintr')) # install dev dependencies
devtools::install_deps(dependencies = TRUE) # install package dependencies
pak::local_install_deps(dependencies = "Config/Needs/precompile") # vignette-knitting deps
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

## Vignettes are precompiled

The editable vignette sources are `vignettes/*.Rmd.orig`. They make live API
calls, so they are knitted ahead of time into the static `vignettes/*.Rmd`
files that `R CMD build`, CI, and CRAN render without any network access.
Both files are committed.

To edit a vignette, change its `.Rmd.orig` and re-knit. This requires network,
the current package code installed, and the knitting-only packages listed in
the `Config/Needs/precompile` field of DESCRIPTION (they are deliberately not
Suggests, so CI never installs them):

```bash
make vignettes                                # all vignettes
Rscript vignettes/precompile.R signal-discovery # just one
```

Review the resulting `.Rmd` diff — it contains the real API output, so it
also acts as a snapshot of API behavior. Re-knit whenever a `.Rmd.orig`, the
package's output formatting, or relevant API behavior changes.

Because fetched output is committed and published, chunks calling private
endpoints (`pvt_*`) or anything requiring restricted access must stay
`eval = FALSE` in the `.Rmd.orig` sources. Prefer knitting without
`DELPHI_EPIDATA_KEY` set so a private call cannot succeed by accident.

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

`DESCRIPTION` versions follow `major.minor.patch`. The CRAN version is always
the latest release, and `dev` is always ahead of it.

- You don't need to bump the version in your PR. After each merge into `dev`,
  the `version-bump` workflow opens and immediately merges a bot PR with a patch
  bump (e.g. 1.4.1 -> 1.4.2).
- Label the PR `version:minor` or `version:major` to get a bigger bump
  instead. Release prep for a new minor version is the usual case.
- If the merged PR already changed `Version`, the workflow leaves it alone, so a
  hand-set version always wins. `make bump level=minor` does the same edit
  locally.
- Write NEWS entries under a `# epidatr <version>` heading that matches the
  version you are releasing. `make release-preflight` checks this.

See also the [PR template](.github/pull_request_template.md).

## Release process

Most steps are Makefile targets. Run `make install` once to get the tools they
use. Open a release issue and copy this checklist into it:

Prepare, on `dev`:

- [ ] `make cran-status`: compare the `DESCRIPTION`, `CRAN-SUBMISSION`, and
  on-CRAN versions, and check the current CRAN check results for problems to fix.
- [ ] Set the release version (`make bump level=minor`, or merge a PR labelled
  `version:minor`) and [polish NEWS](https://style.tidyverse.org/news.html#news-release).
- [ ] Update `cran-comments.md`.
- [ ] `make release-check`: preflight, `document`, URL check, README rebuild,
  and a CRAN-like `R CMD check` with incoming checks. Aim for 0 errors, 0
  warnings, and 0 notes. Fix problems in PRs to `dev`.
- [ ] `make check-full-ci branch=dev` to run the full OS/R-version matrix.
- [ ] Open the release PR from `dev` into `main` and merge it.

Submit, from the tip of `main` with a clean working tree:

- [ ] `make check-win` (and optionally `make check-mac`). The maintainer
  ("cre" in `DESCRIPTION`) gets the results by email.
- [ ] `make submit`. It reruns the preflight, refuses to run anywhere except
  `origin/main`, and asks confirmation questions. Afterwards it writes the
  submitted version and SHA to `CRAN-SUBMISSION`.
- [ ] The maintainer confirms the submission from the CRAN email.

Wait for CRAN. If CRAN rejects the package, fix the problems through `dev` and
then `main`, and submit again.

After acceptance:

- [ ] Commit the updated `CRAN-SUBMISSION` on a branch off `main`.
- [ ] `make backmerge` opens the `main` -> `dev` PR. Merge it.
- [ ] `make release draft=true` tags the commit recorded in `CRAN-SUBMISSION`,
  pushes the tag, and creates a draft GitHub release. It refuses to run until
  that version is on CRAN.
- [ ] Review the release notes on GitHub and publish them.

If a submission was made without `make submit`, fix `CRAN-SUBMISSION` by hand
with `make cran-submission version=X.Y.Z ref=<submitted commit>`.
`make release-preflight` fails while the file is out of date.
