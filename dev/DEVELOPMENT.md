# NA

## Setting up the development environment

``` r

pak::pkg_install(c('devtools', 'pkgdown', 'lintr')) # install dev dependencies
devtools::install_deps(dependencies = TRUE) # install package dependencies
pak::local_install_deps(dependencies = "Config/Needs/precompile") # vignette-knitting deps
devtools::document() # generate package meta data and man files
devtools::build() # build package
```

## Validating the package

``` r

# Format code with `just format` (runs `air format .`; needs the air CLI: https://posit-dev.github.io/air/)
lintr::lint_package() # lint code

devtools::test() # test package
devtools::check() # check package for errors
```

## Vignettes are precompiled

The editable vignette sources are `vignettes/*.Rmd.orig`. They make live
API calls, so they are knitted ahead of time into the static
`vignettes/*.Rmd` files that `R CMD build`, CI, and CRAN render without
any network access. Both files are committed.

To edit a vignette, change its `.Rmd.orig` and re-knit. This requires
network, the current package code installed, and the knitting-only
packages listed in the `Config/Needs/precompile` field of DESCRIPTION
(they are deliberately not Suggests, so CI never installs them):

``` bash
just vignettes                  # all vignettes
just vignettes signal-discovery # only this one
```

Review the resulting `.Rmd` diff — it contains the real API output, so
it also acts as a snapshot of API behavior. Re-knit whenever a
`.Rmd.orig`, the package’s output formatting, or relevant API behavior
changes.

Because fetched output is committed and published, chunks calling
private endpoints (`pvt_*`) or anything requiring restricted access must
stay `eval = FALSE` in the `.Rmd.orig` sources. Prefer knitting without
`DELPHI_EPIDATA_KEY` set so a private call cannot succeed by accident.

## Developing the documentation site

Our CI builds two versions of the documentation:

- <https://cmu-delphi.github.io/epidatr/> when a GitHub release is
  published, and
- <https://cmu-delphi.github.io/epidatr/dev> on every push to the `dev`
  branch.

The documentation site can be previewed locally by running in R:

``` r

# Should automatically open a browser
pkgdown::build_site(preview=TRUE)
```

If the above does not open a browser, you can try using a Python server
from the command line:

``` bash
R -e 'devtools::document()'
R -e 'pkgdown::build_site()'
python -m http.server -d docs
```

## Versioning and release notes

`DESCRIPTION` versions follow `major.minor.patch`. The CRAN version is
always the latest release, and `dev` is always ahead of it.

Ordinary PRs touch neither the version nor `NEWS.md`. Write a
conventional-commit title, label the PR (`enhancement`, `bug`,
`documentation`, …) so it is categorized in the generated notes, and
describe what changed for users, including breaking changes: those are
the raw material for the release notes. The release PR (below) bumps the
version by hand (`just bump patch|minor|major`) and writes the
`# epidatr X.Y.Z` section of `NEWS.md`.

## Release process

Most steps are [just](https://just.systems) recipes (run `just` to list
them). Run `just install` once to get the tools they use. Open a release
issue and copy this checklist into it:

`dev` is the only long-lived branch; there is no `main`. Everything
below happens on `dev`.

Prepare:

`just cran-status`: compare the `DESCRIPTION`, `CRAN-SUBMISSION`, and
on-CRAN versions, and check the current CRAN check results for problems
to fix.

In a PR to `dev`, set the release version
(`just bump patch|minor|major`) and write the `# epidatr X.Y.Z` section
at the top of `NEWS.md`, replacing any `(development version)` heading.
Run `just news` to print GitHub’s generated notes since the last tag
(categories come from `.github/release.yml`) and give them to an agent,
or edit them yourself, into the [NEWS
style](https://style.tidyverse.org/news.html#news-release): sections
`Breaking changes`, `Changes`, `Documentation` and `Patches`, no
housekeeping, and a note on what users must do about breaking changes.
Review it like any other PR. The heading must be exactly
`# epidatr X.Y.Z`.

Update `cran-comments.md`.

`just release-check`: preflight, `document`, URL check, README rebuild,
and a CRAN-like `R CMD check` with incoming checks. Aim for 0 errors, 0
warnings, and 0 notes. Fix problems in PRs to `dev`.

`just check-full-ci dev` to run the full OS/R-version matrix.

Check reverse dependencies (e.g. `epiprocess`, `epipredict`) against
`dev` with [`r-devel/recheck`](https://github.com/r-devel/recheck) and
fix any regressions. CRAN also runs this on submission.

Merge the release PR (version bump and NEWS) into `dev`.

Submit, from the tip of `dev` with a clean working tree:

`just check-win` (and optionally `just check-mac`). The maintainer
(“cre” in `DESCRIPTION`) gets the results by email.

`just submit`. It reruns the preflight, refuses to run anywhere except
`origin/dev`, and asks confirmation questions. After a successful upload
it writes the version and SHA to `CRAN-SUBMISSION` and leaves it
uncommitted.

The maintainer confirms the submission from the CRAN email.

Wait for CRAN. If CRAN rejects the package, discard the
`CRAN-SUBMISSION` change (`git checkout CRAN-SUBMISSION`), fix the
problems through `dev`, and submit again.

After acceptance:

Commit the updated `CRAN-SUBMISSION` on a branch off `dev`, and open and
merge a PR for it into `dev`.

`just release --draft` tags the commit recorded in `CRAN-SUBMISSION`,
pushes the tag, and creates a draft GitHub release whose body is that
version’s `NEWS.md` section. It refuses to run until that version is on
CRAN.

Review the release notes on GitHub and publish them. Publishing rebuilds
the root documentation site from the release tag.

If a submission was made without `just submit`, fix `CRAN-SUBMISSION` by
hand with `just cran-submission X.Y.Z <submitted commit>`.
`just cran-status` shows whether it matches CRAN.
