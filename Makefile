install:
	Rscript -e "install.packages(c('pak', 'devtools', 'pkgdown', 'webshot', 'textshaping', 'styler', 'lintr', 'urlchecker', 'foghorn'));devtools::install_deps(dependencies = TRUE);pak::local_install_deps(dependencies = 'Config/Needs/precompile')"
lint:
	Rscript -e "devtools::load_all();lintr::lint_package()"
format:
	Rscript -e "styler::style_pkg()"
test:
	Rscript -e "devtools::test()"
# Set pvt=TRUE to include pvt_* endpoints (requires a key with private access).
# Set cast_url to point cast-API tests at a non-default server, e.g.:
#   make test-live cast_url=https://development.delphi.cmu.edu/epidata/v5/
pvt ?= FALSE
cast_url ?=
test-live:
	EPIDATR_LIVE_TEST=TRUE EPIDATR_TEST_PVT=$(pvt) EPIDATR_CAST_BASE_URL=$(cast_url) Rscript -e "devtools::test(filter = 'live')"
test-live-cast:
	EPIDATR_LIVE_TEST=TRUE EPIDATR_CAST_BASE_URL=$(cast_url) Rscript -e "devtools::test(filter = 'live-cast')"
update-fixtures:
	Rscript data-raw/update_fixtures.R
coverage:
	Rscript -e "devtools::test_coverage()"
document:
	Rscript -e "devtools::document()"
# Re-knit vignettes/*.Rmd.orig (live API calls) into the committed static .Rmd
.PHONY: vignettes
vignettes:
	Rscript -e "devtools::install(quick = TRUE, upgrade = FALSE)"
	Rscript vignettes/precompile.R
build: document
	Rscript -e "devtools::build()"
check:
	Rscript -e "devtools::check(args = c('--no-manual', '--as-cran'), error_on = 'warning')"
chores: format lint check

# ---- Release ----
# The full checklist lives in DEVELOPMENT.md "Release process".
pkg_version = $(shell sed -n 's/^Version: //p' DESCRIPTION)
submitted_version = $(shell sed -n 's/^Version: //p' CRAN-SUBMISSION)
submitted_sha = $(shell sed -n 's/^SHA: //p' CRAN-SUBMISSION)
# Looked up at most once per make run, and only by targets that use it.
cran_version = $(eval cran_version := $(shell Rscript --vanilla -e 'cat(available.packages(repos = "https://cloud.r-project.org")["epidatr", "Version"])' 2>/dev/null))$(cran_version)

# Bump the DESCRIPTION version, e.g. `make bump level=minor`.
level ?= patch
.PHONY: bump
bump:
	.github/scripts/bump-version.sh $(level)

# Compare the local, last-submitted, and published versions, and summarize CRAN checks.
.PHONY: cran-status
cran-status:
	@echo "DESCRIPTION:     $(pkg_version)"
	@echo "CRAN-SUBMISSION: $(submitted_version) ($(submitted_sha))"
	@echo "On CRAN:         $(cran_version)"
	@Rscript -e "print(foghorn::cran_results(pkg = 'epidatr'))"
	@echo "Details: https://cran.r-project.org/web/checks/check_results_epidatr.html"

# Fail fast on the release mistakes we have made before.
.PHONY: release-preflight
release-preflight:
	@test -n "$(cran_version)" || { echo "Could not look up the epidatr version on CRAN."; exit 1; }
	@test -z "$$(git status --porcelain)" || { echo "Working tree is not clean; the submitted SHA would not match the tarball."; git status --short; exit 1; }
	@! grep -q '^Remotes:' DESCRIPTION || { echo "Remove Remotes from DESCRIPTION; CRAN does not support it."; exit 1; }
	@grep -q '^# epidatr $(pkg_version)$$' NEWS.md || { echo "NEWS.md has no '# epidatr $(pkg_version)' heading."; exit 1; }
	@test "$(submitted_version)" = "$(cran_version)" || { echo "CRAN-SUBMISSION ($(submitted_version)) does not match CRAN ($(cran_version)); fix it with 'make cran-submission'."; exit 1; }
	@test "$(pkg_version)" != "$(cran_version)" || { echo "DESCRIPTION version $(pkg_version) is already on CRAN; run 'make bump'."; exit 1; }
	@echo "Preflight OK for epidatr $(pkg_version)."

# CRAN-like local check, including incoming feasibility checks and the PDF manual.
check-cran:
	Rscript -e "devtools::check(manual = TRUE, remote = TRUE, incoming = TRUE, env_vars = c(NOT_CRAN = 'false'), error_on = 'warning')"
urlcheck:
	Rscript -e "urlchecker::url_check()"
readme:
	Rscript -e "devtools::build_readme()"
# Everything local that should pass before submitting.
.PHONY: release-check
release-check: release-preflight document urlcheck readme check-cran

# Remote checks; results are emailed to the maintainer.
check-win:
	Rscript -e "devtools::check_win_devel(quiet = TRUE); devtools::check_win_release(quiet = TRUE)"
check-mac:
	Rscript -e "devtools::check_mac_release(quiet = TRUE)"
# Run the full OS/R-version matrix on GitHub Actions, e.g. `make check-full-ci branch=dev`.
branch ?= main
check-full-ci:
	gh workflow run R-CMD-check-full.yaml --ref $(branch)

# Submit from the tip of main. submit_cran() asks confirmation questions, so R runs interactively.
# It records the submitted version and SHA in CRAN-SUBMISSION, which must be committed.
.PHONY: submit
submit: release-preflight
	git fetch origin main
	@test "$$(git rev-parse HEAD)" = "$$(git rev-parse origin/main)" || { echo "HEAD is not origin/main; submit from the tip of main."; exit 1; }
	R --interactive --no-save --no-restore -q -e "devtools::submit_cran()"
	@echo "Now commit CRAN-SUBMISSION and include it in the main -> dev back-merge PR."

# Rewrite CRAN-SUBMISSION by hand, e.g. if a submission was made without `make submit`.
# Defaults to the DESCRIPTION version at HEAD, e.g. `make cran-submission ref=v1.4.0 version=1.4.0`.
version ?= $(pkg_version)
ref ?= HEAD
.PHONY: cran-submission
cran-submission:
	@printf 'Version: %s\nDate: %s\nSHA: %s\n' "$(version)" "$$(date -u '+%Y-%m-%d %H:%M:%S UTC')" "$$(git rev-parse $(ref)^{commit})" > CRAN-SUBMISSION
	@cat CRAN-SUBMISSION

# After CRAN accepts: tag the submitted commit, push the tag, and publish a GitHub release.
# Defaults to the version and SHA in CRAN-SUBMISSION, e.g. `make release draft=true`.
draft ?= false
.PHONY: release
release:
	@test -n "$(cran_version)" || { echo "Could not look up the epidatr version on CRAN."; exit 1; }
	@test "$(submitted_version)" = "$(cran_version)" || { echo "CRAN-SUBMISSION ($(submitted_version)) is not the version on CRAN ($(cran_version)); has it been accepted?"; exit 1; }
	git tag -a v$(submitted_version) $(submitted_sha) -m "epidatr $(submitted_version)"
	git push origin v$(submitted_version)
	gh release create v$(submitted_version) --verify-tag --generate-notes --title "epidatr $(submitted_version)" $(if $(filter true,$(draft)),--draft)

# Open the PR that brings release fixes and CRAN-SUBMISSION from main back to dev.
.PHONY: backmerge
backmerge:
	gh pr create --base dev --head main --title "chore: merge main into dev after epidatr $(submitted_version)" --body "Back-merge after the $(submitted_version) CRAN release."
