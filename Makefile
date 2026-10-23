install:
	Rscript -e "install.packages(c('pak', 'devtools', 'pkgdown', 'webshot', 'textshaping', 'styler', 'lintr'));devtools::install_deps(dependencies = TRUE);pak::local_install_deps(dependencies = 'Config/Needs/precompile')"
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
# Tag a commit, push the tag, and publish a GitHub release with generated notes.
# Defaults to the DESCRIPTION version at HEAD, e.g. `make release ref=origin/main draft=true`.
version ?= $(shell sed -n 's/^Version: //p' DESCRIPTION)
ref ?= HEAD
draft ?= false
.PHONY: release
release:
	git tag -a v$(version) $(ref) -m "epidatr $(version)"
	git push origin v$(version)
	gh release create v$(version) --verify-tag --generate-notes --title "epidatr $(version)" $(if $(filter true,$(draft)),--draft)
