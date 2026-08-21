install:
	Rscript -e "install.packages(c('pak', 'devtools', 'pkgdown', 'webshot', 'textshaping', 'styler', 'lintr'));devtools::install_deps(dependencies = TRUE);pak::local_install_deps(dependencies = 'Config/Needs/precompile')"
lint:
	Rscript -e "devtools::load_all();lintr::lint_package()"
format:
	Rscript -e "styler::style_pkg()"
test:
	Rscript -e "devtools::test()"
# Set pvt=FALSE to skip the pvt_* endpoints (they need a key with private access).
pvt ?= TRUE
test-live:
	EPIDATR_LIVE_TEST=TRUE EPIDATR_TEST_PVT=$(pvt) Rscript -e "devtools::test(filter = 'live')"
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
