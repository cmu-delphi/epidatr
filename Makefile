install:
	Rscript -e "install.packages(c('devtools', 'pkgdown', 'webshot', 'textshaping', 'styler', 'lintr'));devtools::install_deps(dependencies = TRUE)"
lint:
	Rscript -e "devtools::load_all();lintr::lint_package()"
format:
	Rscript -e "styler::style_pkg()"
test:
	Rscript -e "devtools::test()"
document:
	Rscript -e "devtools::document()"
build: document
	Rscript -e "devtools::build()"
check:
	Rscript -e "devtools::check(args = c('--no-manual', '--as-cran'), error_on = 'warning')"
chores: format lint check
