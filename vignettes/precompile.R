# Precompile vignettes. The .Rmd.orig files are the editable sources: they
# make live API calls, so they are knitted here (not at check time) into the
# static .Rmd files that R CMD build renders. Commit both. See DEVELOPMENT.md.
#
# Run from the package root, with the current package installed:
#   Rscript vignettes/precompile.R [vignette-name ...]
# With no arguments, all .Rmd.orig files are knitted.

local({
  stopifnot("run from the package root" = dir.exists("vignettes"))
  sources <- list.files("vignettes", pattern = "\\.Rmd\\.orig$")
  targets <- commandArgs(trailingOnly = TRUE)
  if (length(targets) > 0) {
    sources <- sources[sub("\\.Rmd\\.orig$", "", sources) %in% targets]
    stopifnot("no .Rmd.orig matched the given names" = length(sources) > 0)
  }

  # knitr::knit() defaults to error = TRUE, which would silently bake API
  # failures into the output; make unexpected errors abort instead. Chunks
  # demonstrating errors on purpose set error = TRUE locally.
  knitr::opts_chunk$set(error = FALSE)

  # Match pkgdown's figure settings (see pkgdown:::fig_save_args()) so the
  # committed PNGs look like ones the website would have generated itself.
  stopifnot("the ragg package is needed to knit figures" = requireNamespace("ragg", quietly = TRUE))
  knitr::opts_chunk$set(
    fig.width = 7.2916667, fig.asp = 0.618, dpi = 96,
    fig.retina = 2, dev = "ragg_png"
  )

  # Match the other knit-time settings pkgdown::build_article() applies.
  options(width = 80, knitr.graphics.rel_path = FALSE)

  # Knit from vignettes/ so figure paths (e.g. img/) resolve relative to it.
  owd <- setwd("vignettes")
  on.exit(setwd(owd))
  for (src in sources) {
    out <- sub("\\.orig$", "", src)
    message("Knitting ", src, " -> ", out)
    set.seed(1014) # pkgdown::build_article() seeds each article the same way
    knitr::knit(src, output = out, envir = new.env(), quiet = TRUE)
  }
})
