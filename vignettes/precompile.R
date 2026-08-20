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

  # Knit from vignettes/ so figure paths (e.g. img/) resolve relative to it.
  owd <- setwd("vignettes")
  on.exit(setwd(owd))
  for (src in sources) {
    out <- sub("\\.orig$", "", src)
    message("Knitting ", src, " -> ", out)
    knitr::knit(src, output = out, envir = new.env(), quiet = TRUE)
  }
})
