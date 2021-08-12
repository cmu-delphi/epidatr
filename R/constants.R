

version <- "1.0.0"
http_headers <- httr::add_headers("User-Agent" = paste0("delphi_epidata", version))
base_rul <- "https://delphi.cmu.edu/epidata/"
