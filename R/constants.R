version <- "1.0.0"
http_headers <- httr::add_headers("User-Agent" = paste0("epidatr/", version))
global_base_url <- "https://delphi.cmu.edu/epidata/"
