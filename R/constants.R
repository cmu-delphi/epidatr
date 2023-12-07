version <- "1.0.0.9000"
http_headers <- httr::add_headers("User-Agent" = paste0("epidatr/", version), "Accept-Encoding" = "gzip")
global_base_url <- "https://api.delphi.cmu.edu/epidata/"
