#' @title Get the API key
#' @description Get the API key from the environment variable DELPHI_API_KEY or from ~/.config/delphi_epidata/key
#' @return The API key
#' @export
get_auth_key <- function() {
    if (nchar(Sys.getenv("DELPHI_API_KEY")) == 0) {
        if (file.exists("~/.config/delphi_epidata/key")) {
            print("Using key from ~/.config/delphi_epidata/key")
            key <- readLines("~/.config/delphi_epidata/key")
            Sys.setenv(DELPHI_API_KEY = key)
            return(key)
        } else {
            warning("No key found. Please set one with set_auth_key() to avoid rate limits.")
            return("")
        }
        return(Sys.getenv("DELPHI_API_KEY"))
    } else {
        return(Sys.getenv("DELPHI_API_KEY"))
    }
}

#' @title Set the API key
#' @description Set the API key in the environment variable DELPHI_API_KEY and (optionally) in
#'   ~/.config/delphi_epidata/key
#' @export
set_auth_key <- function() {
    key <- readline("Enter your key: ")
    confirmation <- readline("Do you want to store the key in ~/.config/delphi_epidata/key? [y/n] ")
    if (confirmation != "y") {
        stop("Key not saved.")
    } else {
        dir.create("~/.config/delphi_epidata", showWarnings = FALSE, recursive = TRUE)
        print("Key saved to ~/.config/delphi_epidata/key")
        writeLines(key, "~/.config/delphi_epidata/key")
    }
    Sys.setenv(DELPHI_API_KEY = key)
}
