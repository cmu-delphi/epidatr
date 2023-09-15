#' Get and set API keys
#'
#' Get and set the API key used to make requests to the Epidata API. Without a
#' key, requests may be subject to rate limits and other limitations.
#'
#' We recommend you register for an API key. While most endpoints are available
#' without one, there are [limits on API usage for anonymous
#' users](https://cmu-delphi.github.io/delphi-epidata/api/api_keys.html),
#' including a rate limit. If you regularly request large amounts of data,
#' please consider [registering for an API
#' key](https://api.delphi.cmu.edu/epidata/admin/registration_form).
#'
#' API keys are strings and can be set in two ways. If the environment variable
#' `DELPHI_EPIDATA_KEY` is set, it will be used automatically. Environment
#' variables can be set either in the shell or by editing your `.Renviron` file,
#' which will ensure the setting applies to all R sessions. See `?Startup` for a
#' description of `Renviron` files and where they can be placed.
#'
#' Alternately, the API key can be set from within a session by using
#' `set_api_key()`, which sets the R option `delphi.epidata.key` using
#' `options()`. If this option is set, it is used in preference to the
#' environment variable, so you may change keys within an R session. R options
#' are not preserved between sessions, so `set_api_key()` must be run every time
#' you open R. Alternately, you can have R set the option at startup by adding
#' it to your `.Rprofile`; see `?Startup` for a description of `Rprofile` files
#' and where they can be placed.
#'
#' Once an API key is set, it is automatically used for all requests made by
#' functions in this package.
#'
#' @return For `get_api_key()`, returns the current API key as a string, or
#'   `""` if none is set.
#'
#' @seealso [usethis::edit_r_environ()] to automatically edit the `.Renviron`
#'   file; [usethis::edit_r_profile()] to automatically edit the `.Rprofile`
#'   file
#'
#' @references Delphi Epidata API Keys documentation.
#'   <https://cmu-delphi.github.io/delphi-epidata/api/api_keys.html>
#'
#' Delphi Epidata API Registration Form.
#' <https://api.delphi.cmu.edu/epidata/admin/registration_form>
#' @export
get_api_key <- function() {
  key <- getOption("delphi.epidata.key", default = "")
  if (key != "") {
    return(key)
  }

  key <- Sys.getenv("DELPHI_EPIDATA_KEY", unset = "")
  if (key != "") {
    return(key)
  }

  cli::cli_warn(
    c(
      "No API key found. You will be limited to non-complex queries and encounter rate limits if you proceed.",
      "i" = "See {.help set_api_key} for details on obtaining and setting API keys."
    ),
    .frequency = "regularly",
    .frequency_id = "delphi.epidata.key"
  )
  return("")
}

#' @rdname get_api_key
#' @param key API key to use for future requests
#' @export
set_api_key <- function(key) {
  options(delphi.epidata.key = key)
}
