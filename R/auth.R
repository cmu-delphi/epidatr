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
#' API keys are strings read from the environment variable `DELPHI_EPIDATA_KEY`.
#' We recommend setting your key with `save_api_key()`, which will modify an
#' applicable `.Renviron` file, which will be read in automatically when you
#' start future R sessions (see [`?Startup`][base::Startup] for details on
#' `.Renviron` files). Alternatively, you can modify the environment variable at
#' the command line before/while launching R, or inside an R session with
#' [`Sys.setenv()`], but these will not persist across sessions.
#'
#' Once an API key is set, it is automatically used for all requests made by
#' functions in this package.
#'
#' @return For `get_api_key()`, returns the current API key as a string, or
#'   `""` if none is set.
#'
#' @references
#' - [Delphi Epidata API Keys
#'   documentation](https://cmu-delphi.github.io/delphi-epidata/api/api_keys.html).
#' - [Delphi Epidata API Registration
#'   Form](https://api.delphi.cmu.edu/epidata/admin/registration_form).
#'
#' @export
get_api_key <- function() {
  key <- Sys.getenv("DELPHI_EPIDATA_KEY", unset = "")
  if (key != "") {
    return(key)
  }

  cli::cli_warn(
    c(
      "No API key found. You will be limited to non-complex queries and encounter rate limits if you proceed.",
      "i" = "See {.help save_api_key} for details on obtaining and setting API keys."
    ),
    .frequency = "regularly",
    .frequency_id = "delphi.epidata.key"
  )
  return("")
}

#' @rdname get_api_key
#' @export
save_api_key <- function() {
  cli::cli_inform(
    c(
      "i" = "This function will open your {.code .Renviron} file in a text editor. You will need to
      write {.code DELPHI_EPIDATA_KEY=yourkeyhere} (without quotes) in the file and save it. If the editor
      does not open, you will need to edit the file manually.",
      "i" = "Press enter to continue."
    )
  )
  readline()

  if (file.exists(usethis::proj_path(".Renviron"))) {
    usethis::edit_r_environ(scope = "project")

    cat("\n\n")
    cli::cli_inform(
      c(
        "i" = "Your project {.code .Renviron} file has been updated. Make sure not to share this
        file (add it to .gitignore or equivalents)."
      )
    )
  } else {
    usethis::edit_r_environ(scope = "user")
  }
}
