

create_epidata_call <- function(endpoint, params) {
    stopifnot(is.character(endpoint), length(endpoint) == 1)
    stopifnot(is.list(params))
    structure(list(
        endpoint=endpoint
        params=params
    ),
    class="epidatacall")
}

full_url <- function(epidatacall, base_url = BASE_URL) {
    stopifnot(inherits(epidatacall, 'epidatacall'))
    stopifnot(is.character(base_url), length(base_url) == 1)

}

