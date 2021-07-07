
# Helper function to cast values and/or ranges to strings
format_item <- function(value) {
    if(is.list(value) && 'from' %in% names(value) && 'to' %in% names(value)) {
      paste0(toString(value$from), '-', toString(value$to))
    } else {
      toString(value)
    }
  }

# Helper function to build a list of values and/or ranges
format_list <- function(values) {
    if(!is.list(values) || ('from' %in% names(values) && 'to' %in% names(values))) {
      values <- list(values)
    }
    paste(sapply(values, format_item), collapse=',')
}
