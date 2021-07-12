
parse_api_date <- function(value) {
    as.Date(as.character(value), format = "%Y%m%d")
}

parse_api_week <- function(value) {
    years <- as.numeric(substr(value, 1, 4))
    weeks <- as.numeric(substr(value, 5, 6))
    MMWRweek::MMWRweek2Date(years, weeks)
}
