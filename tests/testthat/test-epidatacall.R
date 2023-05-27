test_that("fetch and fetch_tbl", {
  epidata_call <- covidcast(
    data_source = "jhu-csse",
    signals = "confirmed_7dav_incidence_prop",
    time_type = "day",
    geo_type = "state",
    time_values = epirange("2020-06-01", "2020-08-01"),
    geo_values = "ca,fl"
  )

  # This test compares the output of a tibble using fetch and fetch_tbl.
  with_mocked_bindings(
    {
      tbl_out <- epidata_call %>% fetch_tbl()
      out <- epidata_call %>% fetch()
    },
    # RDS file generated with
    # epidata_call %>%
    # fetch_debug(format_type = "classic") %>%
    # readr::write_rds(testthat::test_path("data/test-classic.rds"))
    content = function(...) readRDS(testthat::test_path("data/test-classic.rds")),
    .package = "httr"
  )

  expect_identical(out, tbl_out)
})

test_that("fetch_tbl warns on non-success", {
  epidata_call <- covidcast(
    data_source = "jhu-csse",
    signals = "confirmed_7dav_incidence_prop",
    time_type = "day",
    geo_type = "state",
    time_values = epirange("2020-06-01", "2020-08-01"),
    geo_values = "ca,fl"
  )
  artificial_warning <- "* This is a warning with a leading asterisk and {braces} to make sure we don't have bulleting/glue bugs."
  debug_response_content_triplet <-
    # see generation code above
    readRDS(testthat::test_path("data/test-classic.rds")) %>%
    jsonlite::fromJSON() %>%
    `[[<-`("message", artificial_warning)
  with_mocked_bindings(
    {
      expect_warning(epidata_call %>% fetch_tbl(),
        regexp = paste0("epidata warning: ", artificial_warning),
        fixed = TRUE
      )
    },
    fromJSON = function(...) debug_response_content_triplet,
    .package = "jsonlite"
  )
})
