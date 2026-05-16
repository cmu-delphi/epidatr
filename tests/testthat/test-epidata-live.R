# This script tests all endpoints in the list of sources
# provided in cast_sources.
skip_on_cran()
skip_if_not(curl::has_internet())

auth <- Sys.getenv("DELPHI_EPIDATA_KEY")
if (auth == "") {
  warning("DELPHI_EPIDATA_KEY not set; skipping live endpoint tests")
  skip("DELPHI_EPIDATA_KEY not set")
}

cast_sources <- c("nssp", "nhsn", "pophive", "nwss")

for (source in cast_sources) {
  test_that(sprintf("test `epidata_meta` for source =%s", source), {

    source_meta <- epidata_meta(source = source)[[source]]
    expect_type(source_meta, "list")
    expect_true(length(source_meta$signals) > 0)
    expect_true(length(source_meta$geo_types) > 0)
  })
  for (signal in source_meta$signals) {
    test_that(sprintf("epidata_* for source =%s and signal = %s", source, signal), {
      meta_geo_type <- source_meta$geo_types
      geo_type <- if ("nation" %in% meta_geo_type) "nation" else meta_geo_type[[1]]
      snapshot <- epidata_snapshot(
        source = source,
        signals = signal,
        geo_type = geo_type
      )
      expect_s3_class(snapshot, "tbl_df")
      expect_s3_class(snapshot$time_value, "Date")
      expect_s3_class(snapshot$version, "Date")
      archive <- epidata_archive(
        source = source,
        signals = signal,
        geo_type = geo_type
      )
      expect_s3_class(archive, "tbl_df")
      expect_s3_class(archive$version, "Date")
    })
  }
}
