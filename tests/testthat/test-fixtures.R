# Golden parsing tests over recorded real API responses (see helper-fixtures.R).
# Each fixture is replayed through the endpoint's full parsing pipeline with the
# network mocked; the snapshot pins column classes and a sample of rows. A diff
# here after refreshing fixtures (make update-fixtures) means the upstream
# contract changed; a diff without refreshing means our parsing changed.

for (fixture_spec in fixture_specs()) {
  local({
    spec <- fixture_spec
    test_that(paste0("fixture parses: ", spec$file), {
      skip_if_not(
        file.exists(test_path("fixtures", spec$file)),
        "fixture missing; run make update-fixtures"
      )
      result <- NULL
      expect_no_warning(result <- replay_fixture(spec))
      if (inherits(result, "data.frame")) {
        expect_gt(nrow(result), 0)
        expect_snapshot({
          print(vapply(
            result,
            function(col) paste(class(col), collapse = "/"),
            character(1)
          ))
          print(head(as.data.frame(result), 3))
        })
      } else {
        expect_gt(length(result), 0)
        expect_snapshot(str(result, max.level = 3))
      }
    })
  })
}
