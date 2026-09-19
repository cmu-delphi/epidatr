<% if (exists("type") && type == "historic") { %>
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This endpoint was previously deprecated and remains available for historical
#' reference. The underlying data source is no longer updated and no new data is
#' being ingested. For more details, refer to the "Endpoints kept for historical
#' reference" section of `vignette("migration-guide")`, and visit the [V5 signals
#' documentation](https://cmu-delphi.github.io/delphi-epidata/api/v5_signals.html).
<% } else { %>
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This is a V3/V4 endpoint. **As of September 30, 2026, the V3 and V4 APIs no
#' longer receive new data.** They still serve the historical data they already
#' have, but current data is only added to the V5 API, accessed via the
#' [epidata_snapshot()], [epidata_archive()], and [epidata_meta()] functions.
#' For more details on the changes, refer to `vignette("migration-guide")`,
#' and visit the [V5 signals
#' documentation](https://cmu-delphi.github.io/delphi-epidata/api/v5_signals.html)
#' to see which sources are currently available.
<% } %>
