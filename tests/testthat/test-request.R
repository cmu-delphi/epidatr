# Retry policy (see `is_transient_response()` in R/request.R). The server sends
# `Retry-After: 3600` while a table is being rebuilt and `Retry-After: 10` when
# the database is busy; the client must not sleep for an hour.

retry_resp <- function(status, retry_after = NULL, date = NULL, body = NULL) {
  headers <- list("content-type" = "application/json")
  if (!is.null(retry_after)) headers[["Retry-After"]] <- retry_after
  if (!is.null(date)) headers[["Date"]] <- date
  if (is.null(body)) {
    body <- '{"detail": "Data temporarily unavailable.", "code": "database_table_unavailable"}'
  }
  create_mock_response( # nolint: object_usage_linter.
    body,
    status_code = status,
    headers = headers
  )
}

test_that("retry_after_seconds() parses Retry-After and never errors", {
  expect_identical(retry_after_seconds(retry_resp(503)), NA_real_)
  expect_equal(retry_after_seconds(retry_resp(503, "10")), 10)
  expect_equal(retry_after_seconds(retry_resp(503, "3600")), 3600)
  expect_equal(retry_after_seconds(retry_resp(503, "0")), 0)
  # Malformed or unusable values count as no header, without warnings.
  for (bad in c("-5", "abc", "NaN", "Inf", "1e400", "10, 20", "")) {
    expect_no_warning(val <- retry_after_seconds(retry_resp(503, bad)))
    expect_identical(val, NA_real_, info = bad)
  }
  # An HTTP-date is measured from the response's Date header.
  expect_equal(
    retry_after_seconds(retry_resp(
      503, "Wed, 21 Oct 2015 07:28:00 GMT",
      date = "Wed, 21 Oct 2015 07:27:00 GMT"
    )),
    60
  )
  # A date in the past counts as no header.
  expect_identical(
    retry_after_seconds(retry_resp(
      503, "Wed, 21 Oct 2015 07:28:00 GMT",
      date = "Wed, 21 Oct 2015 08:00:00 GMT"
    )),
    NA_real_
  )
  # An HTTP-date with no Date header makes httr2 error; we return NA instead.
  expect_identical(
    retry_after_seconds(retry_resp(503, "Wed, 21 Oct 2015 07:28:00 GMT")),
    NA_real_
  )
})

test_that("is_transient_response() retries short waits and fails fast on long ones", {
  cap <- 60
  # Server errors and gateway errors are retried (with backoff) when they give
  # no Retry-After or a short one.
  for (status in c(500, 502, 503, 504)) {
    expect_true(is_transient_response(retry_resp(status), cap), info = status)
    expect_true(is_transient_response(retry_resp(status, "10"), cap), info = status)
    # A wait longer than the cap is never honoured, whatever the status.
    expect_false(is_transient_response(retry_resp(status, "3600"), cap), info = status)
  }
  expect_true(is_transient_response(retry_resp(503, "60"), cap))
  expect_false(is_transient_response(retry_resp(503, "61"), cap))
  expect_true(is_transient_response(retry_resp(503, "abc"), cap))
  # 429 is retried only when the server says when to come back.
  expect_false(is_transient_response(retry_resp(429), cap))
  expect_false(is_transient_response(retry_resp(429, "abc"), cap))
  expect_true(is_transient_response(retry_resp(429, "10"), cap))
  expect_false(is_transient_response(retry_resp(429, "3600"), cap))
  # Other statuses are not transient.
  for (status in c(200, 400, 401, 404, 414, 422)) {
    expect_false(is_transient_response(retry_resp(status, "10"), cap), info = status)
  }
  # The cap is configurable; zero means never honour a Retry-After wait.
  expect_true(is_transient_response(retry_resp(503, "3600"), 7200))
  expect_false(is_transient_response(retry_resp(503, "10"), 0))
  expect_true(is_transient_response(retry_resp(503, "0"), 0))
})

test_that("the epidatr.max_retry_wait option is validated", {
  withr::local_options(epidatr.max_retry_wait = NULL)
  expect_equal(max_retry_wait(), 60)
  withr::local_options(epidatr.max_retry_wait = 600)
  expect_equal(max_retry_wait(), 600)
  withr::local_options(epidatr.max_retry_wait = 0L)
  expect_equal(max_retry_wait(), 0)
  for (bad in list(-1, Inf, NA_real_, NaN, "60", c(1, 2), TRUE)) {
    withr::local_options(epidatr.max_retry_wait = bad)
    expect_error(max_retry_wait(), class = "epidatr__invalid_option")
  }
})

# Drive httr2's real retry loop by mocking its internal single-attempt
# functions and its sleep, so no network or real waiting is involved. Both the
# in-memory (`req_perform()`) and streaming (`req_perform_connection()`) entry
# points are mocked when they exist, so these tests can never reach the live
# API. If epidatr streams responses through `perform_and_read()`, it is routed
# through `req_perform()` here: the request carries the same retry policy, and
# mocked responses can't be streamed.
local_retry_loop <- function(responses, env = parent.frame()) {
  httr2_ns <- asNamespace("httr2")
  testthat::skip_if_not(
    exists("req_perform1", envir = httr2_ns, inherits = FALSE) &&
      exists("sys_sleep", envir = httr2_ns, inherits = FALSE),
    "httr2 internals used by this test are not available"
  )
  state <- new.env(parent = emptyenv())
  state$calls <- 0L
  state$sleeps <- numeric()
  next_response <- function(req, ...) {
    state$calls <- state$calls + 1L
    resp <- responses[[min(state$calls, length(responses))]]
    resp$request <- req
    resp
  }
  mocks <- list(
    req_perform1 = next_response,
    sys_sleep = function(seconds, ...) {
      state$sleeps <- c(state$sleeps, seconds)
      invisible()
    }
  )
  if (exists("req_perform_connection1", envir = httr2_ns, inherits = FALSE)) {
    mocks$req_perform_connection1 <- next_response
  }
  rlang::exec(
    testthat::local_mocked_bindings,
    !!!mocks,
    .package = "httr2",
    .env = env
  )
  if (exists("perform_and_read", envir = asNamespace("epidatr"), inherits = FALSE)) {
    testthat::local_mocked_bindings(
      perform_and_read = function(req, ...) httr2::req_perform(req),
      .package = "epidatr",
      .env = env
    )
  }
  state
}

test_that("do_request() fails fast on a long Retry-After instead of sleeping", {
  withr::local_options(epidatr.max_retry_wait = NULL)
  call <- pvt_cdc(
    auth = "x", epiweeks = epirange(202003, 202304), locations = "ma",
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
  state <- local_retry_loop(list(retry_resp(503, "3600")))
  expect_error(
    do_request(call, "json", timeout_seconds = 10, fields = NULL),
    regexp = "Data temporarily unavailable",
    class = "httr2_http_503"
  )
  expect_equal(state$calls, 1L)
  expect_true(all(state$sleeps == 0))
})

test_that("do_request() still retries short Retry-After waits", {
  withr::local_options(epidatr.max_retry_wait = NULL)
  call <- pvt_cdc(
    auth = "x", epiweeks = epirange(202003, 202304), locations = "ma",
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
  state <- local_retry_loop(list(
    retry_resp(503, "10"),
    create_mock_response('{"epidata": [], "result": -2, "message": "no results"}')
  ))
  res <- do_request(call, "json", timeout_seconds = 10, fields = NULL)
  expect_equal(httr2::resp_status(res), 200L)
  expect_equal(state$calls, 2L)
  expect_equal(max(state$sleeps), 10)
})

test_that("do_request() does not retry a 429 without Retry-After", {
  withr::local_options(epidatr.max_retry_wait = NULL)
  call <- pvt_cdc(
    auth = "x", epiweeks = epirange(202003, 202304), locations = "ma",
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
  state <- local_retry_loop(list(retry_resp(
    429,
    body = '{"detail": "Rate limit exceeded.", "code": "rate_limited"}'
  )))
  expect_error(
    do_request(call, "json", timeout_seconds = 10, fields = NULL),
    regexp = "Rate limit exceeded",
    class = "httr2_http_429"
  )
  expect_equal(state$calls, 1L)
})

test_that("do_request() respects a larger epidatr.max_retry_wait", {
  withr::local_options(epidatr.max_retry_wait = 7200)
  call <- pvt_cdc(
    auth = "x", epiweeks = epirange(202003, 202304), locations = "ma",
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
  state <- local_retry_loop(list(retry_resp(503, "3600")))
  expect_error(
    do_request(call, "json", timeout_seconds = 10, fields = NULL),
    class = "httr2_http_503"
  )
  expect_equal(state$calls, 3L)
  expect_equal(sum(state$sleeps), 7200)
})

test_that("HTTP errors say when the server suggests retrying", {
  msg <- function(resp) error_body_message(resp)
  expect_identical(
    msg(retry_resp(503, "3600")),
    "Data temporarily unavailable. The server suggests retrying in about 60 minutes."
  )
  expect_identical(
    msg(retry_resp(503, "10")),
    "Data temporarily unavailable. The server suggests retrying in about 10 seconds."
  )
  expect_identical(
    msg(retry_resp(503, "90")),
    "Data temporarily unavailable. The server suggests retrying in about 2 minutes."
  )
  # Rounding: never "0 seconds" or "60 seconds"; hours for long waits.
  hint <- function(after) retry_hint(retry_resp(503, after))
  expect_identical(hint("0.4"), "The server suggests retrying in about 1 second.")
  expect_identical(hint("59.6"), "The server suggests retrying in about 1 minute.")
  expect_identical(hint("89"), "The server suggests retrying in about 1 minute.")
  expect_identical(hint("5340"), "The server suggests retrying in about 89 minutes.")
  expect_identical(hint("7200"), "The server suggests retrying in about 2 hours.")
  # Monotone at the minutes/hours boundary: 89.5 minutes reads as 2 hours, not 1.
  expect_identical(hint("5370"), "The server suggests retrying in about 2 hours.")
  expect_identical(hint("5400"), "The server suggests retrying in about 2 hours.")
  expect_identical(hint("86400"), "The server suggests retrying in about 24 hours.")
  # No hint for no, zero, unusable or absurd waits.
  expect_identical(msg(retry_resp(503)), "Data temporarily unavailable.")
  expect_identical(msg(retry_resp(503, "0")), "Data temporarily unavailable.")
  expect_identical(msg(retry_resp(503, "abc")), "Data temporarily unavailable.")
  expect_identical(msg(retry_resp(503, "1000000")), "Data temporarily unavailable.")
  # The hint also appears when the body gives no detail.
  expect_identical(
    msg(retry_resp(503, "3600", body = "{}")),
    "The server suggests retrying in about 60 minutes."
  )
  no_body <- retry_resp(503, "3600")
  no_body$body <- raw()
  expect_identical(msg(no_body), "The server suggests retrying in about 60 minutes.")
  # HTML bodies with several paragraphs come back as a single string.
  html <- create_mock_response(
    "<html><body><p>Service unavailable.</p><p>Try later.</p></body></html>",
    status_code = 503L,
    headers = list("content-type" = "text/html", "Retry-After" = "120")
  )
  expect_identical(
    msg(html),
    "Service unavailable. Try later. The server suggests retrying in about 2 minutes."
  )
})

test_that("the retry hint reaches the user-facing error", {
  call <- pvt_cdc(
    auth = "x", epiweeks = epirange(202003, 202304), locations = "ma",
    fetch_args = fetch_args_list(dry_run = TRUE)
  )
  with_mocked_response(
    retry_resp(503, "3600"),
    expect_error(
      fetch(call),
      regexp = "retrying in about 60 minutes",
      class = "httr2_http_503"
    )
  )
})

test_that("the 414 fallback POSTs with parameters in the body only", {
  seen <- list()
  with_mock_perform(
    function(req) {
      seen[[length(seen) + 1]] <<- req
      if (length(seen) == 1) {
        create_mock_response("{}", status_code = 414L)
      } else {
        '{"epidata":[],"result":-2,"message":"no results"}'
      }
    },
    suppressWarnings(pvt_cdc(
      auth = "k", locations = "ma", epiweeks = epirange(202003, 202304),
      fetch_args = fetch_args_list(return_empty = TRUE)
    ))
  )
  expect_length(seen, 2)
  expect_identical(seen[[2]]$method, "POST")
  expect_false(grepl("?", seen[[2]]$url, fixed = TRUE))
  expect_named(seen[[2]]$body$data, c("auth", "locations", "epiweeks"), ignore.order = TRUE)
})

test_that("the API key is sent as Basic auth (classic) or a token header (cast)", {
  hdr <- function(req) httr2::req_get_headers(req, redacted = "reveal")
  seen <- NULL
  capture <- function(req) {
    seen <<- req
    '{"epidata":[],"result":-2,"message":"no results"}'
  }

  withr::local_envvar(DELPHI_EPIDATA_KEY = "secret")
  with_mock_perform(capture, suppressWarnings(
    pub_delphi(system = "ec", epiweek = 201501, fetch_args = fetch_args_list(return_empty = TRUE))
  ))
  expect_identical(
    hdr(seen)$Authorization,
    paste("Basic", jsonlite::base64_enc(charToRaw("epidata:secret")))
  )
  expect_null(hdr(seen)$token)

  seen <- NULL
  with_mock_perform(capture, try(epidata_meta(), silent = TRUE))
  expect_identical(hdr(seen)$token, "secret")
  expect_null(hdr(seen)$Authorization)

  withr::local_envvar(DELPHI_EPIDATA_KEY = "")
  seen <- NULL
  with_mock_perform(capture, suppressWarnings(try(epidata_meta(), silent = TRUE)))
  expect_null(hdr(seen)$token)
})
