skip_if_not(cromwell_localhost_up())

test_that("proof api or DIY cromwell server down", {
  # This should happen whether proof or DIY if not on
  # campus or VPN for Fred Hutch at least
  # and happens if someone puts in a bad url

  httr::set_callback("response", \(req, res) curl::nslookup("abcdefg"))
  expect_error(
    cromwell_submit_batch(wdl = file_hello, params = file_inputs),
    "Unable to resolve host"
  )
  httr::set_callback("response", NULL)
})

test_that("401 unauthorized for proof api", {
  # This should only happen with proof api
  # regular cromwell server

  webmockr::stub_registry_clear()
  webmockr::stub_request("get", make_url(cw_url(), "engine/v1/version")) %>%
    webmockr::to_return(
      body = jsonlite::toJSON(response_proof_401, auto_unbox = TRUE),
      status = 401L,
      headers = list("Content-type" = "application/json")
    )

  unloadNamespace("vcr")
  webmockr::enable(quiet = TRUE)

  expect_error(
    cromwell_version(),
    "Unauthorized"
  )

  webmockr::stub_registry_clear()
  webmockr::disable(quiet = TRUE)
})

# reload vcr
attachNamespace("vcr")
