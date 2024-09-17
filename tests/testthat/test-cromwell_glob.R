test_that("cromwell_glob", {
  vcr::use_cassette("cromwell_glob", {
    res <- cromwell_submit_batch(wdl = file_hello, params = file_inputs)
    # Sys.sleep(15) # Needed only for recording new fixture #nolint
    res <- cromwell_glob(res$id)
  })

  expect_type(res, "list")
  expect_gt(length(res), 10)
  expect_equal(res$status, "Running")

  # FIXME: expand_sub_workflows doesn't change anything in response
  vcr::use_cassette("cromwell_glob_expanded", {
    expanded <- cromwell_glob(res$id, expand_sub_workflows = TRUE)
  })

  expect_identical(res, expanded)
})
