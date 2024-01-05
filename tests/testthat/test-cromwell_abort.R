test_that("cromwell_abort", {
  vcr::use_cassette("cromwell_abort", {
    job <- cromwell_submit_batch(wdl = file_hello, params = file_inputs)

    # Sys.sleep(20) # Needed only for recording new fixture

    res <- cromwell_abort(job$id)
  })

  expect_equal(NROW(res), 1)
  expect_named(res, c("id", "status"))
  expect_equal(res$status, "Aborted")
})
