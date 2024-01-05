test_that("cromwell_backends", {
  vcr::use_cassette("cromwell_backends", {
    res <- cromwell_backends()
  })

  expect_type(res, "list")
  expect_length(res, 2)
  expect_named(res, c("defaultBackend", "supportedBackends"))
  expect_equal(res$defaultBackend, "Local")
})
