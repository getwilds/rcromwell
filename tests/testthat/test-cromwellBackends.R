test_that("cromwellBackends", {
  vcr::use_cassette("cromwellBackends", {
    res <- cromwellBackends()
  })

  expect_type(res, "list")
  expect_length(res, 2)
  expect_named(res, c("defaultBackend", "supportedBackends"))
  expect_equal(res$defaultBackend, "Local")
})
