library(testthat)

test_that("filename is string", {
  filename <- make_filename(2014)
  expect_that(filename, is_a("character"))
})
