context("File Name")

library(testthat)

test_that("the filename is string", {
  filename <- make_filename(2013)
  expect_that(filename, is_a("character"))
})