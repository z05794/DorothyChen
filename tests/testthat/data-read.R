context("Data Reading")

library(testthat)

test_that("fars_read", {
  dir <- "./data/"
  df <- fars_read(paste0(dir, "accident_2013.csv.bz2"))
  expect_true(nrow(df) > 1)
  expect_true(ncol(df) == 50)

})

