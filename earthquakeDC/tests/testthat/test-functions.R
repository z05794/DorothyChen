context("Test functions")
library(earthquakeDC)

filename <- system.file("inst/data/signif.txt", package = "earthquakeDC")
df <- readr::read_delim(filename, delim = "\t")

test_that("eq_clean_data returns a data frame", {
  expect_is(eq_clean_data(df), "data.frame")
})

test_that("eq_clean_data$datetime is Date type", {
  expect_is(eq_clean_data(df)$datetime, "Date")
})

test_that("eq_clean_data returns numeric coordinates", {
  expect_is(eq_clean_data(df)$latitude, "numeric")
  expect_is(eq_clean_data(df)$longitude, "numeric")
})

