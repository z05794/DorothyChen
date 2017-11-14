library(testthat)

test_that("filename is string", {
  filename <- make_filename(2014)
  expect_that(filename, is_a("character"))
})

expect_that(sqrt(3) * sqrt(3), equals(3))

test_that("model fitting", {
  data(airquality)
  fit <- lm(Ozone ~ Wind, data = airquality)
  expect_that(fit, is_a("lm"))
  expect_that(1 + 1, equals(2))
})
