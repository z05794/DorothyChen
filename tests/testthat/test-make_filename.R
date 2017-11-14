test_that("filename is string", {

  filename <- make_filename(2013)
  expect_that(filename, is_a("character"))
})

test_that("fars_summarized_output", {
  years <- c(2013, 2014, 2015)
  df <- fars_summarize_years(years)
  expect_that(ncol(df), equals(length(years)+1))
  expect_that(df, is_a("data.frame"))
  expect_that(fars_summarize_years(2010), throws_error())
})