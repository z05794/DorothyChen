test_that("filename is string", {

  filename <- make_filename(2013)
  expect_that(filename, is_a("character"))
})