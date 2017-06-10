test_that("filename is conform standard", {
  x <- make_filename(2013)
  expect_that(x, equals("accident_2013.csv.bz2"))
})

