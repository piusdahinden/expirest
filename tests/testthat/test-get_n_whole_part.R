context("Get number of digits of whole part of decimal number")

test_that("get_n_whole_part_succeeds", {
  tmp <- c(-1.1, -0.1, 0, 0.1, 1.1, 12.1, 123.1, 123456789.1, NA)

  res <- get_n_whole_part(tmp)

  # <-><-><-><->

  expect_equivalent(res, c(1, 1, 1, 1, 1, 2, 3, 9, NA))
})

test_that("get_n_get_whole_part_fails", {
  expect_error(
    get_n_whole_part("x"),
    "x must be a numeric")
})
