context("Try getting results from expression")

test_that("try_get_model_succeeds", {
  tmp <- try_get_model(1 + 9)

  expect_equal(tmp[["Model"]], 10)
  expect_equal(tmp[["Warning"]], NULL)
  expect_equal(tmp[["Error"]], NULL)
})

test_that("try_get_model_warns", {
  tmp <- suppressWarnings(try_get_model(log(-1)))

  expect_equal(tmp[["Model"]], NaN)
  expect_s3_class(tmp[["Warning"]], "simpleWarning")
  expect_equal(tmp[["Error"]], NULL)
})


test_that("try_get_model_fails", {
  tmp <- try_get_model(1 / "a")

  expect_equal(tmp[["Model"]], NULL)
  expect_equal(tmp[["Warning"]], NULL)
  expect_s3_class(tmp[["Error"]], "simpleError")
})
