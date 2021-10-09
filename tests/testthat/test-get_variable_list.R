context("Variable list")

test_that("get_variable_list_succeeds", {
  tmp1 <-
    get_variable_list(response_vbl = "Assay", time_vbl = "Month",
                      batch_vbl = "Batch", xform = c("no", "no"))
  tmp2 <-
    get_variable_list(response_vbl = "Assay", time_vbl = "Month",
                      batch_vbl = "Batch", xform = c("log", "log"))
  tmp3 <-
    get_variable_list(response_vbl = "Assay", time_vbl = "Month",
                      batch_vbl = "Batch", xform = c("sqrt", "sqrt"))
  tmp4 <-
    get_variable_list(response_vbl = "Assay", time_vbl = "Month",
                      batch_vbl = "Batch", xform = c("sq", "sq"))

  # <-><-><-><->

  expect_equal(tmp1[["response"]], "Assay")
  expect_equal(tmp1[["time"]], "Month")
  expect_equal(tmp1[["batch"]], "Batch")

  expect_equal(tmp2[["response"]], "log.Assay")
  expect_equal(tmp2[["response.orig"]], "Assay")
  expect_equal(tmp2[["time"]], "log.Month")
  expect_equal(tmp2[["time.orig"]], "Month")
  expect_equal(tmp2[["batch"]], "Batch")

  expect_equal(tmp3[["response"]], "sqrt.Assay")
  expect_equal(tmp3[["response.orig"]], "Assay")
  expect_equal(tmp3[["time"]], "sqrt.Month")
  expect_equal(tmp3[["time.orig"]], "Month")
  expect_equal(tmp3[["batch"]], "Batch")

  expect_equal(tmp4[["response"]], "sq.Assay")
  expect_equal(tmp4[["response.orig"]], "Assay")
  expect_equal(tmp4[["time"]], "sq.Month")
  expect_equal(tmp4[["time.orig"]], "Month")
  expect_equal(tmp4[["batch"]], "Batch")
})

test_that("get_variable_list_fails", {
  expect_error(
    get_variable_list(response_vbl = 3, time_vbl = "Month",
                      batch_vbl = "Batch", xform = c("no", "no")),
    "response_vbl must be a string")
  expect_error(
    get_variable_list(response_vbl = "Assay", time_vbl = 2,
                      batch_vbl = "Batch", xform = c("no", "no")),
    "time_vbl must be a string")
  expect_error(
    get_variable_list(response_vbl = "Assay", time_vbl = "Month",
                      batch_vbl = 1, xform = c("no", "no")),
    "batch_vbl must be a string")
  expect_error(
    get_variable_list(response_vbl = "Assay", time_vbl = "Month",
                      batch_vbl = "Batch", xform = "no"),
    "specify xform appropriately")
  expect_error(
    get_variable_list(response_vbl = "Assay", time_vbl = "Month",
                      batch_vbl = "Batch", xform = c("yes", "no")),
    "specify xform appropriately")
  expect_error(
    get_variable_list(response_vbl = "Assay", time_vbl = "Month",
                      batch_vbl = "Batch", xform = c("no", "yes")),
    "specify xform appropriately")
})
