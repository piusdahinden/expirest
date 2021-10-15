context("Generic summary and print functions")

test_that("summary.expirest_osle_succeeds", {
  re <-
    expirest_osle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                  response_vbl = "Potency", time_vbl = "Month",
                  batch_vbl = "Batch", sl = 95, sl_sf = 2,
                  srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
                  xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
                  ivl = "confidence", ivl_type = "one.sided",
                  ivl_side = "lower")

  # <-><-><-><->

  expect_s3_class(expect_output(summary(re)), "expirest_osle")
  expect_output(summary(re), "cics")
  expect_output(summary(re), "b2")

  expect_output(summary(re, digits = 5), "intercept: 100.57")
  expect_output(summary(re, digits = 5), "model: 25.996")
})

test_that("print.expirest_osle_succeeds", {
  re <-
    expirest_osle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                  response_vbl = "Potency", time_vbl = "Month",
                  batch_vbl = "Batch", sl = 95, sl_sf = 2,
                  srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
                  xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
                  ivl = "confidence", ivl_type = "one.sided",
                  ivl_side = "lower")

  # <-><-><-><->

  expect_s3_class(expect_output(print(re)), "expirest_osle")
  expect_output(print(re), "cics")
  expect_output(print(re), "b2")

  expect_output(print(re, digits = 5), "intercept: 100.57")
  expect_output(print(re, digits = 5), "model: 25.996")
})

test_that("summary.expirest_wisle_succeeds", {
  re <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = 2.0, rl_sf = 3, sl = 0.5, sl_sf = 2,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower")

  # <-><-><-><->

  expect_s3_class(expect_output(summary(re)), "expirest_wisle")
  expect_output(summary(re), "cics")
  expect_output(summary(re), "b1")

  expect_output(summary(re, digits = 5), "cics 0.5  2 92.999 118.19")
})

test_that("print.expirest_wisle_succeeds", {
  re <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = 2.0, rl_sf = 3, sl = 0.5, sl_sf = 2,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower")

  # <-><-><-><->

  expect_s3_class(expect_output(print(re)), "expirest_wisle")
  expect_output(print(re), "cics")
  expect_output(print(re), "b1")

  expect_output(print(re, digits = 5), "cics 0.5  2 92.999 118.19")
})
