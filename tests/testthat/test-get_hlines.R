context("Get horizontal line elements")

test_that("get_hlines_succeeds", {
  re <-
    expirest_wisle(
      data = exp4, response_vbl = "Conc", time_vbl = "Month",
      batch_vbl = "Batch", rl = 97.0, rl_sf = 4, sl = c(95.0, 105.0),
      sl_sf = c(3, 4), srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "loose", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "lower")

  sl <- re[["Limits"]][["sl"]]
  ivl_side <- re[["Parameters"]][["ivl.side"]]

  # <-><-><-><->

  d_res <- get_hlines(sl = sl, ivl_side = ivl_side)

  # <-><-><-><->

  expect_equal(d_res$Response, c(94.95, 105.04))
  expect_equal(d_res$Item, c("LSL", "USL"))
  expect_equal(d_res$Colour, c("black", "black"))
  expect_equal(d_res$Type, c("dotted", "dotted"))
})

test_that("get_hlines_fails", {
  re <-
    expirest_wisle(
      data = exp4, response_vbl = "Conc", time_vbl = "Month",
      batch_vbl = "Batch", rl = 97.0, rl_sf = 4, sl = c(95.0, 105.0),
      sl_sf = c(3, 4), srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "loose", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "lower")

  sl <- re[["Limits"]][["sl"]]
  ivl_side <- re[["Parameters"]][["ivl.side"]]

  # <-><-><-><->

  expect_error(
    get_hlines(sl = "95", ivl_side = ivl_side),
    "sl must be a numeric")
  expect_error(
    get_hlines(sl = c(95, 100, 105), ivl_side = ivl_side),
    "sl must be a numeric or vector of length 1 or 2")
  expect_error(
    get_hlines(sl = c(105, 95), ivl_side = ivl_side),
    "sl must be of the form")
  expect_error(
    get_hlines(sl = sl, ivl_side = "middle"),
    "specify ivl_side either as \"lower\", \"upper\" or \"both\"")
})
