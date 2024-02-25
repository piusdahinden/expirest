context("Get horizontal line elements")

test_that("get_hlines_succeeds", {
  re1 <-
    expirest_osle(
      data = exp4, response_vbl = "Conc", time_vbl = "Month",
      batch_vbl = "Batch", sl = 95.0, sl_sf = 3, srch_range = c(0, 5000),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "loose", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "lower")

  re2 <-
    expirest_wisle(
      data = exp4, response_vbl = "Conc", time_vbl = "Month",
      batch_vbl = "Batch", rl = 97.0, rl_sf = 4, sl = c(95.0, 105.0),
      sl_sf = c(3, 4), srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "loose", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "lower")

  # <-><-><-><->

  d_res1 <- get_hlines(model = re1)
  d_res2 <- get_hlines(model = re2)

  # <-><-><-><->

  expect_equal(d_res1$Conc, c(94.95))
  expect_equal(d_res1$Item, c("LSL"))
  expect_equal(d_res1$Colour, c("black"))
  expect_equal(d_res1$Type, c("dotted"))

  expect_equal(d_res2$Conc, c(94.95, 105.04))
  expect_equal(d_res2$Item, c("LSL", "USL"))
  expect_equal(d_res2$Colour, c("black", "black"))
  expect_equal(d_res2$Type, c("dotted", "dotted"))
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

  # <-><-><-><->

  expect_error(get_hlines(model = "re"),
    "model must be an object of class expirest_osle or expirest_wisle")
})
