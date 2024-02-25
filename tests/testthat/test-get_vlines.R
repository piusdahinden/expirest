context("Get vertical line elements")

test_that("get_vlines_succeeds", {
  re1 <-
    expirest_osle(
      data = exp4, response_vbl = "Conc", time_vbl = "Month",
      batch_vbl = "Batch", sl = 95, sl_sf = 3, srch_range = c(0, 5000),
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

  d_res1 <- get_vlines(model = re1, mtbs = "verified")
  d_res2 <- get_vlines(model = re2, rl_index = 1, mtbs = "verified")

  # <-><-><-><->

  expect_equal(signif(d_res1$Month, 5), c(23.698))
  expect_equal(d_res1$Item, c("poi.model"))
  expect_equal(d_res1$Colour, c("forestgreen"))
  expect_equal(d_res1$Type, c("dotdash"))

  expect_equal(signif(d_res2$Month, 5), c(7.5187, 23.698))
  expect_equal(d_res2$Item, c("poi.woca", "poi.model"))
  expect_equal(d_res2$Colour, c("forestgreen", "grey50"))
  expect_equal(d_res2$Type, c("dashed", "dotdash"))
})

test_that("get_vlines_fails", {
  re <-
    expirest_wisle(
      data = exp4, response_vbl = "Conc", time_vbl = "Month",
      batch_vbl = "Batch", rl = 97.0, rl_sf = 4, sl = c(95.0, 105.0),
      sl_sf = c(3, 4), srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "loose", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "lower")

  # <-><-><-><->

  expect_error(get_vlines(model = "re", rl_index = 1, mtbs = "verified"),
    "model must be an object of class expirest_osle or expirest_wisle")
  expect_error(get_vlines(model = re, rl_index = "1", mtbs = "verified"),
    "rl_index must be a positive integer of length 1")
  expect_error(get_vlines(model = re, rl_index = 1.1, mtbs = "verified"),
    "rl_index must be a positive integer of length 1")
  expect_error(get_vlines(model = re, rl_index = 1:3, mtbs = "verified"),
    "rl_index must be a positive integer of length 1")
  expect_error(get_vlines(model = re, rl_index = 1, mtbs = "incorrect"),
               "specify mtbs either as \"verified\", \"cics\", \"dics\", ")
})
