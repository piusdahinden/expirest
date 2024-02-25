context("Get arrow representation for graphical output")

test_that("get_arrow_succeeds", {
  re <-
    expirest_wisle(
      data = exp4, response_vbl = "Conc", time_vbl = "Month",
      batch_vbl = "Batch", rl = 97.0, rl_sf = 4, sl = c(95.0, 105.0),
      sl_sf = c(3, 4), srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "loose", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "lower")

  # <-><-><-><->

  d_res <- get_arrow(model = re, x_range = c(-1, 29), rl_index = 1,
                     mtbs = "verified")

  # <-><-><-><->

  expect_equal(signif(d_res$Month.1, 5), -0.11111)
  expect_equal(signif(d_res$Month.2, 5), -0.50000)
  expect_equal(signif(d_res$Conc.1, 5), 99.428)
  expect_equal(signif(d_res$Conc.2, 5), 95.972)
  expect_equal(d_res$Item, "arrow")
  expect_equal(d_res$Colour, "grey50")
  expect_equal(d_res$Line.Type, "solid")
  expect_equal(d_res$Arrow.Type, "closed")
  expect_equal(d_res$Size, 0.5)
  expect_equal(d_res$Curvature, 0.5)
  expect_equal(d_res$Angle, 90)
  expect_equal(d_res$Length, 5)
})

test_that("get_arrow_fails", {
  re <-
    expirest_wisle(
      data = exp4, response_vbl = "Conc", time_vbl = "Month",
      batch_vbl = "Batch", rl = 97.0, rl_sf = 4, sl = c(95.0, 105.0),
      sl_sf = c(3, 4), srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "loose", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "lower")

  x_range <- c(-1, 29)
  rl_index <- 1

  # <-><-><-><->

  expect_error(
    get_arrow(model = "re", x_range = c(-1, 29), rl_index = 1,
              mtbs = "verified"),
    "model must be an object of class expirest_wisle")
  expect_error(
    get_arrow(model = re, x_range = c("1", "2"), rl_index = rl_index,
              mtbs = "verified"),
    "x_range must be a numeric vector of length 2")
  expect_error(
    get_arrow(model = re, x_range = 29, rl_index = rl_index,
              mtbs = "verified"),
    "x_range must be a numeric vector of length 2")
  expect_error(
    get_arrow(model = re, x_range = x_range, rl_index = "rl_index",
              mtbs = "verified"),
    "rl_index must be a positive integer of length 1")
  expect_error(
    get_arrow(model = re, x_range = x_range, rl_index = 1.1,
              mtbs = "verified"),
    "rl_index must be a positive integer of length 1")
  expect_error(
    get_arrow(model = re, x_range = x_range, rl_index = 1:3,
              mtbs = "verified"),
    "rl_index must be a positive integer of length 1")
  expect_error(
    get_arrow(model = re, x_range = x_range, rl_index = rl_index,
              mtbs = "incorrect"),
    "specify mtbs either as \"verified\", \"cics\", \"dics\", ")
})
