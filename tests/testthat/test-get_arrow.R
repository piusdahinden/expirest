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

  x_range <- c(-1, 29)

  sl <- re[["Limits"]][["sl"]]
  rl <- re[["Limits"]][["rl"]]
  rl_index <- 1

  model_name <- re[["Model.Type"]][["type.acronym"]]
  poi_model_name <- paste("POI.Model", model_name, sep = ".")
  sl_model_name <- paste("Shelf.Life", model_name, sep = ".")
  wcsl_model_name <- paste("WCSL", model_name, sep = ".")

  poi_woca <- re[["POI"]][rl_index, sl_model_name]

  ivl_side <- re[["Parameters"]][["ivl.side"]]
  wc_icpt <- re[["wc.icpt"]][, model_name]

  # <-><-><-><->

  d_res <-
    get_arrow(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
              rl = rl, rl_index = rl_index, wc_icpt = wc_icpt,
              x_range = x_range, sl_model_name = sl_model_name,
              wcsl_model_name = wcsl_model_name)

  # <-><-><-><->

  expect_equal(signif(d_res$Time.1, 5), -0.11111)
  expect_equal(signif(d_res$Time.2, 5), -0.50000)
  expect_equal(signif(d_res$Response.1, 5), 99.428)
  expect_equal(signif(d_res$Response.2, 5), 95.972)
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

  sl <- re[["Limits"]][["sl"]]
  rl <- re[["Limits"]][["rl"]]
  rl_index <- 1

  model_name <- re[["Model.Type"]][["type.acronym"]]
  poi_model_name <- paste("POI.Model", model_name, sep = ".")
  sl_model_name <- paste("Shelf.Life", model_name, sep = ".")
  wcsl_model_name <- paste("WCSL", model_name, sep = ".")

  poi_woca <- re[["POI"]][rl_index, sl_model_name]

  ivl_side <- re[["Parameters"]][["ivl.side"]]
  wc_icpt <- re[["wc.icpt"]][, model_name]

  # <-><-><-><->

  expect_error(
    get_arrow(sl = "95", ivl_side = ivl_side, wisle_est = re[["POI"]],
              rl = rl, rl_index = rl_index, wc_icpt = wc_icpt,
              x_range = x_range, sl_model_name = sl_model_name,
              wcsl_model_name = wcsl_model_name),
    "sl must be a numeric")
  expect_error(
    get_arrow(sl = c(5, 6, 7), ivl_side = ivl_side, wisle_est = re[["POI"]],
              rl = rl, rl_index = rl_index, wc_icpt = wc_icpt,
              x_range = x_range, sl_model_name = sl_model_name,
              wcsl_model_name = wcsl_model_name),
    "sl must be a numeric or vector of length 1 or 2")
  expect_error(
    get_arrow(sl = c(105, 95), ivl_side = ivl_side, wisle_est = re[["POI"]],
              rl = rl, rl_index = rl_index, wc_icpt = wc_icpt,
              x_range = x_range, sl_model_name = sl_model_name,
              wcsl_model_name = wcsl_model_name),
    "sl must be of the form")
  expect_error(
    get_arrow(sl = sl, ivl_side = "middle", wisle_est = re[["POI"]],
              rl = rl, rl_index = rl_index, wc_icpt = wc_icpt,
              x_range = x_range, sl_model_name = sl_model_name,
              wcsl_model_name = wcsl_model_name),
    "specify ivl_side either as \"lower\", \"upper\" or \"both\"")
  expect_error(
    get_arrow(sl = sl, ivl_side = ivl_side, wisle_est = as.matrix(re[["POI"]]),
              rl = rl, rl_index = rl_index, wc_icpt = wc_icpt,
              x_range = x_range, sl_model_name = sl_model_name,
              wcsl_model_name = wcsl_model_name),
    "wisle_est must be a data frame")
  expect_error(
    get_arrow(sl = sl, ivl_side = ivl_side,
              wisle_est = cbind(re[["POI"]], re[["POI"]][1:6]),
              rl = rl, rl_index = rl_index, wc_icpt = wc_icpt,
              x_range = x_range, sl_model_name = sl_model_name,
              wcsl_model_name = wcsl_model_name),
    "wisle_est must be a data frame with 24 columns")
  expect_error(
    get_arrow(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
              rl = "98", rl_index = rl_index, wc_icpt = wc_icpt,
              x_range = x_range, sl_model_name = sl_model_name,
              wcsl_model_name = wcsl_model_name),
    "rl must be a numeric")
  expect_error(
    get_arrow(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
              rl = c(98, 99), rl_index = rl_index, wc_icpt = wc_icpt,
              x_range = x_range, sl_model_name = sl_model_name,
              wcsl_model_name = wcsl_model_name),
    "rl must be a positive integer of the same length")
  expect_error(
    get_arrow(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
              rl = rl, rl_index = "1", wc_icpt = wc_icpt,
              x_range = x_range, sl_model_name = sl_model_name,
              wcsl_model_name = wcsl_model_name),
    "rl_index must be a positive integer of length 1")
  expect_error(
    get_arrow(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
              rl = rl, rl_index = 1.1, wc_icpt = wc_icpt,
              x_range = x_range, sl_model_name = sl_model_name,
              wcsl_model_name = wcsl_model_name),
    "rl_index must be a positive integer of length 1")
  expect_error(
    get_arrow(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
              rl = rl, rl_index = c(1, 2), wc_icpt = wc_icpt,
              x_range = x_range, sl_model_name = sl_model_name,
              wcsl_model_name = wcsl_model_name),
    "rl_index must be a positive integer of length 1")
  expect_error(
    get_arrow(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
              rl = rl, rl_index = -1, wc_icpt = wc_icpt,
              x_range = x_range, sl_model_name = sl_model_name,
              wcsl_model_name = wcsl_model_name),
    "rl_index must be between 1 and the number of rows")
  expect_error(
    get_arrow(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
              rl = rl, rl_index = 2, wc_icpt = wc_icpt,
              x_range = x_range, sl_model_name = sl_model_name,
              wcsl_model_name = wcsl_model_name),
    "rl_index must be between 1 and the number of rows")
  expect_error(
    get_arrow(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
              rl = rl, rl_index = rl_index, wc_icpt = "100",
              x_range = x_range, sl_model_name = sl_model_name,
              wcsl_model_name = wcsl_model_name),
    "wc_icpt must be a numeric")
  expect_error(
    get_arrow(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
              rl = rl, rl_index = rl_index, wc_icpt = c(100, 101),
              x_range = x_range, sl_model_name = sl_model_name,
              wcsl_model_name = wcsl_model_name),
    "wc_icpt must be a numeric of length 1")
  expect_error(
    get_arrow(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
              rl = rl, rl_index = rl_index, wc_icpt = wc_icpt,
              x_range = 29, sl_model_name = sl_model_name,
              wcsl_model_name = wcsl_model_name),
    "x_range must be a vector of length 2")
  expect_error(
    get_arrow(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
              rl = rl, rl_index = rl_index, wc_icpt = wc_icpt,
              x_range = x_range, sl_model_name = 1,
              wcsl_model_name = wcsl_model_name),
    "sl_model_name must be a single string")
  expect_error(
    get_arrow(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
              rl = rl, rl_index = rl_index, wc_icpt = wc_icpt,
              x_range = x_range, sl_model_name = c("s1", "s2"),
              wcsl_model_name = wcsl_model_name),
    "sl_model_name must be a single string")
  expect_error(
    get_arrow(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
              rl = rl, rl_index = rl_index, wc_icpt = wc_icpt,
              x_range = x_range, sl_model_name = sl_model_name,
              wcsl_model_name = 1),
    "wcsl_model_name must be a single string")
  expect_error(
    get_arrow(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
              rl = rl, rl_index = rl_index, wc_icpt = wc_icpt,
              x_range = x_range, sl_model_name = sl_model_name,
              wcsl_model_name = c("m1", "m2")),
    "wcsl_model_name must be a single string")
})
