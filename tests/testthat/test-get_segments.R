context("Get line segments for graphical output")

test_that("get_segments_succeeds", {
  re <-
    expirest_wisle(
      data = exp4, response_vbl = "Conc", time_vbl = "Month",
      batch_vbl = "Batch", rl = 97.0, rl_sf = 4, sl = c(95.0, 105.0),
      sl_sf = c(3, 4), srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "loose", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "lower")

  x_range = c(-1, 29)

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
    get_segments(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
                 rl = rl, rl_index = rl_index, poi_woca = poi_woca,
                 wc_icpt = wc_icpt, x_range = x_range,
                 sl_model_name = sl_model_name,
                 wcsl_model_name = wcsl_model_name)

  # <-><-><-><->

  expect_equal(signif(d_res$Time.1, 5), c(0.00000, 0.00000, -0.33333, -0.11111))
  expect_equal(signif(d_res$Time.2, 5), c(7.5187, 29.000, -0.33333, -0.11111))
  expect_equal(signif(d_res$Response.1, 5), c(98.405, 96.995, 94.950, 98.405))
  expect_equal(signif(d_res$Response.2, 5), c(98.405, 96.995, 96.995, 100.45))
  expect_equal(d_res$Item,
               c("x.delta", "x.delta.shifted", "y.delta", "y.delta.shifted"))
  expect_equal(d_res$Colour, c("red", "grey0", "grey50", "grey50"))
  expect_equal(d_res$Type, c("dashed", "dotted", "solid", "solid"))
  expect_equal(d_res$Size, c(0.5, 0.5, 1.0, 1.0))
})

test_that("get_segments_fails", {
  re <-
    expirest_wisle(
      data = exp4, response_vbl = "Conc", time_vbl = "Month",
      batch_vbl = "Batch", rl = 97.0, rl_sf = 4, sl = c(95.0, 105.0),
      sl_sf = c(3, 4), srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "loose", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "lower")

  x_range = c(-1, 29)

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
    get_segments(sl = "95", ivl_side = ivl_side, wisle_est = re[["POI"]],
                 rl = rl, rl_index = rl_index, poi_woca = poi_woca,
                 wc_icpt = wc_icpt, x_range = x_range,
                 sl_model_name = sl_model_name,
                 wcsl_model_name = wcsl_model_name),
    "sl must be a numeric")
  expect_error(
    get_segments(sl = c(5, 6, 7), ivl_side = ivl_side, wisle_est = re[["POI"]],
                 rl = rl, rl_index = rl_index, poi_woca = poi_woca,
                 wc_icpt = wc_icpt, x_range = x_range,
                 sl_model_name = sl_model_name,
                 wcsl_model_name = wcsl_model_name),
    "sl must be a numeric or vector of length 1 or 2")
  expect_error(
    get_segments(sl = c(105, 95), ivl_side = ivl_side, wisle_est = re[["POI"]],
                 rl = rl, rl_index = rl_index, poi_woca = poi_woca,
                 wc_icpt = wc_icpt, x_range = x_range,
                 sl_model_name = sl_model_name,
                 wcsl_model_name = wcsl_model_name),
    "sl must be of the form")
  expect_error(
    get_segments(sl = sl, ivl_side = "middle", wisle_est = re[["POI"]],
                 rl = rl, rl_index = rl_index, poi_woca = poi_woca,
                 wc_icpt = wc_icpt, x_range = x_range,
                 sl_model_name = sl_model_name,
                 wcsl_model_name = wcsl_model_name),
    "specify ivl_side either as \"lower\", \"upper\" or \"both\"")
  expect_error(
    get_segments(sl = sl, ivl_side = ivl_side,
                 wisle_est = as.matrix(re[["POI"]]),
                 rl = rl, rl_index = rl_index, poi_woca = poi_woca,
                 wc_icpt = wc_icpt, x_range = x_range,
                 sl_model_name = sl_model_name,
                 wcsl_model_name = wcsl_model_name),
    "wisle_est must be a data frame")
  expect_error(
    get_segments(sl = sl, ivl_side = ivl_side,
                 wisle_est = cbind(re$POI, re$POI[, 1:6]),
                 rl = rl, rl_index = rl_index, poi_woca = poi_woca,
                 wc_icpt = wc_icpt, x_range = x_range,
                 sl_model_name = sl_model_name,
                 wcsl_model_name = wcsl_model_name),
    "wisle_est must be a data frame with 24 columns")
  expect_error(
    get_segments(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
                 rl = "98", rl_index = rl_index, poi_woca = poi_woca,
                 wc_icpt = wc_icpt, x_range = x_range,
                 sl_model_name = sl_model_name,
                 wcsl_model_name = wcsl_model_name),
    "rl must be a numeric")
  expect_error(
    get_segments(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
                 rl = c(98, 99), rl_index = rl_index, poi_woca = poi_woca,
                 wc_icpt = wc_icpt, x_range = x_range,
                 sl_model_name = sl_model_name,
                 wcsl_model_name = wcsl_model_name),
    "rl must be a positive integer of the same length")
  expect_error(
    get_segments(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
                 rl = rl, rl_index = "1", poi_woca = poi_woca,
                 wc_icpt = wc_icpt, x_range = x_range,
                 sl_model_name = sl_model_name,
                 wcsl_model_name = wcsl_model_name),
    "rl_index must be a positive integer of length 1")
  expect_error(
    get_segments(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
                 rl = rl, rl_index = 1.1, poi_woca = poi_woca,
                 wc_icpt = wc_icpt, x_range = x_range,
                 sl_model_name = sl_model_name,
                 wcsl_model_name = wcsl_model_name),
    "rl_index must be a positive integer of length 1")
  expect_error(
    get_segments(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
                 rl = rl, rl_index = c(1, 2), poi_woca = poi_woca,
                 wc_icpt = wc_icpt, x_range = x_range,
                 sl_model_name = sl_model_name,
                 wcsl_model_name = wcsl_model_name),
    "rl_index must be a positive integer of length 1")
  expect_error(
    get_segments(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
                 rl = rl, rl_index = -1, poi_woca = poi_woca,
                 wc_icpt = wc_icpt, x_range = x_range,
                 sl_model_name = sl_model_name,
                 wcsl_model_name = wcsl_model_name),
    "rl_index must be between 1 and the number of rows")
  expect_error(
    get_segments(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
                 rl = rl, rl_index = 2, poi_woca = poi_woca,
                 wc_icpt = wc_icpt, x_range = x_range,
                 sl_model_name = sl_model_name,
                 wcsl_model_name = wcsl_model_name),
    "rl_index must be between 1 and the number of rows")
  expect_error(
    get_segments(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
                 rl = rl, rl_index = rl_index, poi_woca = "8",
                 wc_icpt = wc_icpt, x_range = x_range,
                 sl_model_name = sl_model_name,
                 wcsl_model_name = wcsl_model_name),
    "poi_woca must be a numeric")
  expect_error(
    get_segments(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
                 rl = rl, rl_index = rl_index, poi_woca = c(8, 16),
                 wc_icpt = wc_icpt, x_range = x_range,
                 sl_model_name = sl_model_name,
                 wcsl_model_name = wcsl_model_name),
    "poi_woca must be a numeric of length 1")
  expect_error(
    get_segments(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
                 rl = rl, rl_index = rl_index, poi_woca = poi_woca,
                 wc_icpt = "100", x_range = x_range,
                 sl_model_name = sl_model_name,
                 wcsl_model_name = wcsl_model_name),
    "wc_icpt must be a numeric")
  expect_error(
    get_segments(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
                 rl = rl, rl_index = rl_index, poi_woca = poi_woca,
                 wc_icpt = c(100, 101), x_range = x_range,
                 sl_model_name = sl_model_name,
                 wcsl_model_name = wcsl_model_name),
    "wc_icpt must be a numeric of length 1")
  expect_error(
    get_segments(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
                 rl = rl, rl_index = rl_index, poi_woca = poi_woca,
                 wc_icpt = wc_icpt, x_range = 29,
                 sl_model_name = sl_model_name,
                 wcsl_model_name = wcsl_model_name),
    "x_range must be a vector of length 2")
  expect_error(
    get_segments(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
                 rl = rl, rl_index = rl_index, poi_woca = poi_woca,
                 wc_icpt = wc_icpt, x_range = x_range,
                 sl_model_name = 1,
                 wcsl_model_name = wcsl_model_name),
    "sl_model_name must be a single string")
  expect_error(
    get_segments(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
                 rl = rl, rl_index = rl_index, poi_woca = poi_woca,
                 wc_icpt = wc_icpt, x_range = x_range,
                 sl_model_name = c("s1", "s2"),
                 wcsl_model_name = wcsl_model_name),
    "sl_model_name must be a single string")
  expect_error(
    get_segments(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
                 rl = rl, rl_index = rl_index, poi_woca = poi_woca,
                 wc_icpt = wc_icpt, x_range = x_range,
                 sl_model_name = sl_model_name,
                 wcsl_model_name = 1),
    "wcsl_model_name must be a single string")
  expect_error(
    get_segments(sl = sl, ivl_side = ivl_side, wisle_est = re[["POI"]],
                 rl = rl, rl_index = rl_index, poi_woca = poi_woca,
                 wc_icpt = wc_icpt, x_range = x_range,
                 sl_model_name = sl_model_name,
                 wcsl_model_name = c("m1", "m2")),
    "wcsl_model_name must be a single string")
})
