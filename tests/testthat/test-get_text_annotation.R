context("Get text annotation elements for graphical output")

test_that("get_text_annotation_succeeds", {
  re <-
    expirest_wisle(
      data = exp4, response_vbl = "Conc", time_vbl = "Month",
      batch_vbl = "Batch", rl = 97.0, rl_sf = 4, sl = c(95.0, 105.0),
      sl_sf = c(3, 4), srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "loose", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "lower")

  x_range <- c(-1, 29)
  y_range <- c(92, 107)

  sl <- re[["Limits"]][["sl"]]
  sl_sf <- re[["Limits"]][["sl.sf"]] + 1
  rl_index <- 1
  rl_sf <- re[["Limits"]][["rl.sf"]] + 1

  model_name <- re[["Model.Type"]][["type.acronym"]]
  poi_model_name <- paste("POI.Model", model_name, sep = ".")
  sl_model_name <- paste("Shelf.Life", model_name, sep = ".")
  wcsl_model_name <- paste("WCSL", model_name, sep = ".")

  poi_model <- re[["POI"]][rl_index, poi_model_name]
  poi_woca <- re[["POI"]][rl_index, sl_model_name]

  ivl_side <- re[["Parameters"]][["ivl.side"]]
  wc_icpt <- re[["wc.icpt"]][, model_name]

  # <-><-><-><->

  d_res <-
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = rl_index,
                        wcsl_model_name = wcsl_model_name, plot_option = "full")

  # <-><-><-><->

  expect_equal(signif(d_res$Time, 5),
               c(29.000, 29.0000, 0.00000, 0.00000, 7.5187, 23.698, 29.000))
  expect_equal(signif(d_res$Response, 5),
               c(94.450, 105.54, 98.405, 100.45, 92.450, 92.450, 96.495))
  expect_equal(d_res$Label,
               c("LSL: 94.95 mg/kg", "USL: 105.04 mg/kg", "98.405 mg/kg ",
                 "100.45 mg/kg",
                 "7.5\n(worst case\nscenario)", "23.7\n(standard\nscenario)",
                 "LRL: 96.995 mg/kg"))
  expect_equal(d_res$Colour,
               c("black", "black", "red", "royalblue", "forestgreen",
                 "grey50", "grey0"))
})

test_that("get_text_annotation_fails", {
  re <-
    expirest_wisle(
      data = exp4, response_vbl = "Conc", time_vbl = "Month",
      batch_vbl = "Batch", rl = 97.0, rl_sf = 4, sl = c(95.0, 105.0),
      sl_sf = c(3, 4), srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "loose", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "lower")

  x_range <- c(-1, 29)
  y_range <- c(92, 107)

  sl <- re[["Limits"]][["sl"]]
  sl_sf <- re[["Limits"]][["sl.sf"]] + 1
  rl_index <- 1
  rl_sf <- re[["Limits"]][["rl.sf"]] + 1

  model_name <- re[["Model.Type"]][["type.acronym"]]
  poi_model_name <- paste("POI.Model", model_name, sep = ".")
  sl_model_name <- paste("Shelf.Life", model_name, sep = ".")
  wcsl_mn <- paste("WCSL", model_name, sep = ".")

  poi_model <- re[["POI"]][rl_index, poi_model_name]
  poi_woca <- re[["POI"]][rl_index, sl_model_name]

  ivl_side <- re[["Parameters"]][["ivl.side"]]
  wc_icpt <- re[["wc.icpt"]][, model_name]

  # <-><-><-><->

  expect_error(
    get_text_annotation(rvu = 1, x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = rl_index,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "rvu must be a string")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = 29, y_range = y_range,
                        sl = sl, sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = rl_index,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "x_range must be a vector of length 2")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = 107,
                        sl = sl, sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = rl_index,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "y_range must be a vector of length 2")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = "95", sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = rl_index,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "sl must be a numeric")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = c(5, 6, 7), sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = rl_index,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "sl must be a numeric or vector of length 1 or 2")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = c(105, 95), sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = rl_index,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "sl must be of the form")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = "4", poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = rl_index,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "sl_sf must be a positive integer")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = -4, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = rl_index,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "sl_sf must be a positive integer")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = c(4, 5, 5), poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = rl_index,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "sl_sf must be a positive integer")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = c(4.1, 5.2), poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = rl_index,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "sl_sf must be a positive integer")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = sl_sf, poi_model = "POI",
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = rl_index,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "poi_model must be a numeric")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = sl_sf, poi_model = c(24, 36),
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = rl_index,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "poi_model must be a numeric of length 1")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = "middle", poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = rl_index,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "specify ivl_side either as \"lower\", \"upper\" or \"both\"")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = "8",
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = rl_index,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "poi_woca must be a numeric")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = c(8, 16),
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = rl_index,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "poi_woca must be a numeric of length 1")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = as.matrix(re[["POI"]]), wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = rl_index,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "wisle_est must be a data frame")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = cbind(re$POI, re$POI[, 1:6]),
                        wc_icpt = wc_icpt, rl_sf = rl_sf, rl_index = rl_index,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "wisle_est must be a data frame with 24 columns")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = "100",
                        rl_sf = rl_sf, rl_index = rl_index,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "wc_icpt must be a numeric vector")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = "3", rl_index = rl_index,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "rl_sf must be a positive integer")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = -3, rl_index = rl_index,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "rl_sf must be a positive integer")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = c(3, 3), rl_index = rl_index,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "rl_sf must be a positive integer")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = 3.3, rl_index = rl_index,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "rl_sf must be a positive integer")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = "1",
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "rl_index must be a positive integer of length 1")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = 1.1,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "rl_index must be a positive integer of length 1")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = c(1, 2),
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "rl_index must be a positive integer of length 1")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = -1,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "rl_index must be between 1 and the number of rows")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = 2,
                        wcsl_model_name = wcsl_mn, plot_option = "full"),
    "rl_index must be between 1 and the number of rows")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = rl_index,
                        wcsl_model_name = 1, plot_option = "full"),
    "wcsl_model_name must be a single string")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = rl_index,
                        wcsl_model_name = c("m1", "m2"), plot_option = "full"),
    "wcsl_model_name must be a single string")
  expect_error(
    get_text_annotation(rvu = " mg/kg", x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = re[["POI"]], wc_icpt = wc_icpt,
                        rl_sf = rl_sf, rl_index = rl_index,
                        wcsl_model_name = wcsl_mn, plot_option = "incorrect"),
    "specify plot_option either as \"full\", \"lean1\", ")
})
