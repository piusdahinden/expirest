context("Plot illustrating the ordinary shelf life estimation (osle)")

test_that("plot_expirest_osle_succeeds", {
  re1 <-
    expirest_osle(
      data = exp2, response_vbl = "Related", time_vbl = "Month",
      batch_vbl = "Batch", sl = 0.3, sl_sf = 2, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "loose", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper")
  re2 <-
    expirest_osle(
      data = exp4, response_vbl = "Conc", time_vbl = "Month",
      batch_vbl = "Batch", sl = c(95.0, 105.0), sl_sf = c(3, 4),
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower")
  re3 <-
    expirest_osle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", sl = c(0.5, 4.5), sl_sf = c(1, 2),
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "upper")
  re4 <-
    expirest_osle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", sl = 1.5, sl_sf = 2, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("no", "no"),
      shift = c(0, 0), sf_option = "tight", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "lower")

  # <-><-><-><->

  tmp1 <-
    plot_expirest_osle(
      model = re1, show_grouping = "yes", response_vbl_unit = "%",
      y_range = c(-0.01, 0.5), x_range = NULL, mtbs = "verified",
      plot_option = "full", ci_app = "line")
  tmp2 <-
    plot_expirest_osle(
      model = re2, show_grouping = "yes", response_vbl_unit = " mg/kg",
      y_range = c(92, 107), x_range = c(-1, 29), mtbs = "verified",
      plot_option = "full", ci_app = "line")
  tmp3 <-
    plot_expirest_osle(
      model = re2, show_grouping = "yes", response_vbl_unit = NULL,
      y_range = c(92, 107), x_range = c(-1, 29), mtbs = "verified",
      plot_option = "full", ci_app = "ribbon")
  suppressWarnings(tmp4 <-
    plot_expirest_osle(
      model = re2, show_grouping = "no", response_vbl_unit = NULL,
      y_range = c(92, 107), x_range = c(-1, 29), mtbs = "verified",
      plot_option = "full", ci_app = "ribbon"))
  suppressWarnings(tmp5 <-
    plot_expirest_osle(
      model = re3, show_grouping = "no", response_vbl_unit = "% (w/w)",
      y_range = c(0.2, 5.2), x_range = NULL, mtbs = "verified",
      plot_option = "full", ci_app = "line"))
  suppressWarnings(tmp6 <-
    plot_expirest_osle(
      model = re4, show_grouping = "no", response_vbl_unit = "% (w/w)",
      y_range = c(0.2, 5.2), x_range = NULL, mtbs = "verified",
      plot_option = "full", ci_app = "line"))

  # <-><-><-><->

  expect_length(tmp1, 7)
  expect_s3_class(tmp1$Graph, c("gg", "ggplot"))

  expect_equal(signif(tmp1[["text"]][, "Month"], 5), c(20.000, 19.018))
  expect_equal(tmp1[["text"]][, "Related"], c(0.35, 0.39))
  expect_equal(tmp1[["text"]][, "Label"], c("USL: 0.340%", "19.0"))
  expect_equal(tmp1[["text"]][, "Colour"], c("black", "forestgreen"))
  expect_equal(tmp1[["hlines"]][1, "Related"], 0.34)
  expect_equal(tmp1[["hlines"]][1, "Item"], "USL")
  expect_equal(tmp1[["hlines"]][1, "Colour"], "black")
  expect_equal(tmp1[["hlines"]][1, "Type"], "dotted")
  expect_equal(signif(tmp1[["vlines"]][, "Month"], 5), 19.018)
  expect_equal(tmp1[["vlines"]][, "Item"], "poi.model")
  expect_equal(tmp1[["vlines"]][, "Colour"], "forestgreen")
  expect_equal(tmp1[["vlines"]][, "Type"], "dotdash")

  expect_length(tmp2, 7)
  expect_s3_class(tmp2$Graph, c("gg", "ggplot"))

  expect_equal(signif(tmp2[["text"]][, "Month"], 5), c(29.000, 29.000, 23.698))
  expect_equal(tmp2[["text"]][, "Conc"], c(94.45, 105.54, 92.45))
  expect_equal(tmp2[["text"]][, "Label"],
               c("LSL: 94.95 mg/kg", "USL: 105.04 mg/kg", "23.7"))
  expect_equal(tmp2[["text"]][, "Colour"], c("black", "black", "forestgreen"))
  expect_equal(tmp2[["hlines"]][, "Conc"], c(94.95, 105.04))
  expect_equal(tmp2[["hlines"]][, "Item"], c("LSL", "USL"))
  expect_equal(tmp2[["hlines"]][, "Colour"], c("black", "black"))
  expect_equal(tmp2[["hlines"]][, "Type"], c("dotted", "dotted"))
  expect_equal(signif(tmp2[["vlines"]][, "Month"], 5), 23.698)
  expect_equal(tmp2[["vlines"]][, "Item"], "poi.model")
  expect_equal(tmp2[["vlines"]][, "Colour"], "forestgreen")
  expect_equal(tmp2[["vlines"]][, "Type"], "dotdash")

  expect_length(tmp3[["Graph"]]$labels, 9)
  expect_equal(tmp3[["text"]][, "Label"],
               c("LSL: 94.95", "USL: 105.04", "23.7"))

  expect_length(tmp4[["Graph"]]$labels, 6)
  expect_equal(tmp4[["text"]][, "Label"],
               c("LSL: 94.95", "USL: 105.04", "30.2"))

  expect_equal(signif(tmp5[["text"]][, "Month"], 5),
               c(100.00, 100.00, 96.306))
  expect_equal(signif(tmp5[["text"]][, "Moisture"], 5),
               c(0.40000, 4.6000, 0.00000))
  expect_equal(tmp5[["text"]][, "Label"],
               c("LSL: 0.5% (w/w)", "USL: 4.5% (w/w)", "96.3"))
  expect_equal(tmp5[["text"]][, "Colour"], c("black", "black", "forestgreen"))
  expect_equal(tmp5[["hlines"]][, "Moisture"], c(0.5, 4.5))
  expect_equal(tmp5[["hlines"]][, "Item"], c("LSL", "USL"))
  expect_equal(tmp5[["hlines"]][, "Colour"], c("black", "black"))
  expect_equal(tmp5[["hlines"]][, "Type"], c("dotted", "dotted"))
  expect_equal(signif(tmp5[["vlines"]][, "Month"], 5), 96.306)
  expect_equal(tmp5[["vlines"]][, "Item"], "poi.model")
  expect_equal(tmp5[["vlines"]][, "Colour"], "forestgreen")
  expect_equal(tmp5[["vlines"]][, "Type"], "dotdash")

  expect_equal(signif(tmp6[["text"]][, "Month"], 5), c(70.000, 60.761))
  expect_equal(tmp6[["text"]][, "Moisture"], c(1.4, 1.0))
  expect_equal(tmp6[["text"]][, "Label"], c("LSL: 1.5% (w/w)", "60.8"))
  expect_equal(tmp6[["hlines"]][, "Moisture"], 1.5)
  expect_equal(tmp6[["hlines"]][, "Item"], "LSL")
  expect_equal(tmp6[["hlines"]][, "Colour"], "black")
  expect_equal(tmp6[["hlines"]][, "Type"], "dotted")
  expect_equal(signif(tmp6[["vlines"]][, "Month"], 5), 60.761)
  expect_equal(tmp6[["vlines"]][, "Item"], "poi.model")
  expect_equal(tmp6[["vlines"]][, "Colour"], "forestgreen")
  expect_equal(tmp6[["vlines"]][, "Type"], "dotdash")
})

test_that("plot_expirest_osle_succeeds_with_transformations", {
  re1 <-
    expirest_osle(
      data = exp2, response_vbl = "Related", time_vbl = "Month",
      batch_vbl = "Batch", sl = 0.3, sl_sf = 2, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("log", "log"),
      shift = c(1, 0), sf_option = "tight", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper")
  re2 <-
    expirest_osle(
      data = exp2, response_vbl = "Related", time_vbl = "Month",
      batch_vbl = "Batch", sl = 0.3, sl_sf = 2, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = c("sqrt", "sqrt"),
      shift = c(0, 0), sf_option = "tight", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper")
  re3 <-
    expirest_osle(
      data = exp2, response_vbl = "Related", time_vbl = "Month",
      batch_vbl = "Batch", sl = 0.3, sl_sf = 2, srch_range = c(0, 5000),
      alpha = 0.05, alpha_pool = 0.25, xform = c("sq", "sq"),
      shift = c(0, 0), sf_option = "tight", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = "upper")

  re4 <-
    expirest_osle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", sl = c(0.5, 4.5), sl_sf = c(2, 2),
      srch_range = c(0, 5000), alpha = 0.05, alpha_pool = 0.25,
      xform = c("sq", "sq"), shift = c(0, 0), sf_option = "tight",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower")
  re5 <-
    expirest_osle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", sl = c(0.5, 4.5), sl_sf = c(2, 2),
      srch_range = c(0, 5000), alpha = 0.05, alpha_pool = 0.25,
      xform = c("sq", "no"), shift = c(0, 0), sf_option = "tight",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower")
  re6 <-
    expirest_osle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", sl = c(0.5, 4.5), sl_sf = c(2, 2),
      srch_range = c(0, 5000), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "sq"), shift = c(0, 0), sf_option = "tight",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower")
  re7 <-
    expirest_osle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", sl = c(0.5, 4.5), sl_sf = c(2, 2),
      srch_range = c(0, 5000), alpha = 0.05, alpha_pool = 0.25,
      xform = c("sq", "sq"), shift = c(0, 0), sf_option = "tight",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "upper")
  re8 <-
    expirest_osle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", sl = c(0.5, 4.5), sl_sf = c(2, 2),
      srch_range = c(0, 5000), alpha = 0.05, alpha_pool = 0.25,
      xform = c("sq", "no"), shift = c(0, 0), sf_option = "tight",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "upper")
  re9 <-
    expirest_osle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", sl = c(0.5, 4.5), sl_sf = c(2, 2),
      srch_range = c(0, 5000), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "sq"), shift = c(0, 0), sf_option = "tight",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "upper")

  # <-><-><-><->

  tmp1 <-
    plot_expirest_osle(
      model = re1, show_grouping = "yes", response_vbl_unit = "%",
      y_range = c(-0.01, 0.5), x_range = NULL, mtbs = "verified",
      plot_option = "full", ci_app = "line")
  tmp2 <-
    plot_expirest_osle(
      model = re2, show_grouping = "yes", response_vbl_unit = "%",
      y_range = c(-0.01, 0.5), x_range = NULL, mtbs = "verified",
      plot_option = "full", ci_app = "line")
  tmp3 <-
    plot_expirest_osle(
      model = re3, show_grouping = "yes", response_vbl_unit = "%",
      y_range = c(-0.01, 0.5), x_range = NULL, mtbs = "verified",
      plot_option = "full", ci_app = "line")

  suppressWarnings(tmp4 <-
    plot_expirest_osle(
      model = re4, show_grouping = "no", response_vbl_unit = "% (w/w)",
      y_range = c(0.2, 5.2), x_range = NULL, mtbs = "verified",
      plot_option = "full", ci_app = "line"))
  suppressWarnings(tmp5 <-
    plot_expirest_osle(
      model = re5, show_grouping = "no", response_vbl_unit = "% (w/w)",
      y_range = c(0.2, 5.2), x_range = NULL, mtbs = "verified",
      plot_option = "full", ci_app = "ribbon"))
  suppressWarnings(tmp6 <-
    plot_expirest_osle(
      model = re5, show_grouping = "no", response_vbl_unit = "% (w/w)",
      y_range = c(0.2, 5.2), x_range = NULL, mtbs = "verified",
      plot_option = "full", ci_app = "ribbon"))
  suppressWarnings(tmp7 <-
    plot_expirest_osle(
      model = re7, show_grouping = "no", response_vbl_unit = "% (w/w)",
      y_range = c(0.2, 5.2), x_range = NULL, mtbs = "verified",
      plot_option = "lean", ci_app = "line"))
  suppressWarnings(tmp8 <-
    plot_expirest_osle(
      model = re8, show_grouping = "no", response_vbl_unit = "% (w/w)",
      y_range = c(0.2, 5.2), x_range = NULL, mtbs = "verified",
      plot_option = "lean", ci_app = "ribbon"))
  suppressWarnings(tmp9 <-
    plot_expirest_osle(
      model = re9, show_grouping = "no", response_vbl_unit = "% (w/w)",
      y_range = c(0.2, 5.2), x_range = NULL, mtbs = "verified",
      plot_option = "lean", ci_app = "ribbon"))

  # <-><-><-><->

  expect_length(tmp1, 7)
  expect_s3_class(tmp1$Graph, c("gg", "ggplot"))
  expect_equal(signif(tmp1[["text"]][, "Month"], 5), c(30.000, 22.993))
  expect_equal(tmp1[["text"]][, "Related"], c(0.31, 0.35))

  expect_length(tmp2, 7)
  expect_s3_class(tmp2$Graph, c("gg", "ggplot"))
  expect_equal(signif(tmp2[["text"]][, "Month"], 5), c(30.000, 20.582))
  expect_equal(tmp2[["text"]][, "Related"], c(0.31, 0.35))

  expect_length(tmp3, 7)
  expect_s3_class(tmp3$Graph, c("gg", "ggplot"))
  expect_equal(signif(tmp3[["text"]][, "Month"], 5), c(20.000, 14.709))
  expect_equal(tmp3[["text"]][, "Related"], c(0.31, 0.35))

  expect_length(tmp4, 7)
  expect_s3_class(tmp4$Graph, c("gg", "ggplot"))
  expect_equal(signif(tmp4[["text"]][, "Month"], 5), c(50.000, 50.000, 43.224))
  expect_equal(tmp4[["text"]][, "Moisture"], c(0.4, 4.6, 0.0))

  expect_length(tmp5, 7)
  expect_s3_class(tmp5$Graph, c("gg", "ggplot"))
  expect_equal(signif(tmp5[["text"]][, "Month"], 5), c(60.000, 60.000, 51.516))
  expect_equal(tmp5[["text"]][, "Moisture"], c(0.4, 4.6, 0.0))

  expect_length(tmp6, 7)
  expect_s3_class(tmp6$Graph, c("gg", "ggplot"))
  expect_equal(signif(tmp6[["text"]][, "Month"], 5), c(60.000, 60.000, 51.516))
  expect_equal(tmp6[["text"]][, "Moisture"], c(0.4, 4.6, 0.0))

  expect_length(tmp7, 7)
  expect_s3_class(tmp7$Graph, c("gg", "ggplot"))
  expect_equal(signif(tmp7[["text"]][, "Month"], 5), c(60.000, 60.000, 55.908))
  expect_equal(tmp7[["text"]][, "Moisture"], c(0.4, 4.6, 0.0))

  expect_length(tmp8, 7)
  expect_s3_class(tmp8$Graph, c("gg", "ggplot"))
  expect_equal(signif(tmp8[["text"]][, "Month"], 5), c(60.000, 60.000, 50.735))
  expect_equal(tmp8[["text"]][, "Moisture"], c(0.4, 4.6, 0.0))

  expect_length(tmp9, 7)
  expect_s3_class(tmp9$Graph, c("gg", "ggplot"))
  expect_equal(signif(tmp9[["text"]][, "Month"], 5),
               c(120.000, 120.000, 117.04))
  expect_equal(tmp9[["text"]][, "Moisture"], c(0.4, 4.6, 0.0))
})

test_that("plot_expirest_osle_fails", {
  t_dat <- exp1[exp1$Batch %in% c("b2", "b5", "b7"), ]

  usl <- 105
  lsl <- 95

  # <-><-><-><->

  re1 <-
    expirest_osle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = c(95, 105),
      sl_sf = c(3, 4), srch_range = c(0, 500), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "loose", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "lower")

  ree <- re1
  class(ree) <- "expirest"

  re2 <- suppressWarnings(
    expirest_osle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = 3, rl_sf = 2, sl = c(1.5, 3.5),
      sl_sf = c(2, 2), srch_range = c(0, 50), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "loose", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "lower")
  )

  # <-><-><-><->

  expect_error(
    plot_expirest_osle(
      model = ree, show_grouping = "yes", response_vbl_unit = NULL,
      y_range = c(93, 107), x_range = NULL, mtbs = "verified",
      plot_option = "full", ci_app = "line"),
    "model must be an object of class expirest_osle")
  expect_error(
    plot_expirest_osle(
      model = re1, show_grouping = "yes", response_vbl_unit = 1,
      y_range = c(93, 107), x_range = NULL, mtbs = "verified",
      plot_option = "full", ci_app = "line"),
    "response_vbl_unit must be a string")
  expect_error(
    plot_expirest_osle(
      model = re1, show_grouping = "yes", response_vbl_unit = NULL,
      y_range = "range", x_range = NULL, mtbs = "verified",
      plot_option = "full", ci_app = "line"),
    "y_range must be a vector of length 2")
  expect_error(
    plot_expirest_osle(
      model = re1, show_grouping = "yes", response_vbl_unit = NULL,
      y_range = 107, x_range = NULL, mtbs = "verified",
      plot_option = "full", ci_app = "line"),
    "y_range must be a vector of length 2")
  expect_error(
    plot_expirest_osle(
      model = re1, show_grouping = "yes", response_vbl_unit = NULL,
      y_range = c(93, 107), x_range = "range", mtbs = "verified",
      plot_option = "full", ci_app = "line"),
    "x_range must be a vector of length 2")
  expect_error(
    plot_expirest_osle(
      model = re1, show_grouping = "yes", response_vbl_unit = NULL,
      y_range = c(93, 107), x_range = 36, mtbs = "verified",
      plot_option = "full", ci_app = "line"),
    "x_range must be a vector of length 2")
  expect_error(
    plot_expirest_osle(
      model = re1, show_grouping = "yes", response_vbl_unit = NULL,
      y_range = c(93, 107), x_range = NULL, mtbs = "incorrect",
      plot_option = "incorrect", ci_app = "line"),
    "specify mtbs either as \"verified\", \"cics\", \"dics\", ")
  expect_error(
    plot_expirest_osle(
      model = re1, show_grouping = "yes", response_vbl_unit = NULL,
      y_range = c(93, 107), x_range = NULL, mtbs = "verified",
      plot_option = "incorrect", ci_app = "line"),
    "specify plot_option either as \"full\" or \"lean\"")
  expect_error(
    plot_expirest_osle(
      model = re1, show_grouping = "yes", response_vbl_unit = NULL,
      y_range = c(93, 107), x_range = NULL, mtbs = "verified",
      plot_option = "full", ci_app = "points"),
    "specify ci_app either as \"line\" or \"ribbon\"")
  expect_error(
    plot_expirest_osle(
      model = re2, show_grouping = "yes", response_vbl_unit = NULL,
      y_range = c(93, 107), x_range = NULL, mtbs = "verified",
      plot_option = "full", ci_app = "line"),
    "Expiry determination was not successful")
})
