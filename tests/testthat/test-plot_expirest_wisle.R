context("Plot illustrating the what-if shelf life estimation (wisle)")

test_that("plot_expirest_wisle_succeeds", {
  re1 <-
    expirest_wisle(
      data = exp2, response_vbl = "Related", time_vbl = "Month",
      batch_vbl = "Batch", rl = 0.15, rl_sf = 3, sl = 0.3, sl_sf = 2,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "upper")
  re2 <-
    expirest_wisle(
      data = exp4, response_vbl = "Conc", time_vbl = "Month",
      batch_vbl = "Batch", rl = 97.0, rl_sf = 4, sl = c(95.0, 105.0),
      sl_sf = c(3, 4), srch_range = c(0, 500), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "loose", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "lower")
  re3 <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = 3.00, rl_sf = 3, sl = c(0.5, 4.5),
      sl_sf = c(1, 2), srch_range = c(0, 500), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "tight", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "upper")
  re4 <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = 3.00, rl_sf = 3, sl = 1.5, sl_sf = 2,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower")

  # <-><-><-><->

  tmp1 <-
    plot_expirest_wisle(
      model = re1, rl_index = 1, show_grouping = "yes",
      response_vbl_unit = "%", y_range = c(-0.01, 0.50),
      x_range = NULL, scenario = "standard", mtbs = "verified",
      plot_option = "full", ci_app = "line")
  tmp2 <-
    plot_expirest_wisle(
      model = re2, rl_index = 1, show_grouping = "yes",
      response_vbl_unit = " mg/kg", y_range = c(92, 107),
      x_range = c(-1, 29), scenario = "standard", mtbs = "verified",
      plot_option = "full", ci_app = "line")
  tmp3stsc <-
    plot_expirest_wisle(
      model = re2, rl_index = 1, show_grouping = "yes",
      response_vbl_unit = NULL, y_range = c(92, 107),
      x_range = c(-1, 29), scenario = "standard", mtbs = "verified",
      plot_option = "full", ci_app = "ribbon")
  tmp3wcsc <-
    plot_expirest_wisle(
      model = re2, rl_index = 1, show_grouping = "yes",
      response_vbl_unit = NULL, y_range = c(92, 107),
      x_range = NULL, scenario = "worst", mtbs = "verified",
      plot_option = "full", ci_app = "ribbon")
  suppressWarnings(tmp4 <-
    plot_expirest_wisle(
      model = re2, rl_index = 1, show_grouping = "no",
      response_vbl_unit = NULL, y_range = c(92, 107),
      x_range = c(-1, 29), scenario = "standard", mtbs = "verified",
      plot_option = "full", ci_app = "ribbon"))
    suppressWarnings(tmp4l1 <-
    plot_expirest_wisle(
      model = re2, rl_index = 1, show_grouping = "no",
      response_vbl_unit = NULL, y_range = c(92, 107),
      x_range = c(-1, 29), scenario = "standard", mtbs = "verified",
      plot_option = "lean1", ci_app = "ribbon"))
    suppressWarnings(tmp4l2 <-
    plot_expirest_wisle(
      model = re2, rl_index = 1, show_grouping = "no",
      response_vbl_unit = NULL, y_range = c(92, 107),
      x_range = c(-1, 29), scenario = "standard", mtbs = "verified",
      plot_option = "lean2", ci_app = "ribbon"))
    suppressWarnings(tmp4b1 <-
    plot_expirest_wisle(
      model = re2, rl_index = 1, show_grouping = "no",
      response_vbl_unit = NULL, y_range = c(92, 107),
      x_range = c(-1, 29), scenario = "standard", mtbs = "verified",
      plot_option = "basic1", ci_app = "ribbon"))
    suppressWarnings(tmp4b2 <-
    plot_expirest_wisle(
      model = re2, rl_index = 1, show_grouping = "no",
      response_vbl_unit = NULL, y_range = c(92, 107),
      x_range = c(-1, 29), scenario = "standard", mtbs = "verified",
      plot_option = "basic2", ci_app = "ribbon"))
    suppressWarnings(tmp5 <-
    plot_expirest_wisle(
      model = re3, rl_index = 1, show_grouping = "no",
      response_vbl_unit = "% (w/w)", y_range = c(0.2, 5.2),
      x_range = NULL, scenario = "standard", mtbs = "verified",
      plot_option = "full", ci_app = "line"))
    suppressWarnings(tmp6 <-
    plot_expirest_wisle(
      model = re4, rl_index = 1, show_grouping = "no",
      response_vbl_unit = "% (w/w)", y_range = c(0.2, 5.2),
      x_range = NULL, scenario = "standard", mtbs = "verified",
      plot_option = "full", ci_app = "line"))

  # <-><-><-><->

  expect_length(tmp1, 9)
  expect_s3_class(tmp1$Graph, c("gg", "ggplot"))

  expect_equal(signif(tmp1[["text"]][, "Month"], 5),
               c(20.000, 0.00000, 0.00000, 15.703, 19.018, 20.000))
  expect_equal(signif(tmp1[["text"]][, "Related"], 5),
               c(0.35000, 0.29822, 0.11222, 0.39000, 0.39000, 0.16400))
  expect_equal(tmp1[["text"]][, "Label"],
               c("USL: 0.340%", "0.2982% ", "0.1122% ",
                 "15.7\n(worst case scenario)", "19.0\n(standard scenario)",
                 "URL: 0.1540%"))
  expect_equal(tmp1[["text"]][, "Colour"],
               c("black", "red", "royalblue", "forestgreen", "grey50", "grey0"))

  expect_equal(tmp1[["hlines"]][1, "Related"], 0.34)
  expect_equal(tmp1[["hlines"]][1, "Item"], "USL")
  expect_equal(tmp1[["hlines"]][1, "Colour"], "black")
  expect_equal(tmp1[["hlines"]][1, "Type"], "dotted")

  expect_equal(signif(tmp1[["vlines"]][, "Month"], 5), c(15.703, 19.018))
  expect_equal(tmp1[["vlines"]][, "Item"], c("poi.woca", "poi.model"))
  expect_equal(tmp1[["vlines"]][, "Colour"], c("forestgreen", "grey50"))
  expect_equal(tmp1[["vlines"]][, "Type"], c("dashed", "dotdash"))

  expect_equal(signif(tmp1[["segments"]][, "Month.1"], 5),
               c(0.00000, 0.00000, -0.33333, -0.11111))
  expect_equal(signif(tmp1[["segments"]][, "Month.2"], 5),
               c(15.703, 20.000, -0.33333, -0.11111))
  expect_equal(signif(tmp1[["segments"]][, "Related.1"], 5),
               c(0.29822, 0.15400, 0.34000, 0.29822))
  expect_equal(signif(tmp1[["segments"]][, "Related.2"], 5),
               c(0.29822, 0.15400, 0.15400, 0.11222))
  expect_equal(tmp1[["segments"]][, "Item"],
               c("x.delta", "x.delta.shifted", "y.delta", "y.delta.shifted"))
  expect_equal(tmp1[["segments"]][, "Colour"],
               c("red", "grey0", "grey50", "grey50"))
  expect_equal(tmp1[["segments"]][, "Type"],
               c("dashed", "dotted", "solid", "solid"))
  expect_equal(tmp1[["segments"]][, "Size"], c(0.5, 0.5, 1.0, 1.0))

  expect_equal(signif(tmp1[["arrow"]][, "Month.1"], 5), -0.11111)
  expect_equal(signif(tmp1[["arrow"]][, "Month.2"], 5), -0.50000)
  expect_equal(signif(tmp1[["arrow"]][, "Related.1"], 5), 0.20522)
  expect_equal(signif(tmp1[["arrow"]][, "Related.2"], 5), 0.24700)
  expect_equal(tmp1[["arrow"]][1, "Item"], "arrow")
  expect_equal(tmp1[["arrow"]][1, "Colour"], "grey50")
  expect_equal(tmp1[["arrow"]][1, "Line.Type"], "solid")
  expect_equal(tmp1[["arrow"]][1, "Arrow.Type"], "closed")
  expect_equal(tmp1[["arrow"]][1, "Size"], 0.5)
  expect_equal(tmp1[["arrow"]][1, "Curvature"], -0.5)
  expect_equal(tmp1[["arrow"]][1, "Angle"], 90)
  expect_equal(tmp1[["arrow"]][1, "Length"], 5)

  expect_length(tmp2, 9)
  expect_s3_class(tmp2$Graph, c("gg", "ggplot"))

  expect_equal(signif(tmp2[["text"]][, "Month"], 5),
               c(29.000, 29.0000, 0.00000, 0.00000, 7.5187, 23.698, 29.000))
  expect_equal(signif(tmp2[["text"]][, "Conc"], 5),
               c(94.450, 105.54, 98.405, 100.45, 92.450, 92.450, 96.495))
  expect_equal(tmp2[["text"]][, "Label"],
               c("LSL: 94.95 mg/kg", "USL: 105.04 mg/kg", "98.405 mg/kg ",
                 "100.45 mg/kg",
                 "7.5\n(worst case scenario)", "23.7\n(standard scenario)",
                 "LRL: 96.995 mg/kg"))
  expect_equal(tmp2[["text"]][, "Colour"],
               c("black", "black", "red", "royalblue", "forestgreen",
                 "grey50", "grey0"))

  expect_equal(tmp2[["hlines"]][, "Conc"], c(94.95, 105.04))
  expect_equal(tmp2[["hlines"]][, "Item"], c("LSL", "USL"))
  expect_equal(tmp2[["hlines"]][, "Colour"], c("black", "black"))
  expect_equal(tmp2[["hlines"]][, "Type"], c("dotted", "dotted"))

  expect_equal(signif(tmp2[["vlines"]][, "Month"], 5), c(7.5187, 23.698))
  expect_equal(tmp2[["vlines"]][, "Item"], c("poi.woca", "poi.model"))
  expect_equal(tmp2[["vlines"]][, "Colour"], c("forestgreen", "grey50"))
  expect_equal(tmp2[["vlines"]][, "Type"], c("dashed", "dotdash"))

  expect_equal(signif(tmp2[["segments"]][, "Month.1"], 5),
               c(0.00000, 0.00000, -0.33333, -0.11111))
  expect_equal(signif(tmp2[["segments"]][, "Month.2"], 5),
               c(7.5187, 29.000, -0.33333, -0.11111))
  expect_equal(signif(tmp2[["segments"]][, "Conc.1"], 5),
               c(98.405, 96.995, 94.950, 98.405))
  expect_equal(signif(tmp2[["segments"]][, "Conc.2"], 5),
               c(98.405, 96.995, 96.995, 100.45))
  expect_equal(tmp2[["segments"]][, "Item"],
               c("x.delta", "x.delta.shifted", "y.delta", "y.delta.shifted"))
  expect_equal(tmp2[["segments"]][, "Colour"],
               c("red", "grey0", "grey50", "grey50"))
  expect_equal(tmp2[["segments"]][, "Type"],
               c("dashed", "dotted", "solid", "solid"))
  expect_equal(tmp2[["segments"]][, "Size"], c(0.5, 0.5, 1.0, 1.0))

  expect_equal(signif(tmp2[["arrow"]][, "Month.1"], 5), -0.11111)
  expect_equal(signif(tmp2[["arrow"]][, "Month.2"], 5), -0.50000)
  expect_equal(signif(tmp2[["arrow"]][, "Conc.1"], 5), 99.428)
  expect_equal(signif(tmp2[["arrow"]][, "Conc.2"], 5), 95.972)
  expect_equal(tmp2[["arrow"]][1, "Item"], "arrow")
  expect_equal(tmp2[["arrow"]][1, "Colour"], "grey50")
  expect_equal(tmp2[["arrow"]][1, "Line.Type"], "solid")
  expect_equal(tmp2[["arrow"]][1, "Arrow.Type"], "closed")
  expect_equal(tmp2[["arrow"]][1, "Size"], 0.5)
  expect_equal(tmp2[["arrow"]][1, "Curvature"], 0.5)
  expect_equal(tmp2[["arrow"]][1, "Angle"], 90)
  expect_equal(tmp2[["arrow"]][1, "Length"], 5)

  expect_length(tmp3stsc[["Graph"]]$labels, 11)
  expect_equal(tmp3stsc[["text"]][, "Label"],
               c("LSL: 94.95", "USL: 105.04", "98.405 ", "100.45",
                 "7.5\n(worst case scenario)", "23.7\n(standard scenario)",
                 "LRL: 96.995"))
  expect_equal(signif(tmp3stsc[["text"]][, "Month"], 5),
               c(29.000, 29.0000, 0.00000, 0.00000, 7.5187, 23.698, 29.000))

  expect_length(tmp3wcsc[["Graph"]]$labels, 11)
  expect_equal(tmp3wcsc[["text"]][, "Label"],
               c("LSL: 94.95", "USL: 105.04", "98.405 ", "100.45",
                 "7.5\n(worst case scenario)", "23.7\n(standard scenario)",
                 "LRL: 96.995"))
  expect_equal(signif(tmp3wcsc[["text"]][, "Month"], 5),
               c(10.000, 10.0000, 0.00000, 0.00000, 7.5187, 23.698, 10.000))

  expect_length(tmp4[["Graph"]]$labels, 8)
  expect_equal(tmp4[["text"]][, "Label"],
               c("LSL: 94.95", "USL: 105.04", "99.861 ", "101.91",
                 "8.3\n(worst case scenario)", "30.2\n(standard scenario)",
                 "LRL: 96.995"))
  expect_length(tmp4l1[["Graph"]]$labels, 8)
  expect_equal(tmp4l1[["text"]][, "Label"],
               c("LSL: 94.95", "USL: 105.04", "99.861 ", "101.91",
                 "8.3\n(worst case scenario)", "30.2\n(standard scenario)",
                 "LRL: 96.995"))
  expect_length(tmp4l2[["Graph"]]$labels, 8)
  expect_equal(tmp4l2[["text"]][, "Label"],
               c("LSL: 94.95", "USL: 105.04", "99.861 ", "101.91",
                 "8.3", "30.2", "LRL: 96.995"))
  expect_length(tmp4b1[["Graph"]]$labels, 5)
  expect_equal(tmp4b1[["text"]][, "Label"],
               c("LSL: 94.95", "USL: 105.04", "99.861 ", "101.91",
                 "8.3\n(worst case scenario)", "30.2\n(standard scenario)",
                 "LRL: 96.995"))
  expect_length(tmp4b2[["Graph"]]$labels, 5)
  expect_equal(tmp4b2[["text"]][, "Label"],
               c("LSL: 94.95", "USL: 105.04", "99.861 ", "101.91",
                 "8.3\n(worst case scenario)", "30.2\n(standard scenario)",
                 "LRL: 96.995"))

  expect_equal(signif(tmp5[["text"]][, "Month"], 5),
               c(100.00, 100.00, 0.00000, 0.00000, 72.505, 96.306, 100.00))
  expect_equal(signif(tmp5[["text"]][, "Moisture"], 5),
               c(0.6000, 4.6000, 3.9568, 2.4568, 5.0000, 5.0000, 4.0000))
  expect_equal(tmp5[["text"]][, "Label"],
               c("LSL: 0.5% (w/w)", "USL: 4.5% (w/w)", "3.96% (w/w) ",
                 "2.46% (w/w)",
                 "72.5\n(worst case scenario)", "96.3\n(standard scenario)",
                 "URL: 3.00% (w/w)"))
  expect_equal(tmp5[["text"]][, "Colour"],
               c("black", "black", "red", "royalblue", "forestgreen",
                 "grey50", "grey0"))

  expect_equal(tmp5[["hlines"]][, "Moisture"], c(0.5, 4.5))
  expect_equal(tmp5[["hlines"]][, "Item"], c("LSL", "USL"))
  expect_equal(tmp5[["hlines"]][, "Colour"], c("black", "black"))
  expect_equal(tmp5[["hlines"]][, "Type"], c("dotted", "dotted"))

  expect_equal(signif(tmp5[["vlines"]][, "Month"], 5), c(72.505, 96.306))
  expect_equal(tmp5[["vlines"]][, "Item"], c("poi.woca", "poi.model"))
  expect_equal(tmp5[["vlines"]][, "Colour"], c("forestgreen", "grey50"))
  expect_equal(tmp5[["vlines"]][, "Type"], c("dashed", "dotdash"))

  expect_equal(signif(tmp5[["segments"]][, "Month.1"], 5),
               c(0.00000, 0.00000, -1.0000, -0.33333))
  expect_equal(signif(tmp5[["segments"]][, "Month.2"], 5),
               c(72.505, 100.00, -1.0000, -0.33333))
  expect_equal(signif(tmp5[["segments"]][, "Moisture.1"], 5),
               c(3.9568, 3.0000, 4.5000, 3.9568))
  expect_equal(signif(tmp5[["segments"]][, "Moisture.2"], 5),
               c(3.9568, 3.0000, 3.0000, 2.4568))
  expect_equal(tmp5[["segments"]][, "Item"],
               c("x.delta", "x.delta.shifted", "y.delta", "y.delta.shifted"))
  expect_equal(tmp5[["segments"]][, "Colour"],
               c("red", "grey0", "grey50", "grey50"))
  expect_equal(tmp5[["segments"]][, "Type"],
               c("dashed", "dotted", "solid", "solid"))
  expect_equal(tmp5[["segments"]][, "Size"], c(0.5, 0.5, 1.0, 1.0))

  expect_equal(signif(tmp5[["arrow"]][, "Month.1"], 5), -0.33333)
  expect_equal(signif(tmp5[["arrow"]][, "Month.2"], 5), -1.5000)
  expect_equal(signif(tmp5[["arrow"]][, "Moisture.1"], 5), 3.2068)
  expect_equal(signif(tmp5[["arrow"]][, "Moisture.2"], 5), 3.7500)
  expect_equal(tmp5[["arrow"]][1, "Item"], "arrow")
  expect_equal(tmp5[["arrow"]][1, "Colour"], "grey50")
  expect_equal(tmp5[["arrow"]][1, "Line.Type"], "solid")
  expect_equal(tmp5[["arrow"]][1, "Arrow.Type"], "closed")
  expect_equal(tmp5[["arrow"]][1, "Size"], 0.5)
  expect_equal(tmp5[["arrow"]][1, "Curvature"], -0.5)
  expect_equal(tmp5[["arrow"]][1, "Angle"], 90)
  expect_equal(tmp5[["arrow"]][1, "Length"], 7)

  expect_equal(signif(tmp6[["text"]][, "Month"], 5),
               c(70.000, 0.00000, 0.00000, 90.540, 60.761, 70.000))
  expect_equal(signif(tmp6[["text"]][, "Moisture"], 5),
               c(1.4000, 0.95678, 2.4568, 1.0000, 1.0000, 2.9000))
  expect_equal(tmp6[["text"]][, "Label"],
               c("LSL: 1.5% (w/w)", "0.957% (w/w) ", "2.46% (w/w) ",
                 "90.5\n(worst case scenario)", "60.8\n(standard scenario)",
                 "LRL: 3.00% (w/w)"))
  expect_equal(tmp6[["text"]][, "Colour"],
               c("black", "red", "royalblue", "forestgreen",
                 "grey50", "grey0"))

  expect_equal(tmp6[["hlines"]][, "Moisture"], 1.5)
  expect_equal(tmp6[["hlines"]][, "Item"], "LSL")
  expect_equal(tmp6[["hlines"]][, "Colour"], "black")
  expect_equal(tmp6[["hlines"]][, "Type"], "dotted")

  expect_equal(signif(tmp6[["vlines"]][, "Month"], 5), c(90.540, 60.761))
  expect_equal(tmp6[["vlines"]][, "Item"], c("poi.woca", "poi.model"))
  expect_equal(tmp6[["vlines"]][, "Colour"], c("forestgreen", "grey50"))
  expect_equal(tmp6[["vlines"]][, "Type"], c("dashed", "dotdash"))

  expect_equal(signif(tmp6[["segments"]][, "Month.1"], 5),
               c(0.00000, 0.00000, -1.0000, -0.33333))
  expect_equal(signif(tmp6[["segments"]][, "Month.2"], 5),
               c(90.540, 70.000, -1.0000, -0.33333))
  expect_equal(signif(tmp6[["segments"]][, "Moisture.1"], 5),
               c(0.95678, 3.0000, 1.5000, 0.95678))
  expect_equal(signif(tmp6[["segments"]][, "Moisture.2"], 5),
               c(0.95678, 3.0000, 3.0000, 2.4568))
  expect_equal(tmp6[["segments"]][, "Item"],
               c("x.delta", "x.delta.shifted", "y.delta", "y.delta.shifted"))
  expect_equal(tmp6[["segments"]][, "Colour"],
               c("red", "grey0", "grey50", "grey50"))
  expect_equal(tmp6[["segments"]][, "Type"],
               c("dashed", "dotted", "solid", "solid"))
  expect_equal(tmp6[["segments"]][, "Size"], c(0.5, 0.5, 1.0, 1.0))

  expect_equal(signif(tmp6[["arrow"]][, "Month.1"], 5), -0.33333)
  expect_equal(signif(tmp6[["arrow"]][, "Month.2"], 5), -1.5000)
  expect_equal(signif(tmp6[["arrow"]][, "Moisture.1"], 5), 1.7068)
  expect_equal(signif(tmp6[["arrow"]][, "Moisture.2"], 5), 2.2500)
  expect_equal(tmp6[["arrow"]][1, "Item"], "arrow")
  expect_equal(tmp6[["arrow"]][1, "Colour"], "grey50")
  expect_equal(tmp6[["arrow"]][1, "Line.Type"], "solid")
  expect_equal(tmp6[["arrow"]][1, "Arrow.Type"], "closed")
  expect_equal(tmp6[["arrow"]][1, "Size"], 0.5)
  expect_equal(tmp6[["arrow"]][1, "Curvature"], 0.5)
  expect_equal(tmp6[["arrow"]][1, "Angle"], 90)
  expect_equal(tmp6[["arrow"]][1, "Length"], 7)
})

test_that("plot_expirest_wisle_succeeds_with_transformations", {
  re1 <-
    expirest_wisle(
      data = exp2, response_vbl = "Related", time_vbl = "Month",
      batch_vbl = "Batch", rl = 0.15, rl_sf = 3, sl = 0.3, sl_sf = 2,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("log", "log"), shift = c(1, 1), sf_option = "tight",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "upper")
  re2 <-
    expirest_wisle(
      data = exp2, response_vbl = "Related", time_vbl = "Month",
      batch_vbl = "Batch", rl = 0.15, rl_sf = 3, sl = 0.3, sl_sf = 2,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("sqrt", "sqrt"), shift = c(0, 0), sf_option = "tight",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "upper")
  re3 <-
    expirest_wisle(
      data = exp2, response_vbl = "Related", time_vbl = "Month",
      batch_vbl = "Batch", rl = 0.15, rl_sf = 3, sl = 0.3, sl_sf = 2,
      srch_range = c(0, 5000), alpha = 0.05, alpha_pool = 0.25,
      xform = c("sq", "sq"), shift = c(0, 0), sf_option = "tight",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "upper")

  re4 <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = 3.00, rl_sf = 3, sl = c(0.5, 4.5),
      sl_sf = c(1, 2), srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("sq", "sq"), shift = c(0, 0),
      sf_option = "tight", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "lower")
  re5 <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = 3.00, rl_sf = 3, sl = c(0.5, 4.5),
      sl_sf = c(1, 2), srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("sq", "no"), shift = c(0, 0),
      sf_option = "tight", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "lower")
  re6 <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = 3.00, rl_sf = 3, sl = c(0.5, 4.5),
      sl_sf = c(1, 2), srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "sq"), shift = c(0, 0),
      sf_option = "tight", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "lower")
  re7 <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = 3.00, rl_sf = 3, sl = c(0.5, 4.5),
      sl_sf = c(1, 2), srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("sq", "sq"), shift = c(0, 0),
      sf_option = "tight", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "upper")
  re8 <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = 3.00, rl_sf = 3, sl = c(0.5, 4.5),
      sl_sf = c(1, 2), srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("sq", "no"), shift = c(0, 0),
      sf_option = "tight", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "upper")
  re9 <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = 3.00, rl_sf = 3, sl = c(0.5, 4.5),
      sl_sf = c(1, 2), srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "sq"), shift = c(0, 0),
      sf_option = "tight", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "upper")

  # <-><-><-><->

  tmp1 <-
    plot_expirest_wisle(
      model = re1, rl_index = 1, show_grouping = "yes",
      response_vbl_unit = "%", y_range = c(-0.01, 0.50),
      x_range = NULL, scenario = "standard", mtbs = "verified",
      plot_option = "full", ci_app = "line")
  tmp2 <-
    plot_expirest_wisle(
      model = re2, rl_index = 1, show_grouping = "yes",
      response_vbl_unit = "%", y_range = c(-0.01, 0.50),
      x_range = NULL, scenario = "standard", mtbs = "verified",
      plot_option = "full", ci_app = "line")
  tmp3 <-
    plot_expirest_wisle(
      model = re3, rl_index = 1, show_grouping = "yes",
      response_vbl_unit = "%", y_range = c(-0.01, 0.50),
      x_range = NULL, scenario = "standard", mtbs = "verified",
      plot_option = "full", ci_app = "line")

  suppressWarnings(tmp4 <-
    plot_expirest_wisle(
      model = re4, rl_index = 1, show_grouping = "no",
      response_vbl_unit = "% (w/w)", y_range = c(0.2, 5.2),
      x_range = NULL, scenario = "standard", mtbs = "verified",
      plot_option = "full", ci_app = "line"))
    suppressWarnings(tmp5 <-
    plot_expirest_wisle(
      model = re5, rl_index = 1, show_grouping = "no",
      response_vbl_unit = "% (w/w)", y_range = c(0.2, 5.2),
      x_range = NULL, scenario = "standard", mtbs = "verified",
      plot_option = "full", ci_app = "ribbon"))
    suppressWarnings(tmp6 <-
    plot_expirest_wisle(
      model = re6, rl_index = 1, show_grouping = "no",
      response_vbl_unit = "% (w/w)", y_range = c(0.2, 5.2),
      x_range = NULL, scenario = "standard", mtbs = "verified",
      plot_option = "full", ci_app = "ribbon"))
    suppressWarnings(tmp7 <-
    plot_expirest_wisle(
      model = re7, rl_index = 1, show_grouping = "no",
      response_vbl_unit = "% (w/w)", y_range = c(0.2, 5.2),
      x_range = NULL, scenario = "standard", mtbs = "verified",
      plot_option = "lean1", ci_app = "line"))
    suppressWarnings(tmp8 <-
    plot_expirest_wisle(
      model = re8, rl_index = 1, show_grouping = "no",
      response_vbl_unit = "% (w/w)", y_range = c(0.2, 5.2),
      x_range = NULL, scenario = "standard", mtbs = "verified",
      plot_option = "lean1", ci_app = "ribbon"))
    suppressWarnings(tmp9 <-
    plot_expirest_wisle(
      model = re9, rl_index = 1, show_grouping = "no",
      response_vbl_unit = "% (w/w)", y_range = c(0.2, 5.2),
      x_range = NULL, scenario = "standard", mtbs = "verified",
      plot_option = "lean1", ci_app = "ribbon"))

  # <-><-><-><->

  expect_length(tmp1, 9)
  expect_s3_class(tmp1$Graph, c("gg", "ggplot"))
  expect_equal(signif(tmp1[["text"]][, "Month"], 5),
               c(40.000, 0.00000, 0.00000, 10.577, 33.747, 40.000))
  expect_equal(signif(tmp1[["text"]][, "Related"], 5),
               c(0.31000, 0.23388, 0.091511, 0.35000, 0.35000, 0.16000))

  expect_length(tmp2, 9)
  expect_s3_class(tmp2$Graph, c("gg", "ggplot"))
  expect_equal(signif(tmp2[["text"]][, "Month"], 5),
               c(30.000, 0.00000, 0.00000, 9.1088, 20.582, 30.000))
  expect_equal(signif(tmp2[["text"]][, "Related"], 5),
               c(0.31000, 0.22314, 0.097316, 0.35000, 0.35000, 0.16000))

  expect_length(tmp3, 9)
  expect_s3_class(tmp3$Graph, c("gg", "ggplot"))
  expect_equal(signif(tmp3[["text"]][, "Month"], 5),
               c(20.000, 0.00000, 0.00000, 14.222, 14.709, 20.000))
  expect_equal(signif(tmp3[["text"]][, "Related"], 5),
               c(0.31000, 0.29157, 0.13235, 0.35000, 0.35000, 0.16000))

  expect_length(tmp4, 9)
  expect_s3_class(tmp4$Graph, c("gg", "ggplot"))
  expect_equal(signif(tmp4[["text"]][, "Month"], 5),
               c(50.000, 50.000, 0.00000, 0.00000, 51.116, 43.224, 50.000))
  expect_equal(signif(tmp4[["text"]][, "Moisture"], 5),
               c(0.40000, 4.6000, NA, 2.5240, 0.0000, 0.0000, 2.9000))

  expect_length(tmp5, 9)
  expect_s3_class(tmp5$Graph, c("gg", "ggplot"))
  expect_equal(signif(tmp5[["text"]][, "Month"], 5),
               c(60.000, 60.000, 0.00000, 0.00000, 57.698, 51.516, 60.000))
  expect_equal(signif(tmp5[["text"]][, "Moisture"], 5),
               c(0.40000, 4.6000, -0.027167, 2.4728, 0.0000, 0.0000, 2.9000))

  expect_length(tmp6, 9)
  expect_s3_class(tmp6$Graph, c("gg", "ggplot"))
  expect_equal(signif(tmp6[["text"]][, "Month"], 5),
               c(90.000, 90.000, 0.00000, 0.00000, 116.32, 82.919, 90.000))
  expect_equal(signif(tmp6[["text"]][, "Moisture"], 5),
               c(0.40000, 4.6000, NA, 2.5015, 0.0000, 0.0000, 2.9000))

  expect_length(tmp7, 9)
  expect_s3_class(tmp7$Graph, c("gg", "ggplot"))
  expect_equal(signif(tmp7[["text"]][, "Month"], 5),
               c(60.000, 60.000, 0.00000, 0.00000, 50.539, 55.908, 60.000))
  expect_equal(signif(tmp7[["text"]][, "Moisture"], 5),
               c(0.60000, 4.6000, 4.1977, 2.5240, 5.0000, 5.0000, 4.0000))

  expect_length(tmp8, 9)
  expect_s3_class(tmp8$Graph, c("gg", "ggplot"))
  expect_equal(signif(tmp8[["text"]][, "Month"], 5),
               c(60.000, 60.000, 0.00000, 0.00000, 43.983, 50.735, 60.000))
  expect_equal(signif(tmp8[["text"]][, "Moisture"], 5),
               c(0.60000, 4.6000, 3.9728, 2.4728, 5.0000, 5.0000, 4.0000))

  expect_length(tmp9, 9)
  expect_s3_class(tmp9$Graph, c("gg", "ggplot"))
  expect_equal(signif(tmp9[["text"]][, "Month"], 5),
               c(120.000, 120.000, 0.00000, 0.00000, 95.353, 117.04, 120.000))
  expect_equal(signif(tmp9[["text"]][, "Moisture"], 5),
               c(0.60000, 4.6000, 4.1842, 2.5015, 5.0000, 5.0000, 4.0000))
})

test_that("plot_expirest_wisle_fails", {
  t_dat <- exp1[exp1$Batch %in% c("b2", "b5", "b7"), ]

  # <-><-><-><->

  re1 <-
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = c(95, 105),
      sl_sf = c(3, 4), srch_range = c(0, 500), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "loose", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "lower")

  ree <- re1
  class(ree) <- "expirest"

  re2 <- suppressWarnings(
    expirest_wisle(
      data = exp4, response_vbl = "Conc", time_vbl = "Month",
      batch_vbl = "Batch", rl = 96, rl_sf = 2, sl = c(95, 105),
      sl_sf = c(2, 3), srch_range = c(0, 3), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "loose", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "lower")
  )

  # <-><-><-><->

  expect_error(
    plot_expirest_wisle(
      model = ree, rl_index = 1, show_grouping = "yes",
      response_vbl_unit = NULL, y_range = c(93, 107), x_range = NULL,
      scenario = "standard", mtbs = "verified", plot_option = "full",
      ci_app = "line"),
    "model must be an object of class expirest_wisle")
  expect_error(
    plot_expirest_wisle(
      model = re1, rl_index = "x", show_grouping = "yes",
      response_vbl_unit = NULL, y_range = c(93, 107), x_range = NULL,
      scenario = "standard", mtbs = "verified", plot_option = "full",
      ci_app = "line"),
    "rl_index must be a positive integer of length 1")
  expect_error(
    plot_expirest_wisle(
      model = re1, rl_index = 1.1, show_grouping = "yes",
      response_vbl_unit = NULL, y_range = c(93, 107), x_range = NULL,
      scenario = "standard", mtbs = "verified", plot_option = "full",
      ci_app = "line"),
    "rl_index must be a positive integer of length 1")
  expect_error(
    plot_expirest_wisle(
      model = re1, rl_index = c(1, 2), show_grouping = "yes",
      response_vbl_unit = NULL, y_range = c(93, 107), x_range = NULL,
      scenario = "standard", mtbs = "verified", plot_option = "full",
      ci_app = "line"),
    "rl_index must be a positive integer of length 1")
  expect_error(
    plot_expirest_wisle(
      model = re1, rl_index = -1, show_grouping = "yes",
      response_vbl_unit = NULL, y_range = c(93, 107), x_range = NULL,
      scenario = "standard", plot_option = "full", ci_app = "line"),
    "rl_index must be between 1 and the number of rl values")
  expect_error(
    plot_expirest_wisle(
      model = re1, rl_index = 2, show_grouping = "yes",
      response_vbl_unit = NULL, y_range = c(93, 107), x_range = NULL,
      scenario = "standard", mtbs = "verified", plot_option = "full",
      ci_app = "line"),
    "rl_index must be between 1 and the number of rl values")
  expect_error(
    plot_expirest_wisle(
      model = re1, rl_index = 1, show_grouping = "yes",
      response_vbl_unit = 1, y_range = c(93, 107), x_range = NULL,
      scenario = "standard", mtbs = "verified", plot_option = "full",
      ci_app = "line"),
    "response_vbl_unit must be a string")
  expect_error(
    plot_expirest_wisle(
      model = re1, rl_index = 1, show_grouping = "yes",
      response_vbl_unit = NULL, y_range = "range", x_range = NULL,
      scenario = "standard", mtbs = "verified", plot_option = "full",
      ci_app = "line"),
    "y_range must be a vector of length 2")
  expect_error(
    plot_expirest_wisle(
      model = re1, rl_index = 1, show_grouping = "yes",
      response_vbl_unit = NULL, y_range = 107, x_range = NULL,
      scenario = "standard", mtbs = "verified", plot_option = "full",
      ci_app = "line"),
    "y_range must be a vector of length 2")
  expect_error(
    plot_expirest_wisle(
      model = re1, rl_index = 1, show_grouping = "yes",
      response_vbl_unit = NULL, y_range = c(93, 107), x_range = "range",
      scenario = "standard", mtbs = "verified", plot_option = "full",
      ci_app = "line"),
    "x_range must be a vector of length 2")
  expect_error(
    plot_expirest_wisle(
      model = re1, rl_index = 1, show_grouping = "yes",
      response_vbl_unit = NULL, y_range = c(93, 107), x_range = 36,
      scenario = "standard", mtbs = "verified", plot_option = "full",
      ci_app = "line"),
    "x_range must be a vector of length 2")
  expect_error(
    plot_expirest_wisle(
      model = re1, rl_index = 1, show_grouping = "yes",
      response_vbl_unit = NULL, y_range = c(93, 107), x_range = NULL,
      scenario = "incorrect", mtbs = "verified", plot_option = "full",
      ci_app = "line"),
    "specify scenario either as \"standard\" or \"worst\"")
  expect_error(
    plot_expirest_wisle(
      model = re1, rl_index = 1, show_grouping = "yes",
      response_vbl_unit = NULL, y_range = c(93, 107), x_range = NULL,
      scenario = "standard", mtbs = "incorrect", plot_option = "full",
      ci_app = "line"),
    "specify mtbs either as \"verified\", \"cics\", \"dics\", ")
  expect_error(
    plot_expirest_wisle(
      model = re1, rl_index = 1, show_grouping = "yes",
      response_vbl_unit = NULL, y_range = c(93, 107), x_range = NULL,
      scenario = "standard", mtbs = "verified", plot_option = "incorrect",
      ci_app = "line"),
    "specify plot_option either as \"full\", \"lean1\"")
  expect_error(
    plot_expirest_wisle(
      model = re1, rl_index = 1, show_grouping = "yes",
      response_vbl_unit = NULL, y_range = c(93, 107), x_range = NULL,
      scenario = "standard", mtbs = "verified", plot_option = "full",
      ci_app = "points"),
    "specify ci_app either as \"line\" or \"ribbon\"")
  expect_error(
    plot_expirest_wisle(
      model = re2, rl_index = 1, show_grouping = "yes",
      response_vbl_unit = NULL, y_range = c(93, 107), x_range = NULL,
      scenario = "standard", mtbs = "verified", plot_option = "full",
      ci_app = "line"),
    "Expiry determination was not successful")
})
