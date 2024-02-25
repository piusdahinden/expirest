context("Get text annotation elements for graphical output")

test_that("get_text_annotation_succeeds", {
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

  d_res1 <-
    get_text_annotation(model = re1, rvu = " mg/kg", x_range = c(-1, 29),
                        y_range = c(92, 107), plot_option = "full",
                        mtbs = "verified")
  d_res2 <-
    get_text_annotation(model = re2, rvu = " mg/kg", x_range = c(-1, 29),
                        y_range = c(92, 107), rl_index = 1,
                        plot_option = "full", mtbs = "verified")

  # <-><-><-><->

  expect_equal(signif(d_res1$Month, 5), c(29.000, 23.698))
  expect_equal(signif(d_res1$Conc, 5), c(94.450, 92.450))
  expect_equal(d_res1$Label, c("LSL: 94.95 mg/kg", "23.7"))
  expect_equal(d_res1$Colour, c("black", "forestgreen"))

  expect_equal(signif(d_res2$Month, 5),
               c(29.000, 29.0000, 0.00000, 0.00000, 7.5187, 23.698, 29.000))
  expect_equal(signif(d_res2$Conc, 5),
               c(94.450, 105.54, 98.405, 100.45, 92.450, 92.450, 96.495))
  expect_equal(d_res2$Label,
               c("LSL: 94.95 mg/kg", "USL: 105.04 mg/kg", "98.405 mg/kg ",
                 "100.45 mg/kg",
                 "7.5\n(worst case\nscenario)", "23.7\n(standard\nscenario)",
                 "LRL: 96.995 mg/kg"))
  expect_equal(d_res2$Colour,
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
  rl_index <- 1

  # <-><-><-><->

  expect_error(
    get_text_annotation(model = "re", rvu = 1, x_range = x_range,
                        y_range = y_range, rl_index = rl_index,
                        mtbs = "verified", plot_option = "full"),
    "model must be an object of class expirest_osle or expirest_wisle")
  expect_error(
    get_text_annotation(model = re, rvu = 1, x_range = x_range,
                        y_range = y_range, rl_index = rl_index,
                        mtbs = "verified", plot_option = "full"),
    "rvu must be a string")
  expect_error(
    get_text_annotation(model = re, rvu = " mg/kg", x_range = c("1", "2"),
                        y_range = y_range, rl_index = rl_index,
                        mtbs = "verified", plot_option = "full"),
    "x_range must be a numeric vector of length 2")
  expect_error(
    get_text_annotation(model = re, rvu = " mg/kg", x_range = 29,
                        y_range = y_range, rl_index = rl_index,
                        mtbs = "verified", plot_option = "full"),
    "x_range must be a numeric vector of length 2")
  expect_error(
    get_text_annotation(model = re, rvu = " mg/kg", x_range = x_range,
                        y_range = "y_range", rl_index = rl_index,
                        mtbs = "verified", plot_option = "full"),
    "y_range must be a numeric vector of length 2")
  expect_error(
    get_text_annotation(model = re, rvu = " mg/kg", x_range = x_range,
                        y_range = 107, rl_index = rl_index,
                        plot_option = "full"),
    "y_range must be a numeric vector of length 2")
  expect_error(
    get_text_annotation(model = re, rvu = " mg/kg", x_range = x_range,
                        y_range = y_range, rl_index = "1",
                        mtbs = "verified", plot_option = "full"),
    "rl_index must be a positive integer of length 1")
  expect_error(
    get_text_annotation(model = re, rvu = " mg/kg", x_range = x_range,
                        y_range = y_range, rl_index = 1.1,
                        mtbs = "verified", plot_option = "full"),
    "rl_index must be a positive integer of length 1")
  expect_error(
    get_text_annotation(model = re, rvu = " mg/kg", x_range = x_range,
                        y_range = y_range, rl_index = 1:3,
                        mtbs = "verified", plot_option = "full"),
    "rl_index must be a positive integer of length 1")
  expect_error(
    get_text_annotation(model = re, rvu = " mg/kg", x_range = x_range,
                        y_range = y_range, rl_index = rl_index,
                        mtbs = "incorrect", plot_option = "incorrect"),
    "specify mtbs either as \"verified\", \"cics\", \"dics\", ")
  expect_error(
    get_text_annotation(model = re, rvu = " mg/kg", x_range = x_range,
                        y_range = y_range, rl_index = rl_index,
                        mtbs = "verified", plot_option = "incorrect"),
    "specify plot_option either as \"full\", \"lean\", ")
})
