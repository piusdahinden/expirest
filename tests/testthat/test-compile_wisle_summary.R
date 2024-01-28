context("Compilation of wisle summary")

test_that("compile_wisle_summary_succeeds", {
  d_dat <- exp1[exp1$Batch %in% c("b2", "b5", "b7"), ]
  response_vbl <- "Potency"
  time_vbl <- "Month"
  batch_vbl <- "Batch"
  rl <- c(98.0, 98.5, 99.0)
  sl <- 95
  srch_range <- c(0, 500)
  alpha <- 0.05
  xform <- c("no", "no")
  shift <- c(0, 0)
  sf_option <- "tight"
  ivl <- "confidence"
  ivl_type <- "one.sided"
  ivl_side <- "lower"

  re <- expirest_osle(data = d_dat, response_vbl = response_vbl,
                      time_vbl = time_vbl, batch_vbl = batch_vbl, sl = sl,
                      sl_sf = 2, srch_range = srch_range, alpha = alpha,
                      alpha_pool = 0.25, xform = xform, shift = shift,
                      sf_option = sf_option, ivl = ivl, ivl_type = ivl_type,
                      ivl_side = ivl_side, rl = rl, rl_sf = rep(3, 3))

  # <-><-><-><->

  rel_lim <- get_relevant_limits(limits_list = re[["Limits"]],
                                 xform = xform, ivl_side = ivl_side)

  l_wisle <-
    get_wisle_poi_list(icpt_list = re[["Intercepts"]],
                       model_list = re[["Models"]], rl = rl, sl = sl,
                       srch_range = srch_range, alpha = alpha,  xform = xform,
                       shift = shift, ivl = ivl, ivl_type = ivl_type,
                       ivl_side = ivl_side)

  # <-><-><-><->

  l_ws <-
    compile_wisle_summary(data = d_dat, batch_vbl = batch_vbl, rl = rl,
                          poi_list = l_wisle[["all.poi"]],
                          icpt_list = re[["Intercepts"]],
                          wcsl_list = l_wisle[["all.wcsl"]],
                          wcb_list = l_wisle[["which.wc.batch"]],
                          limits_list = rel_lim, poi_ich = re[["POI"]],
                          xform = xform, shift = shift)

  # <-><-><-><->

  expect_equivalent(signif(l_ws[["POI"]][1, ], 12),
                    c(100.566878981, 100.363753745, 100.249139280,
                      100.249139280, 3.00000000000, 3.00000000000,
                      3.00000000000, 3.00000000000, 97.5668789809,
                      97.36375374476, 97.24913928013, 97.24913928013,
                      95.00000000000, 95.00000000000, 98.00000000000,
                      98.00000000000, 14.07398372893, 13.23176334366,
                      13.34560849929, 13.80695465630, 25.99576313688,
                      24.56687696007, 23.14803727031, 23.47038226569))
  expect_equivalent(signif(l_ws[["POI"]][2, ], 12),
                    c(100.566878981, 100.363753745, 100.249139280,
                      100.249139280, 3.50000000000, 3.50000000000,
                      3.50000000000, 3.50000000000, 97.0668789809,
                      96.8637537448, 96.7491392801, 96.7491392801,
                      95.0000000000, 95.0000000000, 98.5000000000,
                      98.5000000000, 16.4495380128, 15.6969837579,
                      15.6781160537, 16.1803020554, 25.9957631369,
                      24.5668769601, 23.1480372703, 23.4703822657))
  expect_equivalent(signif(l_ws[["POI"]][3, ], 12),
                    c(100.566878981, 100.363753745, 100.249139280,
                      100.249139280, 4.00000000000, 4.00000000000,
                      4.00000000000, 4.00000000000, 96.5668789809,
                      96.3637537448, 96.2491392801, 96.2491392801,
                      95.0000000000, 95.0000000000, 99.0000000000,
                      99.0000000000, 18.7877734838, 18.1177172240,
                      17.9210844284, 18.4800799210, 25.9957631369,
                      24.5668769601, 23.1480372703, 23.4703822657))
})

test_that("compile_wisle_summary_succeeds_with_a_single_batch", {
  d_dat <- exp3[exp3$Batch == "b1", ]
  response_vbl <- "Moisture"
  time_vbl <- "Month"
  batch_vbl <- "Batch"
  rl <- c(1.5, 1.6)
  sl <- 4.5
  srch_range <- c(0, 5000)
  alpha <- 0.05
  xform <- c("no", "no")
  shift <- c(0, 0)
  sf_option <- "tight"
  ivl <- "confidence"
  ivl_type <- "one.sided"
  ivl_side <- "upper"

  re <- expirest_osle(data = d_dat, response_vbl = response_vbl,
                      time_vbl = time_vbl, batch_vbl = batch_vbl, sl = sl,
                      sl_sf = 2, srch_range = srch_range, alpha = alpha,
                      alpha_pool = 0.25, xform = xform, shift = shift,
                      sf_option = sf_option, ivl = ivl, ivl_type = ivl_type,
                      ivl_side = ivl_side, rl = rl, rl_sf = rep(3, 2))

  # <-><-><-><->

  rel_lim <- get_relevant_limits(limits_list = re[["Limits"]],
                                 xform = xform, ivl_side = ivl_side)

  l_wisle <-
    get_wisle_poi_list(icpt_list = re[["Intercepts"]],
                       model_list = re[["Models"]], rl = rl, sl = sl,
                       srch_range = srch_range, alpha = alpha,  xform = xform,
                       shift = shift, ivl = ivl, ivl_type = ivl_type,
                       ivl_side = ivl_side)

  # <-><-><-><->

  l_ws <-
    compile_wisle_summary(data = d_dat, batch_vbl = batch_vbl, rl = rl,
                          poi_list = l_wisle[["all.poi"]],
                          icpt_list = re[["Intercepts"]],
                          wcsl_list = l_wisle[["all.wcsl"]],
                          wcb_list = l_wisle[["which.wc.batch"]],
                          limits_list = rel_lim, poi_ich = re[["POI"]],
                          xform = xform, shift = shift)

  # <-><-><-><->

  expect_equivalent(signif(l_ws[["POI"]][1, ], 12),
                    c(NA, NA, 2.31655267606, NA, NA, NA, 3.00000000000,
                      NA, NA, NA, 5.31655267606, NA, 4.50000000000,
                      4.50000000000, 1.50000000000, 1.50000000000, NA, NA,
                      64.7437051333, NA, NA, NA, 48.8707482970, NA))
  expect_equivalent(signif(l_ws[["POI"]][2, ], 12),
                    c(NA, NA, 2.31655267606, NA, NA, NA, 2.90000000000,
                      NA, NA, NA, 5.21655267606, NA, 4.50000000000,
                      4.50000000000, 1.60000000000, 1.60000000000, NA, NA,
                      62.8064385431, NA, NA, NA, 48.8707482970, NA))
})

test_that("compile_wisle_summary_succeeds_with_transformation", {
  response_vbl <- "Related"
  time_vbl <- "Month"
  batch_vbl <- "Batch"
  rl <- 0.15
  sl <- 0.3
  srch_range <- c(0, 500)
  alpha <- 0.05
  xform <- c("log", "log")
  shift <- c(1, 0)
  sf_option <- "tight"
  ivl <- "confidence"
  ivl_type <- "one.sided"
  ivl_side <- "upper"

  re <- expirest_osle(data = exp2, response_vbl = response_vbl,
                      time_vbl = time_vbl, batch_vbl = batch_vbl, sl = sl,
                      sl_sf = 2, srch_range = srch_range, alpha = alpha,
                      alpha_pool = 0.25, xform = xform, shift = shift,
                      sf_option = sf_option, ivl = ivl, ivl_type = ivl_type,
                      ivl_side = ivl_side, rl = rl, rl_sf = 3)

  # <-><-><-><->

  rel_lim <- get_relevant_limits(limits_list = re[["Limits"]],
                                 xform = xform, ivl_side = ivl_side)

  l_wisle <-
    get_wisle_poi_list(icpt_list = re[["Intercepts"]],
                       model_list = re[["Models"]], rl = rl, sl = sl,
                       srch_range = srch_range, alpha = alpha,  xform = xform,
                       shift = shift, ivl = ivl, ivl_type = ivl_type,
                       ivl_side = ivl_side)

  # <-><-><-><->

  l_ws <-
    compile_wisle_summary(data = exp2, batch_vbl = batch_vbl, rl = rl,
                          poi_list = l_wisle[["all.poi"]],
                          icpt_list = re[["Intercepts"]],
                          wcsl_list = l_wisle[["all.wcsl"]],
                          wcb_list = l_wisle[["which.wc.batch"]],
                          limits_list = rel_lim, poi_ich = re[["POI"]],
                          xform = xform, shift = shift)

  # <-><-><-><->

  expect_equivalent(signif(l_ws[["POI"]][1, ], 12),
                    c(0.069355223338, NA, 0.024960711808, NA, 1.16183424273,
                      NA, 1.16183424273, NA, 0.080579273387, NA,
                      0.029000209701, NA, 0.300000000000, 0.300000000000,
                      0.150000000000, 0.150000000000, NA, NA, NA, NA,
                      29.9302941560, 14.3282416139, 22.9926049346,
                      17.7514514859))
})

test_that("compile_wisle_summary_fails", {
  d_dat <- exp1[exp1$Batch %in% c("b2", "b5", "b7"), ]
  response_vbl <- "Potency"
  time_vbl <- "Month"
  batch_vbl <- "Batch"
  rl <- 98
  sl <- 95
  srch_range <- c(0, 500)
  alpha <- 0.05
  xform <- c("no", "no")
  shift <- c(0, 0)
  sf_option <- "tight"
  ivl <- "confidence"
  ivl_type <- "one.sided"
  ivl_side <- "lower"

  re <- expirest_osle(data = d_dat, response_vbl = response_vbl,
                      time_vbl = time_vbl, batch_vbl = batch_vbl, sl = sl,
                      sl_sf = 2, srch_range = srch_range, alpha = alpha,
                      alpha_pool = 0.25, xform = xform, shift = shift,
                      sf_option = sf_option, ivl = ivl, ivl_type = ivl_type,
                      ivl_side = ivl_side, rl = rl, rl_sf = 3)

  # <-><-><-><->

  rel_lim <- get_relevant_limits(limits_list = re[["Limits"]],
                                 xform = xform, ivl_side = ivl_side)

  l_wisle <-
    get_wisle_poi_list(icpt_list = re[["Intercepts"]],
                       model_list = re[["Models"]], rl = rl, sl = sl,
                       srch_range = srch_range, alpha = alpha,  xform = xform,
                       shift = shift, ivl = ivl, ivl_type = ivl_type,
                       ivl_side = ivl_side)

  # <-><-><-><->

  l_ws <-
    compile_wisle_summary(data = d_dat, batch_vbl = batch_vbl, rl = rl,
                          poi_list = l_wisle[["all.poi"]],
                          icpt_list = re[["Intercepts"]],
                          wcsl_list = l_wisle[["all.wcsl"]],
                          wcb_list = l_wisle[["which.wc.batch"]],
                          limits_list = rel_lim, poi_ich = re[["POI"]],
                          xform = xform, shift = shift)

  l_icpt <- re[["Intercepts"]]
  l_poi <- l_wisle[["all.poi"]]
  ll_wcsl <- l_wisle[["all.wcsl"]]
  l_wc_batch <- l_wisle[["which.wc.batch"]]

  # <-><-><-><->
  # Generation of flawed objects

  t_dal <- exp1[exp1$Batch %in% c("b2", "b5", "b7"), ]
  t_dal$Batch <- as.character(t_dal$Batch)
  l_poi_err <- l_poi[1:3]
  l_icpt_err <- re[["Intercepts"]][1:3]
  ll_wcsl_err <- l_wisle[["all.wcsl"]][1:3]
  l_wc_batch_err <- l_wc_batch[1:3]
  rel_lim_err <- rel_lim[1:2]
  poi_ich_err1 <- re[["POI"]][1:3]
  poi_ich_err2 <- re[["POI"]]
  names(poi_ich_err2) <- gsub("cics", "xixs", names(poi_ich_err2))

  # <-><-><-><->

  expect_error(
    compile_wisle_summary(data = "d_dat", batch_vbl = batch_vbl, rl = rl,
                          poi_list = l_poi, icpt_list = l_icpt,
                          wcsl_list = ll_wcsl, wcb_list = l_wc_batch,
                          limits_list = rel_lim, poi_ich = re[["POI"]],
                          xform = xform, shift = shift),
    "data must be provided as data frame")
  expect_error(
    compile_wisle_summary(data = d_dat, batch_vbl = 4, rl = rl,
                          poi_list = l_poi, icpt_list = l_icpt,
                          wcsl_list = ll_wcsl, wcb_list = l_wc_batch,
                          limits_list = rel_lim, poi_ich = re[["POI"]],
                          xform = xform, shift = shift),
    "batch_vbl must be a string")
  expect_error(
    compile_wisle_summary(data = t_dal, batch_vbl = "Lot", rl = rl,
                          poi_list = l_poi, icpt_list = l_icpt,
                          wcsl_list = ll_wcsl, wcb_list = l_wc_batch,
                          limits_list = rel_lim, poi_ich = re[["POI"]],
                          xform = xform, shift = shift),
    "batch_vbl was not found in the provided data frame")
  expect_error(
    compile_wisle_summary(data = d_dat, batch_vbl = batch_vbl, rl = "rl",
                          poi_list = l_poi, icpt_list = l_icpt,
                          wcsl_list = ll_wcsl, wcb_list = l_wc_batch,
                          limits_list = rel_lim, poi_ich = re[["POI"]],
                          xform = xform, shift = shift),
    "rl must be a numeric")
  expect_error(
    compile_wisle_summary(data = d_dat, batch_vbl = batch_vbl, rl = rl,
                          poi_list = "l_poi", icpt_list = l_icpt,
                          wcsl_list = ll_wcsl, wcb_list = l_wc_batch,
                          limits_list = rel_lim, poi_ich = re[["POI"]],
                          xform = xform, shift = shift),
    "poi_list must be a list with four elements")
  expect_error(
    compile_wisle_summary(data = d_dat, batch_vbl = batch_vbl, rl = rl,
                          poi_list = l_poi_err, icpt_list = l_icpt,
                          wcsl_list = ll_wcsl, wcb_list = l_wc_batch,
                          limits_list = rel_lim, poi_ich = re[["POI"]],
                          xform = xform, shift = shift),
    "poi_list must be a list with four elements")
  expect_error(
    compile_wisle_summary(data = d_dat, batch_vbl = batch_vbl, rl = rl,
                          poi_list = l_poi, icpt_list = "l_icpt",
                          wcsl_list = ll_wcsl, wcb_list = l_wc_batch,
                          limits_list = rel_lim, poi_ich = re[["POI"]],
                          xform = xform, shift = shift),
    "icpt_list must be a list with four elements")
  expect_error(
    compile_wisle_summary(data = d_dat, batch_vbl = batch_vbl, rl = rl,
                          poi_list = l_poi, icpt_list = l_icpt_err,
                          wcsl_list = ll_wcsl, wcb_list = l_wc_batch,
                          limits_list = rel_lim, poi_ich = re[["POI"]],
                          xform = xform, shift = shift),
    "icpt_list must be a list with four elements")
  expect_error(
    compile_wisle_summary(data = d_dat, batch_vbl = batch_vbl, rl = rl,
                          poi_list = l_poi, icpt_list = l_icpt,
                          wcsl_list = "ll_wcsl", wcb_list = l_wc_batch,
                          limits_list = rel_lim, poi_ich = re[["POI"]],
                          xform = xform, shift = shift),
    "wcsl_list must be a list with four elements")
  expect_error(
    compile_wisle_summary(data = d_dat, batch_vbl = batch_vbl, rl = rl,
                          poi_list = l_poi, icpt_list = l_icpt,
                          wcsl_list = ll_wcsl_err, wcb_list = l_wc_batch,
                          limits_list = rel_lim, poi_ich = re[["POI"]],
                          xform = xform, shift = shift),
    "wcsl_list must be a list with four elements")
  expect_error(
    compile_wisle_summary(data = d_dat, batch_vbl = batch_vbl, rl = rl,
                          poi_list = l_poi, icpt_list = l_icpt,
                          wcsl_list = ll_wcsl, wcb_list = "l_wc_batch",
                          limits_list = rel_lim, poi_ich = re[["POI"]],
                          xform = xform, shift = shift),
    "wcb_list must be a list with four elements")
  expect_error(
    compile_wisle_summary(data = d_dat, batch_vbl = batch_vbl, rl = rl,
                          poi_list = l_poi, icpt_list = l_icpt,
                          wcsl_list = ll_wcsl, wcb_list = l_wc_batch_err,
                          limits_list = rel_lim, poi_ich = re[["POI"]],
                          xform = xform, shift = shift),
    "wcb_list must be a list with four elements")
  expect_error(
    compile_wisle_summary(data = d_dat, batch_vbl = batch_vbl, rl = rl,
                          poi_list = l_poi, icpt_list = l_icpt,
                          wcsl_list = ll_wcsl, wcb_list = l_wc_batch,
                          limits_list = "rel_lim", poi_ich = re[["POI"]],
                          xform = xform, shift = shift),
    "limits_list must be a list with at least the four elements")
  expect_error(
    compile_wisle_summary(data = d_dat, batch_vbl = batch_vbl, rl = rl,
                          poi_list = l_poi, icpt_list = l_icpt,
                          wcsl_list = ll_wcsl, wcb_list = l_wc_batch,
                          limits_list = rel_lim_err, poi_ich = re[["POI"]],
                          xform = xform, shift = shift),
    "limits_list must be a list with at least the four elements")
  expect_error(
    compile_wisle_summary(data = d_dat, batch_vbl = batch_vbl, rl = rl,
                          poi_list = l_poi, icpt_list = l_icpt,
                          wcsl_list = ll_wcsl, wcb_list = l_wc_batch,
                          limits_list = rel_lim, poi_ich = "re",
                          xform = xform, shift = shift),
    "poi_ich must be a vector of length 4")
  expect_error(
    compile_wisle_summary(data = d_dat, batch_vbl = batch_vbl, rl = rl,
                          poi_list = l_poi, icpt_list = l_icpt,
                          wcsl_list = ll_wcsl, wcb_list = l_wc_batch,
                          limits_list = rel_lim, poi_ich = poi_ich_err1,
                          xform = xform, shift = shift),
    "poi_ich must be a vector of length 4")
  expect_error(
    compile_wisle_summary(data = d_dat, batch_vbl = batch_vbl, rl = rl,
                          poi_list = l_poi, icpt_list = l_icpt,
                          wcsl_list = ll_wcsl, wcb_list = l_wc_batch,
                          limits_list = rel_lim, poi_ich = poi_ich_err2,
                          xform = xform, shift = shift),
    "poi_ich must be a vector with the names")
  expect_error(
    compile_wisle_summary(data = d_dat, batch_vbl = batch_vbl, rl = rl,
                          poi_list = l_poi, icpt_list = l_icpt,
                          wcsl_list = ll_wcsl, wcb_list = l_wc_batch,
                          limits_list = rel_lim, poi_ich = re[["POI"]],
                          xform = "no", shift = shift),
    "specify xform appropriately")
  expect_error(
    compile_wisle_summary(data = d_dat, batch_vbl = batch_vbl, rl = rl,
                          poi_list = l_poi, icpt_list = l_icpt,
                          wcsl_list = ll_wcsl, wcb_list = l_wc_batch,
                          limits_list = rel_lim, poi_ich = re[["POI"]],
                          xform = c("yes", "no"), shift = shift),
    "specify xform appropriately")
  expect_error(
    compile_wisle_summary(data = d_dat, batch_vbl = batch_vbl, rl = rl,
                          poi_list = l_poi, icpt_list = l_icpt,
                          wcsl_list = ll_wcsl, wcb_list = l_wc_batch,
                          limits_list = rel_lim, poi_ich = re[["POI"]],
                          xform = c("no", "yes"), shift = shift),
    "specify xform appropriately")
  expect_error(
    compile_wisle_summary(data = d_dat, batch_vbl = batch_vbl, rl = rl,
                          poi_list = l_poi, icpt_list = l_icpt,
                          wcsl_list = ll_wcsl, wcb_list = l_wc_batch,
                          limits_list = rel_lim, poi_ich = re[["POI"]],
                          xform = xform, shift = c("no", "no")),
    "shift must be a numeric vector of length 2")
  expect_error(
    compile_wisle_summary(data = d_dat, batch_vbl = batch_vbl, rl = rl,
                          poi_list = l_poi, icpt_list = l_icpt,
                          wcsl_list = ll_wcsl, wcb_list = l_wc_batch,
                          limits_list = rel_lim, poi_ich = re[["POI"]],
                          xform = xform, shift = 1),
    "shift must be a numeric vector of length 2")
})
