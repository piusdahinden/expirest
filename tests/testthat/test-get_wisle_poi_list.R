context("Determination of wisle worst case scenario limits, batches and POIs")

test_that("get_wisle_poi_list_succeeds", {
  response_vbl <- "Potency"
  time_vbl <- "Month"
  batch_vbl <- "Batch"
  rl <- c(98, 98.5, 99)
  sl <- 95
  srch_range <- c(0, 500)
  alpha <- 0.05
  xform <- c("no", "no")
  shift <- c(0, 0)
  sf_option <- "tight"
  ivl <- "confidence"
  ivl_type <- "one.sided"
  ivl_side <- "lower"

  re <-
    expirest_osle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                  response_vbl = response_vbl, time_vbl = time_vbl,
                  batch_vbl = batch_vbl, sl = sl, sl_sf = 2,
                  srch_range = srch_range, alpha = alpha, alpha_pool = 0.25,
                  xform = xform, shift = shift, sf_option = sf_option,
                  ivl = ivl, ivl_type = ivl_type, ivl_side = ivl_side,
                  rl = rl, rl_sf = rep(3, 3))

  # <-><-><-><->

  l_wisle <-
    get_wisle_poi_list(icpt_list = re[["Intercepts"]],
                       model_list = re[["Models"]], rl = rl, sl = sl,
                       srch_range = srch_range, alpha = alpha,  xform = xform,
                       shift = shift, ivl = ivl, ivl_type = ivl_type,
                       ivl_side = ivl_side)

  # <-><-><-><->

  expect_equivalent(signif(l_wisle[["all.poi"]][["cics"]][, "(Intercept)"], 12),
                    c(14.0739837289, 16.4495380128, 18.7877734838))
  expect_equivalent(signif(l_wisle[["all.poi"]][["dics"]][, "b2"], 12),
                    c(13.2317633437, 15.6969837579, 18.1177172240))
  expect_equivalent(signif(l_wisle[["all.poi"]][["dids.pmse"]][, "b2"], 12),
                    c(13.8069546563, 16.1803020554, 18.4800799210))
  expect_equivalent(signif(l_wisle[["all.poi"]][["dids"]][, "b2"], 12),
                    c(13.3456084993, 15.6781160537, 17.9210844284))
})

test_that("get_wisle_poi_list_fails", {
  response_vbl <- "Potency"
  time_vbl <- "Month"
  batch_vbl <- "Batch"
  rl <- c(98, 98.5, 99)
  sl <- 95
  srch_range <- c(0, 500)
  alpha <- 0.05
  xform <- c("no", "no")
  shift <- c(0, 0)
  sf_option <- "tight"
  ivl <- "confidence"
  ivl_type <- "one.sided"
  ivl_side <- "lower"

  re <-
    expirest_osle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                  response_vbl = response_vbl, time_vbl = time_vbl,
                  batch_vbl = batch_vbl, sl = sl, sl_sf = 2,
                  srch_range = srch_range, alpha = alpha, alpha_pool = 0.25,
                  xform = xform, shift = shift, sf_option = sf_option,
                  ivl = ivl, ivl_type = ivl_type, ivl_side = ivl_side,
                  rl = rl, rl_sf = rep(3, 3))

  l_icpt <- re[["Intercepts"]]
  l_models <- re[["Models"]]

  # <-><-><-><->
  # Generation of flawed objects

  l_icpt_err <- re[["Intercepts"]][1:3]
  l_models_err <- re[["Models"]][1:3]

  # <-><-><-><->

  expect_error(
    get_wisle_poi_list(icpt_list = "l_icpt", model_list = l_models,
                       rl = rl, sl = sl, srch_range = srch_range,
                       alpha = alpha,  xform = xform, shift = shift, ivl = ivl,
                       ivl_type = ivl_type, ivl_side = ivl_side),
    "icpt_list must be a list with four elements")
  expect_error(
    get_wisle_poi_list(icpt_list = l_icpt_err, model_list = l_models,
                       rl = rl, sl = sl, srch_range = srch_range,
                       alpha = alpha,  xform = xform, shift = shift, ivl = ivl,
                       ivl_type = ivl_type, ivl_side = ivl_side),
    "icpt_list must be a list with four elements")
  expect_error(
    get_wisle_poi_list(icpt_list = l_icpt, model_list = "l_models",
                       rl = rl, sl = sl, srch_range = srch_range,
                       alpha = alpha,  xform = xform, shift = shift, ivl = ivl,
                       ivl_type = ivl_type, ivl_side = ivl_side),
    "model_list must be a list with four elements")
  expect_error(
    get_wisle_poi_list(icpt_list = l_icpt, model_list = l_models_err,
                       rl = rl, sl = sl, srch_range = srch_range,
                       alpha = alpha,  xform = xform, shift = shift, ivl = ivl,
                       ivl_type = ivl_type, ivl_side = ivl_side),
    "model_list must be a list with four elements")
  expect_error(
    get_wisle_poi_list(icpt_list = l_icpt, model_list = l_models,
                       rl = "rl", sl = sl, srch_range = srch_range,
                       alpha = alpha,  xform = xform, shift = shift, ivl = ivl,
                       ivl_type = ivl_type, ivl_side = ivl_side),
    "rl must be a numeric")
  expect_error(
    get_wisle_poi_list(icpt_list = l_icpt, model_list = l_models,
                       rl = rl, sl = "sl", srch_range = srch_range,
                       alpha = alpha,  xform = xform, shift = shift, ivl = ivl,
                       ivl_type = ivl_type, ivl_side = ivl_side),
    "sl must be a numeric value of length 1")
  expect_error(
    get_wisle_poi_list(icpt_list = l_icpt, model_list = l_models,
                       rl = rl, sl = c(95, 105), srch_range = srch_range,
                       alpha = alpha,  xform = xform, shift = shift, ivl = ivl,
                       ivl_type = ivl_type, ivl_side = ivl_side),
    "sl must be a numeric value of length 1")
  expect_error(
    get_wisle_poi_list(icpt_list = l_icpt, model_list = l_models,
                       rl = rl, sl = sl, srch_range = "range",
                       alpha = alpha,  xform = xform, shift = shift, ivl = ivl,
                       ivl_type = ivl_type, ivl_side = ivl_side),
    "srch_range must be a vector of length 2")
  expect_error(
    get_wisle_poi_list(icpt_list = l_icpt, model_list = l_models,
                       rl = rl, sl = sl, srch_range = 500,
                       alpha = alpha,  xform = xform, shift = shift, ivl = ivl,
                       ivl_type = ivl_type, ivl_side = ivl_side),
    "srch_range must be a vector of length 2")
  expect_error(
    get_wisle_poi_list(icpt_list = l_icpt, model_list = l_models,
                       rl = rl, sl = sl, srch_range = srch_range,
                       alpha = 5,  xform = xform, shift = shift, ivl = ivl,
                       ivl_type = ivl_type, ivl_side = ivl_side),
    "specify alpha")
  expect_error(
    get_wisle_poi_list(icpt_list = l_icpt, model_list = l_models,
                       rl = rl, sl = sl, srch_range = srch_range,
                       alpha = -1,  xform = xform, shift = shift, ivl = ivl,
                       ivl_type = ivl_type, ivl_side = ivl_side),
    "specify alpha")
  expect_error(
    get_wisle_poi_list(icpt_list = l_icpt, model_list = l_models,
                       rl = rl, sl = sl, srch_range = srch_range,
                       alpha = alpha,  xform = "no", shift = shift, ivl = ivl,
                       ivl_type = ivl_type, ivl_side = ivl_side),
    "specify xform appropriately")
  expect_error(
    get_wisle_poi_list(icpt_list = l_icpt, model_list = l_models,
                       rl = rl, sl = sl, srch_range = srch_range,
                       alpha = ,  xform = c("yes", "no"), shift = shift,
                       ivl = ivl, ivl_type = ivl_type, ivl_side = ivl_side),
    "specify xform appropriately")
  expect_error(
    get_wisle_poi_list(icpt_list = l_icpt, model_list = l_models,
                       rl = rl, sl = sl, srch_range = srch_range,
                       alpha = ,  xform = c("no", "yes"), shift = shift,
                       ivl = ivl, ivl_type = ivl_type, ivl_side = ivl_side),
    "specify xform appropriately")
  expect_error(
    get_wisle_poi_list(icpt_list = l_icpt, model_list = l_models,
                       rl = rl, sl = sl, srch_range = srch_range,
                       alpha = alpha,  xform = xform, shift = c("no", "no"),
                       ivl = ivl, ivl_type = ivl_type, ivl_side = ivl_side),
    "shift must be a numeric vector of length 2")
  expect_error(
    get_wisle_poi_list(icpt_list = l_icpt, model_list = l_models,
                       rl = rl, sl = sl, srch_range = srch_range,
                       alpha = alpha,  xform = xform, shift = 1,
                       ivl = ivl, ivl_type = ivl_type, ivl_side = ivl_side),
    "shift must be a numeric vector of length 2")
  expect_error(
    get_wisle_poi_list(icpt_list = l_icpt, model_list = l_models,
                       rl = rl, sl = sl, srch_range = srch_range,
                       alpha = alpha,  xform = xform, shift = shift,
                       ivl = "incorrect", ivl_type = ivl_type,
                       ivl_side = ivl_side),
    "specify ivl either as \"confidence\" or \"prediction\"")
  expect_error(
    get_wisle_poi_list(icpt_list = l_icpt, model_list = l_models,
                       rl = rl, sl = sl, srch_range = srch_range,
                       alpha = alpha,  xform = xform, shift = shift, ivl = ivl,
                       ivl_type = "incorrect", ivl_side = ivl_side),
    "specify ivl_type either as \"one.sided\" or \"two.sided\"")
  expect_error(
    get_wisle_poi_list(icpt_list = l_icpt, model_list = l_models,
                       rl = rl, sl = sl, srch_range = srch_range,
                       alpha = alpha,  xform = xform, shift = shift, ivl = ivl,
                       ivl_type = ivl_type, ivl_side = "both"),
    "specify ivl_side either as \"lower\" or \"upper\".")
})
