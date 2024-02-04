context("Determination of osle worst case batches and POIs")

test_that("get_osle_poi_list_succeeds", {
  d_dat <- exp1[exp1$Batch %in% c("b2", "b5", "b7"), ]
  response_vbl <- "Potency"
  time_vbl <- "Month"
  batch_vbl <- "Batch"
  xform <- c("no", "no")
  shift <- c(0, 0)

  l_models <-
    get_model_list(data = d_dat, response_vbl = response_vbl,
                   time_vbl = time_vbl, batch_vbl = batch_vbl)$Models

  l_icpt <- get_icpt_list(data = d_dat, response_vbl = response_vbl,
                          time_vbl = time_vbl, batch_vbl = batch_vbl,
                          model_list = l_models, xform = xform, shift = shift)
  # <-><-><-><->

  l_osle <-
    get_osle_poi_list(data = d_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                      model_list = l_models, sl = 95, srch_range = c(0, 500),
                      alpha = 0.05,  xform = xform, shift = shift,
                      ivl = "confidence", ivl_type = "one.sided",
                      ivl_side = "lower")

  # <-><-><-><->

  expect_equivalent(signif(l_osle[["poi"]], 12),
                    c(25.9957631369, 24.5668769601,
                      23.4703822657, 23.1480372703))
  expect_equivalent(attributes(l_osle$poi)$names,
                    c("cics", "dics", "dids.pmse", "dids"))
  expect_equivalent(attributes(l_osle$poi)$side, rep("lower", 4))
})

test_that("get_osle_poi_list_fails", {
  d_dat <- exp1[exp1$Batch %in% c("b2", "b5", "b7"), ]
  response_vbl <- "Potency"
  time_vbl <- "Month"
  batch_vbl <- "Batch"
  sl <- 95
  srch_range <- c(0, 500)
  alpha <- 0.05
  xform <- c("no", "no")
  shift <- c(0, 0)
  ivl <- "confidence"
  ivl_type <- "one.sided"
  ivl_side <- "lower"

  l_models <-
    get_model_list(data = d_dat, response_vbl = response_vbl,
                   time_vbl = time_vbl, batch_vbl = batch_vbl)$Models

  l_icpt <- get_icpt_list(data = d_dat, response_vbl = response_vbl,
                          time_vbl = time_vbl, batch_vbl = batch_vbl,
                          model_list = l_models, xform = xform, shift = shift)

  # <-><-><-><->
  # Generation of flawed objects

  t_dat <- as.matrix(d_dat[, c("Month", "Potency")])
  l_icpt_err <- l_icpt[["Intercepts"]][1:3]
  l_models_err <- l_models[["Models"]][1:3]

  # <-><-><-><->

  expect_error(
    get_osle_poi_list(data = t_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                      model_list = l_models, sl = sl, srch_range = srch_range,
                      alpha = alpha,  xform = xform, shift = shift, ivl = ivl,
                      ivl_type = ivl_type, ivl_side = ivl_side),
    "data must be provided as data frame")
  expect_error(
    get_osle_poi_list(data = d_dat, batch_vbl = 4, icpt_list = l_icpt,
                      model_list = l_models, sl = sl, srch_range = srch_range,
                      alpha = alpha,  xform = xform, shift = shift, ivl = ivl,
                      ivl_type = ivl_type, ivl_side = ivl_side),
    "batch_vbl must be a string")
  expect_error(
    get_osle_poi_list(data = d_dat, batch_vbl = "Lot", icpt_list = l_icpt,
                      model_list = l_models, sl = sl, srch_range = srch_range,
                      alpha = alpha,  xform = xform, shift = shift, ivl = ivl,
                      ivl_type = ivl_type, ivl_side = ivl_side),
    "batch_vbl was not found in the provided data frame")
  expect_error(
    get_osle_poi_list(data = d_dat, batch_vbl = batch_vbl, icpt_list = "l_icpt",
                      model_list = l_models, sl = sl, srch_range = srch_range,
                      alpha = alpha,  xform = xform, shift = shift, ivl = ivl,
                      ivl_type = ivl_type, ivl_side = ivl_side),
    "icpt_list must be a list with four elements")
  expect_error(
    get_osle_poi_list(data = d_dat, batch_vbl = batch_vbl,
                      icpt_list = l_icpt_err, model_list = l_models, sl = sl,
                      srch_range = srch_range, alpha = alpha,  xform = xform,
                      shift = shift, ivl = ivl, ivl_type = ivl_type,
                      ivl_side = ivl_side),
    "icpt_list must be a list with four elements")
  expect_error(
    get_osle_poi_list(data = d_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                      model_list = "l_models", sl = sl, srch_range = srch_range,
                      alpha = alpha,  xform = xform, shift = shift, ivl = ivl,
                      ivl_type = ivl_type, ivl_side = ivl_side),
    "model_list must be a list with four elements")
  expect_error(
    get_osle_poi_list(data = d_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                      model_list = l_models_err, sl = sl,
                      srch_range = srch_range, alpha = alpha,  xform = xform,
                      shift = shift, ivl = ivl, ivl_type = ivl_type,
                      ivl_side = ivl_side),
    "model_list must be a list with four elements")
  expect_error(
    get_osle_poi_list(data = d_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                      model_list = l_models, sl = "sl", srch_range = srch_range,
                      alpha = alpha,  xform = xform, shift = shift, ivl = ivl,
                      ivl_type = ivl_type, ivl_side = ivl_side),
    "sl must be a numeric or vector of length 1 or 2")
  expect_error(
    get_osle_poi_list(data = d_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                      model_list = l_models, sl = c(95, 100, 105),
                      srch_range = srch_range, alpha = alpha,  xform = xform,
                      shift = shift, ivl = ivl, ivl_type = ivl_type,
                      ivl_side = ivl_side),
    "sl must be a numeric or vector of length 1 or 2")
  expect_error(
    get_osle_poi_list(data = d_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                      model_list = l_models, sl = c(105, 95),
                      srch_range = srch_range, alpha = alpha,  xform = xform,
                      shift = shift, ivl = ivl, ivl_type = ivl_type,
                      ivl_side = ivl_side),
    "sl must be of the form")
  expect_error(
    get_osle_poi_list(data = d_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                      model_list = l_models, sl = sl, srch_range = "srch_range",
                      alpha = alpha,  xform = xform, shift = shift, ivl = ivl,
                      ivl_type = ivl_type, ivl_side = ivl_side),
    "srch_range must be a vector of length 2")
  expect_error(
    get_osle_poi_list(data = d_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                      model_list = l_models, sl = sl, srch_range = 500,
                      alpha = alpha,  xform = xform, shift = shift, ivl = ivl,
                      ivl_type = ivl_type, ivl_side = ivl_side),
    "srch_range must be a vector of length 2")
  expect_error(
    get_osle_poi_list(data = d_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                      model_list = l_models, sl = sl, srch_range = srch_range,
                      alpha = 5,  xform = xform, shift = shift, ivl = ivl,
                      ivl_type = ivl_type, ivl_side = ivl_side),
    "specify alpha")
  expect_error(
    get_osle_poi_list(data = d_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                      model_list = l_models, sl = sl, srch_range = srch_range,
                      alpha = -1,  xform = xform, shift = shift, ivl = ivl,
                      ivl_type = ivl_type, ivl_side = ivl_side),
    "specify alpha")
  expect_error(
    get_osle_poi_list(data = d_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                      model_list = l_models, sl = sl, srch_range = srch_range,
                      alpha = alpha,  xform = "no", shift = shift, ivl = ivl,
                      ivl_type = ivl_type, ivl_side = ivl_side),
    "specify xform appropriately")
  expect_error(
    get_osle_poi_list(data = d_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                      model_list = l_models, sl = sl, srch_range = srch_range,
                      alpha = alpha,  xform = c("yes", "no"), shift = shift,
                      ivl = ivl, ivl_type = ivl_type, ivl_side = ivl_side),
    "specify xform appropriately")
  expect_error(
    get_osle_poi_list(data = d_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                      model_list = l_models, sl = sl, srch_range = srch_range,
                      alpha = alpha,  xform = c("no", "yes"), shift = shift,
                      ivl = ivl, ivl_type = ivl_type, ivl_side = ivl_side),
    "specify xform appropriately")
  expect_error(
    get_osle_poi_list(data = d_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                      model_list = l_models, sl = sl, srch_range = srch_range,
                      alpha = alpha,  xform = xform, shift = c("no", "no"),
                      ivl = ivl, ivl_type = ivl_type, ivl_side = ivl_side),
    "shift must be a numeric vector of length 2")
  expect_error(
    get_osle_poi_list(data = d_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                      model_list = l_models, sl = sl, srch_range = srch_range,
                      alpha = alpha,  xform = xform, shift = 1, ivl = ivl,
                      ivl_type = ivl_type, ivl_side = ivl_side),
    "shift must be a numeric vector of length 2")
  expect_error(
    get_osle_poi_list(data = d_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                      model_list = l_models, sl = sl, srch_range = srch_range,
                      alpha = alpha,  xform = xform, shift = shift,
                      ivl = "incorrect", ivl_type = ivl_type,
                      ivl_side = ivl_side),
    "specify ivl either as \"confidence\" or \"prediction\"")
  expect_error(
    get_osle_poi_list(data = d_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                      model_list = l_models, sl = sl, srch_range = srch_range,
                      alpha = alpha,  xform = xform, shift = shift, ivl = ivl,
                      ivl_type = "incorrect", ivl_side = ivl_side),
    "specify ivl_type either as \"one.sided\" or \"two.sided\"")
  expect_error(
    get_osle_poi_list(data = d_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                      model_list = l_models, sl = sl, srch_range = srch_range,
                      alpha = alpha,  xform = xform, shift = shift, ivl = ivl,
                      ivl_type = ivl_type, ivl_side = "middle"),
    "specify ivl_side either as \"lower\", \"upper\" or \"both\"")
})
