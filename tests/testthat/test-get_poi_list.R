context("Preparation of list of points of intersection")

test_that("get_poi_list_succeeds", {
  l_mod <-
    get_linear_models(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                      response_vbl = "Potency", time_vbl = "Month",
                      batch_vbl = "Batch")$Models

  re <- get_poi_list(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                     batch_vbl = "Batch", model_list = l_mod, sl = 95,
                     srch_range = c(0, 500), mode = "minimal", alpha = 0.05,
                     ivl = "confidence", ivl_type = "one.sided",
                     ivl_side = "lower")

  # <-><-><-><->

  expect_equivalent(signif(re$cics, 12), 25.9957631369)
  expect_equivalent(signif(re$dics, 12),
                    c(24.5668769601, 25.8819934812, 26.0115876344))
  expect_equivalent(signif(re$dids, 12),
                    c(23.3263956198, 23.1480372703, 25.0525070104))
  expect_equivalent(signif(re$dids.pmse, 12),
                    c(24.0613267712, 23.4703822657, 24.8534942104))
})

test_that("get_poi_list_fails", {
  t_dat <- exp1[exp1$Batch %in% c("b2", "b5", "b7"), ]
  t_dal <- t_dat
  t_dal$Batch <- as.character(t_dal$Batch)

  l_mod <- l_mod2 <-
    get_linear_models(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                      response_vbl = "Potency", time_vbl = "Month",
                      batch_vbl = "Batch")$Models
  names(l_mod2) <- gsub("dids.pmse", "individual", names(l_mod))

  # <-><-><-><->

  expect_error(
    get_poi_list(data = as.matrix(t_dat[, c("Month", "Potency")]),
                 batch_vbl = "Batch", model_list = l_mod,
                 sl = 95, srch_range = c(0, 500), alpha = 0.05,
                 ivl = "confidence", ivl_type = "one.sided",
                 ivl_side = "lower"),
    "data must be provided as data frame")
  expect_error(
    get_poi_list(data = t_dat, batch_vbl = 3, model_list = l_mod, sl = 95,
                 srch_range = c(0, 500), mode = "minimal", alpha = 0.05,
                 ivl = "confidence", ivl_type = "one.sided",
                 ivl_side = "lower"),
    "batch_vbl must be a string")
  expect_error(
    get_poi_list(data = t_dat, batch_vbl = "Lot", model_list = l_mod, sl = 95,
                 srch_range = c(0, 500), mode = "minimal", alpha = 0.05,
                 ivl = "confidence", ivl_type = "one.sided",
                 ivl_side = "lower"),
    "batch_vbl was not found in the provided data frame")
  expect_error(
    get_poi_list(data = t_dal, batch_vbl = "Batch", model_list = l_mod,
                 sl = 95, srch_range = c(0, 500),  mode = "minimal",
                 alpha = 0.05, ivl = "confidence", ivl_type = "one.sided",
                 ivl_side = "lower"),
    "column in data specified by batch_vbl")
  expect_error(
    get_poi_list(data = t_dat, batch_vbl = "Batch", model_list = "l_mod",
                 sl = 95, srch_range = c(0, 500), mode = "minimal",
                 alpha = 0.05, ivl = "confidence", ivl_type = "one.sided",
                 ivl_side = "lower"),
    "model_list must be a list")
  expect_error(
    get_poi_list(data = t_dat, batch_vbl = "Batch", model_list = l_mod2,
                 sl = 95, srch_range = c(0, 500), mode = "minimal",
                 alpha = 0.05, ivl = "confidence", ivl_type = "one.sided",
                 ivl_side = "lower"),
    "model_list must have four elements named ")
  expect_error(
    get_poi_list(data = t_dat, batch_vbl = "Batch", model_list = l_mod,
                 sl = "SL", srch_range = c(0, 500), mode = "minimal",
                 alpha = 0.05, ivl = "confidence", ivl_type = "one.sided",
                 ivl_side = "lower"),
    "sl must be a numeric value of length 1")
  expect_error(
    get_poi_list(data = t_dat, batch_vbl = "Batch", model_list = l_mod,
                 sl = c(95, 105), srch_range = c(0, 500), mode = "minimal",
                 alpha = 0.05, ivl = "confidence", ivl_type = "one.sided",
                 ivl_side = "lower"),
    "sl must be a numeric value of length 1")
  expect_error(
    get_poi_list(data = t_dat, batch_vbl = "Batch", model_list = l_mod,
                 sl = 95, srch_range = "range", mode = "minimal",
                 alpha = 0.05, ivl = "confidence", ivl_type = "one.sided",
                 ivl_side = "lower"),
    "srch_range must be a vector of length 2")
  expect_error(
    get_poi_list(data = t_dat, batch_vbl = "Batch", model_list = l_mod,
                 sl = 95, srch_range = 500, mode = "minimal",
                 alpha = 0.05, ivl = "confidence", ivl_type = "one.sided",
                 ivl_side = "lower"),
    "srch_range must be a vector of length 2")
  expect_error(
    get_poi_list(data = t_dat, batch_vbl = "Batch", model_list = l_mod,
                 sl = 95, srch_range = c(0, 500), mode = "maximal",
                 alpha = 5, ivl = "confidence", ivl_type = "one.sided",
                 ivl_side = "lower"),
    "specify mode either as \"minimal\" or \"all\"")
  expect_error(
    get_poi_list(data = t_dat, batch_vbl = "Batch", model_list = l_mod,
                 sl = 95, srch_range = c(0, 500), mode = "minimal",
                 alpha = 5, ivl = "confidence", ivl_type = "one.sided",
                 ivl_side = "lower"),
    "specify alpha")
  expect_error(
    get_poi_list(data = t_dat, batch_vbl = "Batch", model_list = l_mod,
                 sl = 95, srch_range = c(0, 500), mode = "minimal",
                 alpha = -1, ivl = "confidence", ivl_type = "one.sided",
                 ivl_side = "lower"),
    "specify alpha")
  expect_error(
    get_poi_list(data = t_dat, batch_vbl = "Batch", model_list = l_mod,
                 sl = 95, srch_range = c(0, 500), mode = "minimal",
                 alpha = 0.05, ivl = "incorrect", ivl_type = "one.sided",
                 ivl_side = "lower"),
    "specify ivl either as \"confidence\" or \"prediction\"")
  expect_error(
    get_poi_list(data = t_dat, batch_vbl = "Batch", model_list = l_mod,
                 sl = 95, srch_range = c(0, 500), mode = "minimal",
                 alpha = 0.05, ivl = "confidence", ivl_type = "incorrect",
                 ivl_side = "lower"),
    "specify ivl_type either as \"one.sided\" or \"two.sided\"")
  expect_error(
    get_poi_list(data = t_dat, batch_vbl = "Batch", model_list = l_mod,
                 sl = 95, srch_range = c(0, 500), mode = "minimal",
                 alpha = 0.05, ivl = "confidence", ivl_type = "one.sided",
                 ivl_side = "incorrect"),
    "specify ivl_side either as \"lower\", \"upper\" or \"both\"")
  expect_error(
    get_poi_list(data = t_dat, batch_vbl = "Batch", model_list = l_mod,
                 sl = 95, srch_range = c(0, 500), mode = "minimal",
                 alpha = 0.05, ivl = "confidence", ivl_type = "one.sided",
                 ivl_side = "both"),
    "Since ivl_side = \"both\"")
})
