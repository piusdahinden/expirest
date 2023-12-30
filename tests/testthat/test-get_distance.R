context("Distance of lines")

test_that("get_distance_succeeds", {
  r_cics <- stats::lm(Potency ~ Month,
                      data = exp1[exp1$Batch %in% c("b3", "b4", "b5"), ])
  r_dics <- stats::lm(Potency ~ Month + Batch,
                       data = exp1[exp1$Batch %in% c("b3", "b4", "b5"), ])
  r_dids <- stats::lm(Potency ~ Batch * Month,
                      data = exp1[exp1$Batch %in% c("b3", "b4", "b5"), ])

  usl <- 105
  lsl <- 95

  tmp <- rep(NA, 4)

  # <-><-><-><->

  tmp[1] <- get_distance(x_new = 36, model = r_cics, sl = lsl,
                         mode = "minimal", alpha = 0.05, ivl = "confidence",
                         ivl_type = "one.sided", ivl_side = "lower")
  tmp[2] <- get_distance(x_new = 36, model = r_cics, sl = lsl,
                         mode = "minimal", alpha = 0.05, ivl = "prediction",
                         ivl_type = "one.sided", ivl_side = "lower")
  tmp[3] <- get_distance(x_new = 36, model = r_cics, sl = usl,
                         mode = "minimal", alpha = 0.05, ivl = "confidence",
                         ivl_type = "one.sided", ivl_side = "upper")
  tmp[4] <- get_distance(x_new = 36, model = r_cics, sl = usl,
                         mode = "minimal", alpha = 0.05, ivl = "prediction",
                         ivl_type = "one.sided", ivl_side = "upper")

  tmp_dics <- get_distance(x_new = 36, model = r_dics, sl = lsl,
                            mode = "all", alpha = 0.05, ivl = "confidence",
                            ivl_type = "one.sided", ivl_side = "lower")
  tmp_dids <- get_distance(x_new = 36, model = r_dids, sl = lsl,
                            mode = "all", alpha = 0.05, ivl = "confidence",
                            ivl_type = "one.sided", ivl_side = "lower")

  # <-><-><-><->

  expect_equal(signif(tmp, 12),
               c(1.80614791794, 3.48734839117, 8.04782628398, 6.36662581075))
  expect_equivalent(signif(tmp_dics, 12),
                    c(1.74086124875, -0.35099637106, 3.12628691752))
  expect_equivalent(signif(tmp_dids, 12),
                    c(3.12057631253, 0.10662397569, 3.73848267441))
})

test_that("get_distance_fails", {
  r_potency <- stats::lm(Potency ~ Month,
                         data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ])
  usl <- 105
  lsl <- 95

  tmp <- rep(NA, 2)

  # <-><-><-><->

  tmp[1] <- get_distance(x_new = 36, model = r_potency, sl = usl,
                         alpha = 1E-32, ivl = "confidence",
                         ivl_type = "one.sided", ivl_side = "upper")
  tmp[2] <- get_distance(x_new = 36, model = r_potency, sl = usl,
                         alpha = 1E-32, ivl = "prediction",
                         ivl_type = "one.sided", ivl_side = "upper")

  # <-><-><-><->

  expect_equal(tmp, c(-Inf, -Inf))

  # <-><-><-><->

  expect_error(
    get_distance(x_new = "alpha", model = r_potency, sl = lsl, mode = "minimal",
                 alpha = 0.05, ivl = "confidence", ivl_type = "one.sided",
                 ivl_side = "lower"),
    "x_new must be a numeric")
  expect_error(
    get_distance(x_new = 36, model = "r_potency", sl = lsl, mode = "minimal",
                 alpha = 0.05, ivl = "confidence", ivl_type = "one.sided",
                 ivl_side = "lower"),
    "Please provide a model of type \"lm\"")
  expect_error(
    get_distance(x_new = 36, model = r_potency, sl = "SL", mode = "minimal",
                 alpha = 0.05, ivl = "confidence", ivl_type = "one.sided",
                 ivl_side = "lower"),
    "sl must be a numeric value of length 1")
  expect_error(
    get_distance(x_new = 36, model = r_potency, sl = lsl, mode = "maximal",
                 alpha = 0.05, ivl = "incorrect", ivl_type = "one.sided",
                 ivl_side = "lower"),
    "specify mode either as \"minimal\" or \"all\"")
  expect_error(
    get_distance(x_new = 36, model = r_potency, sl = lsl, mode = "minimal",
                 alpha = 5, ivl = "confidence", ivl_type = "one.sided",
                 ivl_side = "lower"),
    "specify alpha")
  expect_error(
    get_distance(x_new = 36, model = r_potency, sl = lsl, mode = "minimal",
                 alpha = -1, ivl = "confidence", ivl_type = "one.sided",
                 ivl_side = "lower"),
    "specify alpha")
  expect_error(
    get_distance(x_new = 36, model = r_potency, sl = lsl, mode = "minimal",
                 alpha = 0.05, ivl = "incorrect", ivl_type = "one.sided",
                 ivl_side = "lower"),
    "specify ivl either as \"confidence\" or \"prediction\"")
  expect_error(
    get_distance(x_new = 36, model = r_potency, sl = lsl, mode = "minimal",
                 alpha = 0.05, ivl = "confidence", ivl_type = "incorrect",
                 ivl_side = "lower"),
    "specify ivl_type either as \"one.sided\" or \"two.sided\"")
  expect_error(
    get_distance(x_new = 36, model = r_potency, sl = lsl, mode = "minimal",
                 alpha = 0.05, ivl = "confidence", ivl_type = "one.sided",
                 ivl_side = "incorrect"),
    "specify ivl_side either as \"lower\" or \"upper\"")
})
