context("Find Point of Intersection (POI)")

test_that("find_poi_succeeds_one_sided", {
  r_cics <- stats::lm(Potency ~ Month,
                      data = exp1[exp1$Batch %in% c("b3", "b4", "b5"), ])
  r_dics1 <- stats::lm(Potency ~ Month + Batch,
                       data = exp1[exp1$Batch %in% c("b3", "b4", "b5"), ])
  r_dics2 <- stats::lm(Potency ~ Batch + Month,
                       data = exp1[exp1$Batch %in% c("b3", "b4", "b5"), ])
  r_dids1 <- stats::lm(Potency ~ Batch * Month,
                       data = exp1[exp1$Batch %in% c("b3", "b4", "b5"), ])

  r_b2 <- stats::lm(Potency ~ Month, data = exp1[exp1$Batch == "b2", ])
  r_b3 <- stats::lm(Potency ~ Month, data = exp1[exp1$Batch == "b3", ])
  r_b4 <- stats::lm(Potency ~ Month, data = exp1[exp1$Batch == "b4", ])
  r_b5 <- stats::lm(Potency ~ Month, data = exp1[exp1$Batch == "b5", ])
  r_b7 <- stats::lm(Potency ~ Month, data = exp1[exp1$Batch == "b7", ])
  r_b8 <- stats::lm(Potency ~ Month, data = exp1[exp1$Batch == "b8", ])

  r_dids2 <- stats::lm(Potency ~ Batch * Month,
                       data = exp1[exp1$Batch %in%
                                     c("b2", "b3", "b4", "b5", "b7", "b8"), ])

  usl <- 105
  lsl <- 95

  tmp1 <- rep(NA, 4)
  tmp2 <- rep(NA, 6)

  # <-><-><-><->

  tmp1[1] <- find_poi(srch_range = c(0, 500), model = r_cics, sl = lsl,
                      mode = "minimal",  alpha = 0.05, ivl = "confidence",
                      ivl_type = "one.sided", ivl_side = "lower")
  tmp1[2] <- find_poi(srch_range = c(0, 500), model = r_dics1, sl = lsl,
                      mode = "minimal",  alpha = 0.05, ivl = "confidence",
                      ivl_type = "one.sided", ivl_side = "lower")
  tmp1[3] <- find_poi(srch_range = c(0, 500), model = r_dics2, sl = lsl,
                      mode = "minimal",  alpha = 0.05, ivl = "confidence",
                      ivl_type = "one.sided", ivl_side = "lower")
  tmp1[4] <- find_poi(srch_range = c(0, 500), model = r_dids1, sl = lsl,
                      mode = "minimal",  alpha = 0.05, ivl = "confidence",
                      ivl_type = "one.sided", ivl_side = "lower")

  tmp2[1] <- find_poi(srch_range = c(0, 500), model = r_b2, sl = lsl,
                      mode = "minimal",  alpha = 0.05, ivl = "confidence",
                      ivl_type = "one.sided", ivl_side = "lower")
  tmp2[2] <- find_poi(srch_range = c(0, 500), model = r_b3, sl = lsl,
                      mode = "minimal",  alpha = 0.05, ivl = "confidence",
                      ivl_type = "one.sided", ivl_side = "lower")
  tmp2[3] <- find_poi(srch_range = c(0, 500), model = r_b4, sl = lsl,
                      mode = "minimal",  alpha = 0.05, ivl = "confidence",
                      ivl_type = "one.sided", ivl_side = "lower")
  tmp2[4] <- find_poi(srch_range = c(0, 500), model = r_b5, sl = lsl,
                      mode = "minimal",  alpha = 0.05, ivl = "confidence",
                      ivl_type = "one.sided", ivl_side = "lower")
  tmp2[5] <- find_poi(srch_range = c(0, 500), model = r_b7, sl = lsl,
                      mode = "minimal",  alpha = 0.05, ivl = "confidence",
                      ivl_type = "one.sided", ivl_side = "lower")
  tmp2[6] <- find_poi(srch_range = c(0, 500), model = r_b8, sl = lsl,
                      mode = "minimal",  alpha = 0.05, ivl = "confidence",
                      ivl_type = "one.sided", ivl_side = "lower")

  tmp3 <- find_poi(srch_range = c(0, 500), model = r_dids2, sl = lsl,
                   mode = "all",  alpha = 0.05, ivl = "confidence",
                   ivl_type = "one.sided", ivl_side = "lower")

  # <-><-><-><->

  expect_equal(signif(tmp1, 12),
               c(28.9857465765, 23.3972651235, 23.3972651235, 22.3112616922))
  expect_equal(signif(tmp2, 12),
               c(23.3263956198, 23.1159717387, 40.7917619344,
                 23.1480372703, 25.0525070104, 15.8448655130))
  expect_equivalent(signif(tmp3, 12),
                    c(23.4541475909, 26.4304167722, 36.7752481590,
                      22.9437282369, 24.2449267957, 14.6392554074))
})

test_that("find_poi_succeeds_two_sided", {
  r_moist <- stats::lm(Moisture ~ Month, data = exp3)
  usl <- 4
  lsl <- 1

  tmp <- rep(NA, 4)

  # <-><-><-><->

  tmp[1] <- find_poi(srch_range = c(0, 500), model = r_moist, sl = lsl,
                     mode = "minimal", alpha = 0.05, ivl = "confidence",
                     ivl_type = "two.sided", ivl_side = "lower")
  tmp[2] <- find_poi(srch_range = c(0, 500), model = r_moist, sl = usl,
                     mode = "minimal", alpha = 0.05, "confidence",
                     ivl_type = "two.sided", ivl_side = "upper")
  tmp[3] <- find_poi(srch_range = c(0, 500), model = r_moist, sl = lsl,
                     mode = "minimal", alpha = 0.2, ivl = "prediction",
                     ivl_type = "two.sided", ivl_side = "lower")
  tmp[4] <- find_poi(srch_range = c(0, 500), model = r_moist, sl = usl,
                     mode = "minimal", alpha = 0.05, ivl = "prediction",
                     ivl_type = "two.sided", ivl_side = "upper")

  # <-><-><-><->

  expect_equal(signif(tmp, 12),
               c(73.1928698050, 64.0441262510, 102.743203353, 41.3403352543))
})

test_that("find_poi_fails_with_warning_tight_uniroot_interval", {
  r_moist <- stats::lm(Moisture ~ Month, data = exp3)
  usl <- 4
  lsl <- 1

  # <-><-><-><->

  expect_error(
    find_poi(srch_range = c(0, 10), model = r_moist, sl = lsl, mode = "minimal",
             alpha = 0.05, ivl = "confidence", ivl_type = "one.sided",
             ivl_side = "lower"),
    "Error in uniroot")
  expect_error(
    find_poi(srch_range = c(0, 10), model = r_moist, sl = usl, mode = "minimal",
             alpha = 0.05, ivl = "confidence", ivl_type = "one.sided",
             ivl_side = "upper"),
    "Error in uniroot")
  expect_error(
    find_poi(srch_range = c(0, 10), model = r_moist, sl = lsl, mode = "minimal",
             alpha = 0.05, ivl = "prediction", ivl_type = "one.sided",
             ivl_side = "lower"),
    "Error in uniroot")
  expect_error(
    find_poi(srch_range = c(0, 10), model = r_moist, sl = usl, mode = "minimal",
             alpha = 0.05, ivl = "prediction", ivl_type = "one.sided",
             ivl_side = "upper"),
    "Error in uniroot")
})

test_that("find_poi_fails", {
  r_moist <- stats::lm(Moisture ~ Month, data = exp3)
  usl <- 4
  lsl <- 1

  # <-><-><-><->

  expect_error(
    find_poi(srch_range = 500, model = r_moist, sl = lsl, mode = "minimal",
             alpha = 0.05, ivl = "confidence", ivl_type = "one.sided",
             ivl_side = "lower"),
    "srch_range must be a vector of length 2")
  expect_error(
    find_poi(srch_range = "alpha", model = r_moist, sl = lsl, mode = "minimal",
             alpha = 0.05, ivl = "confidence", ivl_type = "one.sided",
             ivl_side = "lower"),
    "srch_range must be a vector of length 2")
  expect_error(
    find_poi(srch_range = c(0, 500), model = "r_moist", sl = lsl,
             mode = "minimal", alpha = 0.05, ivl = "confidence",
             ivl_type = "one.sided", ivl_side = "lower"),
    "Please provide a model of type \"lm\"")
  expect_error(
    find_poi(srch_range = c(0, 500), model = r_moist, sl = "SL",
             mode = "minimal", alpha = 0.05, ivl = "confidence",
             ivl_type = "one.sided", ivl_side = "lower"),
    "sl must be a numeric value of length 1")
  expect_error(
    find_poi(srch_range = c(0, 500), model = r_moist, sl = c(95, 105),
             mode = "minimal", alpha = 0.05, ivl = "confidence",
             ivl_type = "one.sided", ivl_side = "lower"),
    "sl must be a numeric value of length 1")
  expect_error(
    find_poi(srch_range = c(0, 500), model = r_moist, sl = lsl,
             mode = "maximal", alpha = 0.05, ivl = "confidence",
             ivl_type = "one.sided", ivl_side = "lower"),
    "specify mode either as \"minimal\" or \"all\"")
  expect_error(
    find_poi(srch_range = c(0, 500), model = r_moist, sl = lsl,
             mode = "minimal", alpha = 5, ivl = "confidence",
             ivl_type = "one.sided", ivl_side = "lower"),
    "specify alpha")
  expect_error(
    find_poi(srch_range = c(0, 500), model = r_moist, sl = lsl,
             mode = "minimal", alpha = -1, ivl = "confidence",
             ivl_type = "one.sided", ivl_side = "lower"),
    "specify alpha")
  expect_error(
    find_poi(srch_range = c(0, 500), model = r_moist, sl = lsl,
             mode = "minimal", alpha = 0.05, ivl = "incorrect",
             ivl_type = "one.sided", ivl_side = "lower"),
    "specify ivl either as \"confidence\" or \"prediction\"")
  expect_error(
    find_poi(srch_range = c(0, 500), model = r_moist, sl = lsl,
             mode = "minimal", alpha = 0.05, ivl = "confidence",
             ivl_type = "incorrect", ivl_side = "lower"),
    "specify ivl_type either as \"one.sided\" or \"two.sided\"")
  expect_error(
    find_poi(srch_range = c(0, 500), model = r_moist, sl = lsl,
             mode = "minimal", alpha = 0.05, ivl = "confidence",
             ivl_type = "one.sided", ivl_side = "incorrect"),
    "specify ivl_side either as \"lower\" or \"upper\"")
})
