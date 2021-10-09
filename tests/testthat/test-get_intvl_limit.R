context("Confidence or prediction interval limit")

test_that("get_intvl_limit_succeeds_simple", {
  r_pot <- stats::lm(Potency ~ Month,
                     data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ])
  usl <- 105
  lsl <- 95

  tmp1 <- rep(NA, 4)
  tmp2 <- rep(NA, 4)

  # <-><-><-><->

  tmp1[1] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                             ivl = "confidence", ivl_type = "one.sided",
                             ivl_side = "lower")
  tmp1[2] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                             ivl = "prediction", ivl_type = "one.sided",
                             ivl_side = "lower")
  tmp1[3] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                             ivl = "confidence", ivl_type = "one.sided",
                             ivl_side = "upper")
  tmp1[4] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                             ivl = "prediction", ivl_type = "one.sided",
                             ivl_side = "upper")

  tmp2[1] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                             ivl = "fitted.line", ivl_type = "one.sided",
                             ivl_side = "lower")
  tmp2[2] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                             ivl = "fitted.line", ivl_type = "one.sided",
                             ivl_side = "upper")
  tmp2[3] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                             ivl = "fitted.line", ivl_type = "two.sided",
                             ivl_side = "lower")
  tmp2[4] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                             ivl = "fitted.line", ivl_type = "two.sided",
                             ivl_side = "upper")

  # <-><-><-><->

  expect_equal(signif(tmp1, 12),
               c(95.4363220855, 94.5044959376, 96.4337416087, 97.3655677567))
  expect_equal(signif(tmp2, 12),
               c(95.9350318471, 95.9350318471, 95.9350318471, 95.9350318471))
})

test_that("get_intvl_limit_succeeds_multiple", {
  r_pot <- stats::lm(Potency ~ Batch + Month,
                     data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ])
  usl <- 105
  lsl <- 95

  tmp1 <- matrix(NA, nrow = 4, ncol = 3)
  tmp2 <- matrix(NA, nrow = 4, ncol = 3)

  # <-><-><-><->

  tmp1[1, ] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                               ivl = "confidence", ivl_type = "one.sided",
                               ivl_side = "lower")
  tmp1[2, ] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                               ivl = "prediction", ivl_type = "one.sided",
                               ivl_side = "lower")
  tmp1[3, ] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                               ivl = "confidence", ivl_type = "one.sided",
                               ivl_side = "upper")
  tmp1[4, ] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                               ivl = "prediction", ivl_type = "one.sided",
                               ivl_side = "upper")

  tmp2[1, ] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                               ivl = "fitted.line", ivl_type = "one.sided",
                               ivl_side = "lower")
  tmp2[2, ] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                               ivl = "fitted.line", ivl_type = "one.sided",
                               ivl_side = "upper")
  tmp2[3, ] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                               ivl = "fitted.line", ivl_type = "two.sided",
                               ivl_side = "lower")
  tmp2[4, ] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                               ivl = "fitted.line", ivl_type = "two.sided",
                               ivl_side = "upper")

  # <-><-><-><->

  expect_equal(signif(tmp1[, 1], 12),
               c(95.1212749178, 94.2361299862, 96.3556633686, 97.2408083002))
  expect_equal(signif(tmp1[, 2], 12),
               c(95.4050190819, 94.5199009752, 96.6394985934, 97.5246167000))
  expect_equal(signif(tmp1[, 3], 12),
               c(95.4312749178, 94.5461299862, 96.6656633686, 97.5508083002))

  expect_equal(signif(tmp2[, 1], 12),
               c(95.7384691432, 95.7384691432, 95.7384691432, 95.7384691432))
  expect_equal(signif(tmp2[, 2], 12),
               c(96.0222588376, 96.0222588376, 96.0222588376, 96.0222588376))
  expect_equal(signif(tmp2[, 3], 12),
               c(96.0484691432, 96.0484691432, 96.0484691432, 96.0484691432))
})

test_that("get_intvl_limit_succeeds_interaction", {
  r_pot <- stats::lm(Potency ~ Batch * Month,
                     data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ])
  usl <- 105
  lsl <- 95

  tmp1 <- matrix(NA, nrow = 4, ncol = 3)
  tmp2 <- matrix(NA, nrow = 4, ncol = 3)

  # <-><-><-><->

  tmp1[1, ] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                               ivl = "confidence", ivl_type = "one.sided",
                               ivl_side = "lower")
  tmp1[2, ] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                               ivl = "prediction", ivl_type = "one.sided",
                               ivl_side = "lower")
  tmp1[3, ] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                               ivl = "confidence", ivl_type = "one.sided",
                               ivl_side = "upper")
  tmp1[4, ] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                               ivl = "prediction", ivl_type = "one.sided",
                               ivl_side = "upper")

  tmp2[1, ] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                               ivl = "fitted.line", ivl_type = "one.sided",
                               ivl_side = "lower")
  tmp2[2, ] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                               ivl = "fitted.line", ivl_type = "one.sided",
                               ivl_side = "upper")
  tmp2[3, ] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                               ivl = "fitted.line", ivl_type = "two.sided",
                               ivl_side = "lower")
  tmp2[4, ] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                               ivl = "fitted.line", ivl_type = "two.sided",
                               ivl_side = "upper")

  # <-><-><-><->

  expect_equal(signif(tmp1[, 1], 12),
               c(95.0139020968, 94.2428935457, 96.8383670738, 97.6093756249))
  expect_equal(signif(tmp1[, 2], 12),
               c(94.8654255049, 94.0933206140, 96.6851087350, 97.4572136259))
  expect_equal(signif(tmp1[, 3], 12),
               c(95.2007423301, 94.4297337790, 97.0252073071, 97.7962158582))

  expect_equal(signif(tmp2[, 1], 12),
               c(95.9261345853, 95.9261345853, 95.9261345853, 95.9261345853))
  expect_equal(signif(tmp2[, 2], 12),
               c(95.7752671200, 95.7752671200, 95.7752671200, 95.7752671200))
  expect_equal(signif(tmp2[, 3], 12),
               c(96.1129748186, 96.1129748186, 96.1129748186, 96.1129748186))
})

test_that("get_intvl_limit_fails", {
  r_pot <- stats::lm(Potency ~ Month,
                     data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ])

  usl <- 105
  lsl <- 95

  tmp <- rep(NA, 2)

  # <-><-><-><->

  tmp[1] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 1E-32,
                            ivl = "confidence", ivl_type = "one.sided",
                            ivl_side = "upper")
  tmp[2] <- get_intvl_limit(x_new = 24, model = r_pot, alpha = 1E-32,
                            ivl = "prediction", ivl_type = "one.sided",
                            ivl_side = "upper")

  # <-><-><-><->

  expect_equal(signif(tmp, 12), c(Inf, Inf))

  # <-><-><-><->

  expect_error(
    get_intvl_limit(x_new = "alpha", model = r_pot, alpha = 0.05,
                    ivl = "confidence", ivl_type = "one.sided",
                    ivl_side = "lower"),
    "x_new must be a numeric")
  expect_error(
    get_intvl_limit(x_new = 24, model = "r_pot", alpha = 0.05,
                    ivl = "confidence", ivl_type = "one.sided",
                    ivl_side = "lower"),
    "Please provide a model of type \"lm\"")
  expect_error(
    get_intvl_limit(x_new = 24, model = r_pot, alpha = 5,
                    ivl = "confidence", ivl_type = "one.sided",
                    ivl_side = "lower"),
    "specify alpha")
  expect_error(
    get_intvl_limit(x_new = 24, model = r_pot, alpha = -1,
                    ivl = "confidence", ivl_type = "one.sided",
                    ivl_side = "lower"),
    "specify alpha")
  expect_error(
    get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                    ivl = "incorrect", ivl_type = "one.sided",
                    ivl_side = "lower"),
    "specify ivl either as \"confidence\" or \"prediction\"")
  expect_error(
    get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                    ivl = "confidence", ivl_type = "incorrect",
                    ivl_side = "lower"),
    "specify ivl_type either as \"one.sided\" or \"two.sided\"")
  expect_error(
    get_intvl_limit(x_new = 24, model = r_pot, alpha = 0.05,
                    ivl = "confidence", ivl_type = "one.sided",
                    ivl_side = "incorrect"),
    "specify ivl_side either as \"lower\" or \"upper\"")
})
