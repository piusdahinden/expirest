context("Get worst-case scenario (wcs) limit")

test_that("get_wcs_limit_succeeds", {
  tmp1 <- get_wcs_limit(rl = 97.00, sl = 95.0, intercept = 100,
                        xform = c("no", "no"), shift = c(0, 0),
                        ivl_side = "lower")
  tmp2 <- get_wcs_limit(rl = 103.00, sl = 105.0, intercept = 100,
                        xform = c("no", "no"), shift = c(0, 0),
                        ivl_side = "upper")

  tmp3 <- get_wcs_limit(rl = log(97.00 + 1), sl = log(95.0 + 1),
                        intercept = log(100 + 1),
                        xform = c("no", "log"), shift = c(0, 1),
                        ivl_side = "lower")
  tmp4 <- get_wcs_limit(rl = sqrt(97.00), sl = sqrt(95.0),
                        intercept = sqrt(100),
                        xform = c("no", "sqrt"), shift = c(0, 0),
                        ivl_side = "lower")
  tmp5 <- get_wcs_limit(rl = (97.00)^2, sl = (95.0)^2,
                        intercept = (100)^2,
                        xform = c("no", "sq"), shift = c(0, 0),
                        ivl_side = "lower")

  tmp6 <- get_wcs_limit(rl = 0.45, sl = 0.5, intercept = -0.1,
                        xform = c("no", "sq"), shift = c(0, 0),
                        ivl_side = "upper")

  # <-><-><-><->

  expect_equal(signif(tmp1[["delta.lim"]], 3), 2.00)
  expect_equal(signif(tmp1[["wcs.lim"]], 4), 98.00)

  expect_equal(signif(tmp2[["delta.lim"]], 3), 2.000)
  expect_equal(signif(tmp2[["wcs.lim"]], 4), 102.000)

  expect_equal(signif(tmp3[["delta.lim"]], 9), 0.0206192872)
  expect_equal(signif(tmp3[["wcs.lim"]], 9), 4.59450123)
  expect_equal(signif(tmp3[["wcs.lim.orig"]], 9), 97.9387755)

  expect_equal(signif(tmp4[["delta.lim"]], 9), 0.102063457)
  expect_equal(signif(tmp4[["wcs.lim"]], 9), 9.89793654)
  expect_equal(signif(tmp4[["wcs.lim.orig"]], 9), 97.9691478)

  expect_equal(signif(tmp5[["delta.lim"]], 3), 384.0)
  expect_equal(signif(tmp5[["wcs.lim"]], 4), 9616.0)
  expect_equal(signif(tmp5[["wcs.lim.orig"]], 9), 98.0612054)

  expect_equal(signif(tmp6[["delta.lim"]], 3), 0.0500)
  expect_equal(signif(tmp6[["wcs.lim"]], 4), -0.05000)
  expect_equivalent(tmp6[["wcs.lim.orig"]], NA)
})

test_that("get_wcs_limit_fails", {
  expect_error(
    get_wcs_limit(rl = "96.995", sl = 94.95, intercept = 100,
                  xform = c("no", "no"), shift = c(0, 0), ivl_side = "lower"),
    "rl must be a numeric")
  expect_error(
    get_wcs_limit(rl = 96.995, sl = "94.95", intercept = 100,
                  xform = c("no", "no"), shift = c(0, 0), ivl_side = "lower"),
    "sl must be a numeric")
  expect_error(
    get_wcs_limit(rl = 96.995, sl = 94.95, intercept = "100",
                  xform = c("no", "no"), shift = c(0, 0), ivl_side = "lower"),
    "intercept must be a numeric")
  expect_error(
    get_wcs_limit(rl = 96.995, sl = 94.95, intercept = 100,
                  xform = "no", shift = c(0, 0), ivl_side = "lower"),
    "specify xform appropriately")
  expect_error(
    get_wcs_limit(rl = 96.995, sl = 94.95, intercept = 100,
                  xform = c("no", "yes"), shift = c(0, 0), ivl_side = "lower"),
    "specify xform appropriately")
  expect_error(
    get_wcs_limit(rl = 96.995, sl = 94.95, intercept = 100,
                  xform = c("no", "no"), shift = "no", ivl_side = "lower"),
    "shift must be a numeric vector of length 2")
  expect_error(
    get_wcs_limit(rl = 96.995, sl = 94.95, intercept = 100,
                  xform = c("no", "no"), shift = 1, ivl_side = "lower"),
    "shift must be a numeric vector of length 2")
  expect_error(
    get_wcs_limit(rl = 96.995, sl = 94.95, intercept = 100,
                  xform = c("no", "no"), shift = c(0, 0), ivl_side = "middle"),
    "specify ivl_side either as \"lower\" or \"upper\"")
  expect_error(
    get_wcs_limit(rl = 94, sl = 95, intercept = 100, xform = c("no", "no"),
                  shift = c(0, 0), ivl_side = "lower"),
    "If ivl_side is \"lower\" rl must be > sl")
  expect_error(
    get_wcs_limit(rl = 106, sl = 105, intercept = 100, xform = c("no", "no"),
                  shift = c(0, 0), ivl_side = "upper"),
    "If ivl_side is \"upper\" rl must be < sl")
})
