context("Get relevant limits")

test_that("get_relevant_limits_succeeds", {
  sl <- 95
  sl_sf <- 2
  rl <- 98
  rl_sf <- 2
  xform <- c("no", "no")
  ivl_side <- "lower"

  re <-
    expirest_osle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                  response_vbl = "Potency", time_vbl = "Month",
                  batch_vbl = "Batch", sl = sl, sl_sf = 2,
                  srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
                  xform = xform, shift = c(0, 0), sf_option = "tight",
                  ivl = "confidence", ivl_type = "one.sided",
                  ivl_side = ivl_side, rl = rl, rl_sf = rl_sf)

  tmp1 <- get_relevant_limits(limits_list = re[["Limits"]],
                              xform = xform, ivl_side = ivl_side)

  # <-><-><-><->

  expect_equal(tmp1[["sl.orig"]], 95)
  expect_equal(tmp1[["sl"]], 95)
  expect_equal(tmp1[["rl.orig"]], 98)
  expect_equal(tmp1[["rl"]], 98)
})

test_that("get_relevant_limits_succeeds_with_transformations", {
  sl <- 0.3
  sl_sf <- 2
  rl = 0.15
  rl_sf = 3
  xform <- c("log", "log")
  ivl_side <- "upper"

  re <-
    expirest_osle(
      data = exp2, response_vbl = "Related", time_vbl = "Month",
      batch_vbl = "Batch", sl = 0.3, sl_sf = 2, srch_range = c(0, 500),
      alpha = 0.05, alpha_pool = 0.25, xform = xform,
      shift = c(1, 0), sf_option = "tight", ivl = "confidence",
      ivl_type = "one.sided", ivl_side = ivl_side, rl = rl, rl_sf = rl_sf)

  tmp1 <- get_relevant_limits(limits_list = re[["Limits"]],
                              xform = xform, ivl_side = ivl_side)

  # <-><-><-><->

  expect_equal(signif(tmp1[["sl.orig"]], 3), 0.3)
  expect_equal(signif(tmp1[["sl"]], 12), -1.20397280433)
  expect_equal(signif(tmp1[["rl.orig"]], 3), 0.15)
  expect_equal(signif(tmp1[["rl"]], 12), -1.89711998489)
})

test_that("get_relevant_limits_fails", {
  sl <- 95
  sl_sf <- 2
  rl <- 98
  rl_sf <- 2
  xform <- c("no", "no")
  ivl_side <- "lower"

  re <-
    expirest_osle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                  response_vbl = "Potency", time_vbl = "Month",
                  batch_vbl = "Batch", sl = sl, sl_sf = 2,
                  srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
                  xform = xform, shift = c(0, 0), sf_option = "tight",
                  ivl = "confidence", ivl_type = "one.sided",
                  ivl_side = ivl_side, rl = rl, rl_sf = rl_sf)

  tmp <- re[["Limits"]]
  names(tmp) <- gsub("sl", "xl", names(tmp))

  # <-><-><-><->

  expect_error(
    get_relevant_limits(limits_list = "limits",
                        xform = xform, ivl_side = ivl_side),
    "limits_list must be a list")
  expect_error(
    get_relevant_limits(limits_list = tmp,
                        xform = xform, ivl_side = ivl_side),
    "limits_list must have at least the elements")
  expect_error(
    get_relevant_limits(limits_list = re[["Limits"]],
                        xform = "no", ivl_side = ivl_side),
    "specify xform appropriately")
  expect_error(
    get_relevant_limits(limits_list = re[["Limits"]],
                        xform = c("yes", "no"), ivl_side = ivl_side),
    "specify xform appropriately")
  expect_error(
    get_relevant_limits(limits_list = re[["Limits"]],
                        xform = c("no", "yes"), ivl_side = ivl_side),
    "specify xform appropriately")
  expect_error(
    get_relevant_limits(limits_list = re[["Limits"]],
                        xform = xform, ivl_side = "middle"),
    "specify ivl_side either as \"lower\", \"upper\" or \"both\"")
  expect_error(
    get_relevant_limits(limits_list = re[["Limits"]],
                        xform = xform, ivl_side = "both"),
    "Please provide a specification with two sides.")
})
