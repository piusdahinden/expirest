context("What-if (approach for) shelf life estimation")

test_that("expirest_wisle_estimation_succeeds_for_poi", {
  usl <- 4.5
  lsl <- 0.5
  lrl <- 2.0
  url <- 3.0

  l_res <- list()

  # <-><-><-><->

  l_res[[length(l_res) + 1]] <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = lrl, rl_sf = 3, sl = lsl, sl_sf = 2,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower")[["POI"]]
  l_res[[length(l_res) + 1]] <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = lrl, rl_sf = 3, sl = lsl, sl_sf = 2,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "prediction", ivl_type = "one.sided", ivl_side = "lower")[["POI"]]
  l_res[[length(l_res) + 1]] <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = url, rl_sf = 3, sl = usl, sl_sf = 2,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "upper")[["POI"]]
  l_res[[length(l_res) + 1]] <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = url, rl_sf = 3, sl = usl, sl_sf = 2,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "prediction", ivl_type = "one.sided", ivl_side = "upper")[["POI"]]
  l_res[[length(l_res) + 1]] <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = lrl, rl_sf = 3, sl = lsl, sl_sf = 2,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "two.sided", ivl_side = "lower")[["POI"]]
  l_res[[length(l_res) + 1]] <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = lrl, rl_sf = 3, sl = lsl, sl_sf = 2,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "prediction", ivl_type = "two.sided", ivl_side = "lower")[["POI"]]
  l_res[[length(l_res) + 1]] <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = url, rl_sf = 3, sl = usl, sl_sf = 2,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "two.sided", ivl_side = "upper")[["POI"]]
  l_res[[length(l_res) + 1]] <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = url, rl_sf = 3, sl = usl, sl_sf = 2,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "prediction", ivl_type = "two.sided", ivl_side = "upper")[["POI"]]

  # <-><-><-><->

  expect_equal(signif(vapply(l_res, function(x) x[1, "Shelf.Life.cics"],
                             double(1)), 12),
               c(92.9989852223, 76.2160112925, 74.0855957780, 56.6107606557,
                 77.1293866352, 55.3445128338, 63.7755228959, 40.8959602433))
  expect_equal(signif(vapply(l_res, function(x) x[1, "Shelf.Life.dics"],
                             double(1)), 12),
               c(89.8717192065, 72.2813738660, 71.7431531307, 53.3870614673,
                 74.1940890259, 50.8809265809, 61.3857588081, 36.7607873047))
  expect_equal(signif(vapply(l_res, function(x) x[1, "Shelf.Life.dids"],
                             double(1)), 12),
               c(32.0995817441, 28.3162280937, 34.6801744921, 18.8338397352,
                 28.4323876929, NA, 31.149707448336, NA))
})

test_that("expirest_wisle_estimation_succeeds_with_transformations", {
  tmp <- rep(NA, 7)

  # <-><-><-><->

  tmp[1] <-
    expirest_wisle(
      data = exp2, response_vbl = "Related", time_vbl = "Month",
      batch_vbl = "Batch", rl = 0.15, rl_sf = 3, sl = 0.3, sl_sf = 2,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("log", "log"), shift = c(1, 0), sf_option = "tight",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "upper"
    )[["POI"]][1, "Shelf.Life.dids"]
  tmp[2] <-
    expirest_wisle(
      data = exp2, response_vbl = "Related", time_vbl = "Month",
      batch_vbl = "Batch", rl = 0.15, rl_sf = 3, sl = 0.3, sl_sf = 2,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("sqrt", "sqrt"), shift = c(0, 0), sf_option = "tight",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "upper"
    )[["POI"]][1, "Shelf.Life.dids"]
  tmp[3] <-
    expirest_wisle(
      data = exp2, response_vbl = "Related", time_vbl = "Month",
      batch_vbl = "Batch", rl = 0.15, rl_sf = 3, sl = 0.3, sl_sf = 2,
      srch_range = c(0, 5000), alpha = 0.05, alpha_pool = 0.25,
      xform = c("sq", "sq"), shift = c(0, 0), sf_option = "tight",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "upper"
    )[["POI"]][1, "Shelf.Life.dids"]

  tmp[4] <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = 2.0, rl_sf = 3, sl = c(0.5, 4.5),
      sl_sf = c(2, 2), srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("sq", "sq"), shift = c(0, 0),
      sf_option = "tight", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "lower")[["POI"]][1, "Shelf.Life.dids"]
  tmp[5] <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = 2.0, rl_sf = 3, sl = c(0.5, 4.5),
      sl_sf = c(2, 2), srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("sq", "no"), shift = c(0, 0),
      sf_option = "tight", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "lower")[["POI"]][1, "Shelf.Life.dids"]
  tmp[6] <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = 3.0, rl_sf = 3, sl = c(0.5, 4.5),
      sl_sf = c(2, 2), srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("sq", "sq"), shift = c(0, 0),
      sf_option = "tight", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "upper")[["POI"]][1, "Shelf.Life.dids"]
  tmp[7] <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = 3.0, rl_sf = 3, sl = c(0.5, 4.5),
      sl_sf = c(2, 2), srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("sq", "no"), shift = c(0, 0),
      sf_option = "tight", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "upper")[["POI"]][1, "Shelf.Life.dids"]

  # <-><-><-><->

  expect_equal(signif(tmp, 12),
               c(1.24633645239, 9.92303429998, 14.2215253811, 24.0097015693,
                 26.9485678209, 33.2240413518, 29.2139414080))
})

test_that("expirest_wisle_estimation_succeeds_for_model_type", {
  t_dat1 <- exp1[exp1$Batch %in% c("b2", "b5", "b7"), ]
  t_dat2 <- exp1[exp1$Batch %in% c("b3", "b4", "b5"), ]
  t_dat3 <- exp1[exp1$Batch %in% c("b4", "b5", "b8"), ]

  usl <- 105
  lsl <- 95
  lrl <- 98

  # <-><-><-><->

  r_exp1 <-
    expirest_wisle(
      data = t_dat1, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = lrl, rl_sf = 2, sl = lsl, sl_sf = 2,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower")
  r_exp2 <-
    expirest_wisle(
      data = t_dat2, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = lrl, rl_sf = 2, sl = lsl, sl_sf = 2,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower")
  r_exp3 <-
    expirest_wisle(
      data = t_dat3, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = lrl, rl_sf = 2, sl = lsl, sl_sf = 2,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower")

  # <-><-><-><->

  expect_is(r_exp1, "expirest_wisle")
  expect_is(r_exp2, "expirest_wisle")
  expect_is(r_exp3, "expirest_wisle")

  expect_equal(r_exp1[["Model.Type"]][[2]], "cics")
  expect_equal(r_exp2[["Model.Type"]][[2]], "dics")
  expect_equal(r_exp3[["Model.Type"]][[2]], "dids")

  expect_equivalent(r_exp1[["Model.Type"]][[1]], c(1, 1))
  expect_equivalent(r_exp2[["Model.Type"]][[1]], c(0, 1))
  expect_equivalent(r_exp3[["Model.Type"]][[1]], c(0, 0))
})

test_that("expirest_wisle_warns", {
  usl <- 3.5
  lsl <- 1.5
  lrl <- 2.0

  # <-><-><-><->

  expect_warning(
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = lrl, rl_sf = 2, sl = lsl, sl_sf = 2,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "prediction", ivl_type = "one.sided", ivl_side = "lower"),
    "Not for all model types POI values obtained.")
  expect_warning(
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = lrl, rl_sf = 2, sl = lsl, sl_sf = 2,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "prediction", ivl_type = "one.sided", ivl_side = "lower"),
    "Not for all model types POI values obtained.")
})

test_that("expirest_wisle_fails_with_warning_tight_spec_limits", {
  usl <- 3.5
  lsl <- 1.5
  url <- 3.0
  lrl <- 2.0

  tmp <- numeric(2)

  # <-><-><-><->

  tmp[1] <-
    suppressWarnings(
      expirest_wisle(
        data = exp3, response_vbl = "Moisture", time_vbl = "Month",
        batch_vbl = "Batch", rl = lrl, rl_sf = 2, sl = lsl, sl_sf = 2,
        srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
        xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
        ivl = "prediction", ivl_type = "two.sided", ivl_side = "lower"
      )[["POI"]][1, "Shelf.Life.cics"])
  tmp[2] <-
    suppressWarnings(
      expirest_wisle(
        data = exp3, response_vbl = "Moisture", time_vbl = "Month",
        batch_vbl = "Batch", rl = lrl, rl_sf = 2, sl = lsl, sl_sf = 2,
        srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
        xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
        ivl = "prediction", ivl_type = "two.sided", ivl_side = "lower"
      )[["POI"]][1, "Shelf.Life.cics"])

  # <-><-><-><->

  expect_equal(is.na(tmp), rep(TRUE, 2))
})

test_that("expirest_wisle_fails_with_warning_tight_uniroot_interval", {
  t_dat <- exp1[exp1$Batch %in% c("b2", "b5", "b7"), ]

  usl <- 105
  lsl <- 95
  url <- 103
  lrl <- 97

  tmp <- numeric(4)

  # <-><-><-><->

  tmp[1] <-
    suppressWarnings(
      expirest_wisle(
        data = exp1, response_vbl = "Potency", time_vbl = "Month",
        batch_vbl = "Batch", rl = lrl, rl_sf = 2, sl = lsl, sl_sf = 2,
        srch_range = c(0, 3), alpha = 0.05, alpha_pool = 0.25,
        xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
        ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"
      )[["POI"]][1, "Shelf.Life.cics"])
  tmp[2] <-
    suppressWarnings(
      expirest_wisle(
        data = exp1, response_vbl = "Potency", time_vbl = "Month",
        batch_vbl = "Batch", rl = lrl, rl_sf = 2, sl = lsl, sl_sf = 2,
        srch_range = c(0, 3), alpha = 0.05, alpha_pool = 0.25,
        xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
        ivl = "prediction", ivl_type = "one.sided", ivl_side = "lower"
      )[["POI"]][1, "Shelf.Life.cics"])
  tmp[3] <-
    suppressWarnings(
      expirest_wisle(
        data = exp1, response_vbl = "Potency", time_vbl = "Month",
        batch_vbl = "Batch", rl = url, rl_sf = 2, sl = usl, sl_sf = 3,
        srch_range = c(0, 3), alpha = 0.05, alpha_pool = 0.25,
        xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
        ivl = "confidence", ivl_type = "one.sided", ivl_side = "upper"
      )[["POI"]][1, "Shelf.Life.cics"])
  tmp[4] <-
    suppressWarnings(
      expirest_wisle(
        data = exp1, response_vbl = "Potency", time_vbl = "Month",
        batch_vbl = "Batch", rl = url, rl_sf = 2, sl = usl, sl_sf = 3,
        srch_range = c(0, 3), alpha = 0.05, alpha_pool = 0.25,
        xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
        ivl = "prediction", ivl_type = "one.sided", ivl_side = "upper"
      )[["POI"]][1, "Shelf.Life.cics"])

  # <-><-><-><->

  expect_equal(is.na(tmp), rep(TRUE, 4))
})

test_that("expirest_wisle_fails_with_error", {
  t_dat <- exp1[exp1$Batch %in% c("b2", "b5", "b7"), ]
  t_dal <- t_dat
  t_dal$Batch <- as.character(t_dal$Batch)

  # <-><-><-><->

  expect_error(
    expirest_wisle(
      data = as.matrix(t_dat[, c("Month", "Potency")]),
      response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "data must be provided as data frame")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = 2, time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "response_vbl must be a string")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Mass", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "response_vbl was not found in the provided data frame")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = 3,
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "time_vbl must be a string")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Year",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "time_vbl was not found in the provided data frame")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = 4, rl = 98, rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "batch_vbl must be a string")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Lot", rl = 98, rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "batch_vbl was not found in the provided data frame")
  expect_error(
    expirest_wisle(
      data = t_dal, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "column in data specified by batch_vbl")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = "98", rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "rl must be a numeric")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = "3", sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "rl_sf must be a positive integer")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = -3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "rl_sf must be a positive integer")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = c(3, 3), sl = 95,
      sl_sf = 3, srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "rl_sf must be a positive integer")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3.3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "rl_sf must be a positive integer")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = "95", sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "sl must be a numeric")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = c(95, 100, 105),
      sl_sf = 3, srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "sl must be a numeric or vector of length 1 or 2")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = c(105, 95),
      sl_sf = 3, srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "sl must be of the form")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = "3",
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "sl_sf must be a positive integer")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = -3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "sl_sf must be a positive integer")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = c(3, 4),
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "sl_sf must be a positive integer")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = 3.3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "sl_sf must be a positive integer")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = "alpha", alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "srch_range must be a vector of length 2")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = 500, alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "srch_range must be a vector of length 2")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 5, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "specify alpha")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = -1, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "specify alpha")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 5,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "specify alpha_pool")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = -1,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "specify alpha_pool")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = "no", shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "specify xform appropriately")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("yes", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "specify xform appropriately")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "yes"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "specify xform appropriately")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c("no", "no"), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "shift must be a numeric vector of length 2")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = 1, sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "shift must be a numeric vector of length 2")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "incorrect",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "specify sf_option either as \"tight\" or \"loose\"")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "incorrect", ivl_type = "one.sided", ivl_side = "lower"),
    "specify ivl either as \"confidence\" or \"prediction\"")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "incorrect", ivl_side = "lower"),
    "specify ivl_type either as \"one.sided\" or \"two.sided\"")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 98, rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "middle"),
    "specify ivl_side either as \"lower\" or \"upper\"")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 94, rl_sf = 3, sl = 95, sl_sf = 3,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower"),
    "If ivl_side is \"lower\" rl must be > sl")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 106, rl_sf = 3, sl = 105, sl_sf = 4,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "upper"),
    "If ivl_side is \"upper\" rl must be < sl")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 94, rl_sf = 3, sl = c(95, 105),
      sl_sf = c(3, 4), srch_range = c(0, 500), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "loose", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "lower"),
    "If ivl_side is \"lower\" rl must be > sl")
  expect_error(
    expirest_wisle(
      data = t_dat, response_vbl = "Potency", time_vbl = "Month",
      batch_vbl = "Batch", rl = 106, rl_sf = 3, sl = c(95, 105),
      sl_sf = c(3, 4), srch_range = c(0, 500), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "loose", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "upper"),
    "If ivl_side is \"upper\" rl must be < sl")
})
