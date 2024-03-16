context("What-if (approach for) shelf life estimation")

test_that("expirest_wisle_estimation_succeeds_for_poi", {
  l_res <- vector(mode = "list", length = 8)

  # <-><-><-><->

  l_res[[1]] <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = 2.0, rl_sf = 3, sl = 0.5, sl_sf = 1,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower")[["POI"]]
  l_res[[2]] <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = 2.0, rl_sf = 3, sl = 0.5, sl_sf = 1,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "prediction", ivl_type = "one.sided", ivl_side = "lower")[["POI"]]
  l_res[[3]] <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = 3.0, rl_sf = 3, sl = 4.5, sl_sf = 2,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "upper")[["POI"]]
  l_res[[4]] <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = 3.0, rl_sf = 3, sl = 4.5, sl_sf = 2,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "prediction", ivl_type = "one.sided", ivl_side = "upper")[["POI"]]
  l_res[[5]] <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = 2.0, rl_sf = 3, sl = 0.5, sl_sf = 1,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "two.sided", ivl_side = "lower")[["POI"]]
  l_res[[6]] <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = 2.0, rl_sf = 3, sl = 0.5, sl_sf = 1,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "prediction", ivl_type = "two.sided", ivl_side = "lower")[["POI"]]
  l_res[[7]] <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = 3.0, rl_sf = 3, sl = 4.5, sl_sf = 2,
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "two.sided", ivl_side = "upper")[["POI"]]
  l_res[[8]] <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = 3.0, rl_sf = 3, sl = 4.5, sl_sf = 2,
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
  expect_equal(signif(vapply(l_res, function(x) x[1, "Shelf.Life.dids.pmse"],
                             double(1)), 12),
               c(33.1646535749, 21.6496175929, 31.4687060221, 19.7997864734,
                 29.8437393901, NA, 28.4629285602, NA))
  expect_equal(signif(vapply(l_res, function(x) x[1, "Shelf.Life.dids"],
                             double(1)), 12),
               c(32.0995817441, 28.3162280937, 34.6801744921, 18.8338397352,
                 28.4323876929, NA, 31.149707448336, NA))
})

test_that("expirest_wisle_estimation_succeeds_with_multiple_rl", {
  re1 <-
    expirest_wisle(
      data = exp2, response_vbl = "Related", time_vbl = "Month",
      batch_vbl = "Batch", rl = seq(0.06, 0.18, 0.02), rl_sf = rep(3, 7),
      sl = 0.3, sl_sf = 2, srch_range = c(0, 500), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "tight", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "upper")[["POI"]]
  re2 <-
    expirest_wisle(
      data = exp2, response_vbl = "Related", time_vbl = "Month",
      batch_vbl = "Batch", rl = seq(0.24, 0.34, 0.02), rl_sf = rep(3, 6),
      sl = 0.3, sl_sf = 1, srch_range = c(0, 500), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "tight", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "upper")[["POI"]]

  # <-><-><-><->

  expect_equal(signif(re1[, "Intercept.cics"], 12), rep(0.103507214947, 7))
  expect_equal(signif(re1[, "Intercept.dics"], 12), rep(0.135353267317, 7))
  expect_equal(signif(re1[, "Intercept.dids"], 12), rep(0.112218750000, 7))
  expect_equal(signif(re1[, "Intercept.dids.pmse"], 12),
               c(rep(0.112218750000, 6), 0.126543831957))
  expect_equal(signif(re1[, "Delta.cics"], 2),
               c(0.24, 0.22, 0.20, 0.18, 0.16, 0.14, 0.12))
  expect_equal(signif(re1[, "Delta.dics"], 2),
               c(0.24, 0.22, 0.20, 0.18, 0.16, 0.14, 0.12))
  expect_equal(signif(re1[, "Delta.dids"], 2),
               c(0.24, 0.22, 0.20, 0.18, 0.16, 0.14, 0.12))
  expect_equal(signif(re1[, "Delta.dids.pmse"], 2),
               c(0.24, 0.22, 0.20, 0.18, 0.16, 0.14, 0.12))
  expect_equal(signif(re1[, "WCSL.cics"], 12),
               c(0.343507214947, 0.323507214947, 0.303507214947, 0.283507214947,
                 0.263507214947, 0.243507214947, 0.223507214947))
  expect_equal(signif(re1[, "WCSL.dics"], 12),
               c(0.375353267317, 0.355353267317, 0.335353267317, 0.315353267317,
                 0.295353267317, 0.275353267317, 0.255353267317))
  expect_equal(signif(re1[, "WCSL.dids"], 12),
               c(0.352218750000, 0.332218750000, 0.312218750000, 0.292218750000,
                 0.272218750000, 0.252218750000, 0.232218750000))
  expect_equal(signif(re1[, "WCSL.dids.pmse"], 12),
               c(0.352218750000, 0.332218750000, 0.312218750000, 0.292218750000,
                 0.272218750000, 0.252218750000, 0.246543831957))
  expect_equal(signif(re1[, "Shelf.Life.cics"], 12),
               c(33.7023497447, 31.0532056728, 28.3929462116, 25.7169240050,
                 23.0173720770, 20.2808184067, 17.4818579854))
  expect_equal(signif(re1[, "Shelf.Life.dics"], 12),
               c(32.8209904768, 30.0399757667, 27.2465732221, 24.4371980849,
                 21.6069843973, 18.7493384428, 15.8554616662))
  expect_equal(signif(re1[, "Shelf.Life.dids"], 12),
               c(19.9814248519, 18.4029062578, 16.8177772438, 15.2229638050,
                 13.6133111973, 11.9793567488, 10.3030302984))
  expect_equal(signif(re1[, "Shelf.Life.dids.pmse"], 12),
               c(19.6600800851, 18.1137833890, 16.5601981827, 14.9959803788,
                 13.4153149598, 11.8078402523, 11.3444065974))
  expect_equal(signif(re1[, "POI.Model.cics"], 12), rep(27.9249797381, 7))
  expect_equal(signif(re1[, "POI.Model.dics"], 12), rep(22.2667191569, 7))
  expect_equal(signif(re1[, "POI.Model.dids"], 12), rep(15.8448655130, 7))
  expect_equal(signif(re1[, "POI.Model.dids.pmse"], 12), rep(15.6061043981, 7))

  expect_equal(signif(re2[, "Shelf.Life.dids"], 12),
               c(4.49097470563, 3.82712999791, NA, NA, NA, NA))
  expect_equal(signif(re2[, "Shelf.Life.dids.pmse"], 12),
               c(4.31135902795, 1.81271654695, 0.91856665721, NA, NA, NA))
  expect_equal(signif(re2[, "POI.Model.dids"], 12), rep(15.8448655130, 6))
  expect_equal(signif(re2[, "POI.Model.dids.pmse"], 12), rep(15.6061043981, 6))
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

test_that("expirest_wisle_estimation_succeeds_with_a_single_batch", {
  l_res <- vector(mode = "list", length = 3)

  # <-><-><-><->

  l_res[[1]] <-
    expirest_wisle(
      data = exp3[exp3$Batch == "b1", ], response_vbl = "Moisture",
      time_vbl = "Month", batch_vbl = "Batch", rl = 1.5, rl_sf = 3,
      sl = 4.5, sl_sf = 2, srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "tight", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "upper")[["POI"]]
  l_res[[2]] <-
    expirest_wisle(
      data = exp3[exp3$Batch == "b2", ], response_vbl = "Moisture",
      time_vbl = "Month", batch_vbl = "Batch", rl = 1.5, rl_sf = 3,
      sl = 4.5, sl_sf = 2, srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "tight", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "upper")[["POI"]]
  l_res[[3]] <-
    expirest_wisle(
      data = exp3[exp3$Batch == "b3", ], response_vbl = "Moisture",
      time_vbl = "Month", batch_vbl = "Batch", rl = 1.5, rl_sf = 3,
      sl = 4.5, sl_sf = 2, srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "no"), shift = c(0, 0),
      sf_option = "tight", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "upper")[["POI"]]

  # <-><-><-><->

  expect_equal(is.na(vapply(l_res, function(x) x[1, "Shelf.Life.cics"],
                            double(1))), rep(TRUE, 3))
  expect_equal(is.na(vapply(l_res, function(x) x[1, "Shelf.Life.dics"],
                            double(1))), rep(TRUE, 3))
  expect_equal(is.na(vapply(l_res, function(x) x[1, "Shelf.Life.dids.pmse"],
                            double(1))), rep(TRUE, 3))
  expect_equal(signif(vapply(l_res, function(x) x[1, "Shelf.Life.dids"],
                             double(1)), 12),
               c(64.7437051332, 152.223693637, 63.9895243084))
})

test_that("expirest_wisle_estimation_succeeds_with_a_single_batch_xfrmd", {
  tmp <- rep(NA, 3)

  # <-><-><-><->

  tmp[1] <-
    expirest_wisle(
      data = exp3[exp3$Batch == "b1", ], response_vbl = "Moisture",
      time_vbl = "Month", batch_vbl = "Batch", rl = 1.5, rl_sf = 3,
      sl = 4.5, sl_sf = 2, srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("sq", "sq"), shift = c(0, 0),
      sf_option = "tight", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "upper")[["POI"]][1, "Shelf.Life.dids"]
  tmp[2] <-
    expirest_wisle(
      data = exp3[exp3$Batch == "b1", ], response_vbl = "Moisture",
      time_vbl = "Month", batch_vbl = "Batch", rl = 1.5, rl_sf = 3,
      sl = 4.5, sl_sf = 2, srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("sq", "no"), shift = c(0, 0),
      sf_option = "tight", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "upper")[["POI"]][1, "Shelf.Life.dids"]
  tmp[3] <-
    expirest_wisle(
      data = exp3[exp3$Batch == "b1", ], response_vbl = "Moisture",
      time_vbl = "Month", batch_vbl = "Batch", rl = 1.5, rl_sf = 3,
      sl = 4.5, sl_sf = 2, srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("no", "sq"), shift = c(0, 0),
      sf_option = "tight", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "upper")[["POI"]][1, "Shelf.Life.dids"]

  # <-><-><-><->

  expect_equal(signif(tmp, 12),
               c(43.2875229176, 40.2443864798, 74.4096683313))
})

test_that("expirest_wisle_estimation_succeeds_for_model_type", {
  t_dat1 <- exp1[exp1$Batch %in% c("b2", "b5", "b7"), ]
  t_dat2 <- exp1[exp1$Batch %in% c("b3", "b4", "b5"), ]
  t_dat3 <- exp1[exp1$Batch %in% c("b4", "b5", "b8"), ]

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
  lsl <- 1.5
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
