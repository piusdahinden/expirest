context("Getting intercepts of worst case batches")

test_that("get_wc_icpt_succeeds", {
  response_vbl <- "Potency"
  time_vbl <- "Month"
  batch_vbl <- "Batch"
  xform <- c("no", "no")
  shift <- c(0, 0)

  # Models
  l_mod <-
    get_linear_models(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                      response_vbl = response_vbl, time_vbl = time_vbl,
                      batch_vbl = batch_vbl)$Models

  # Intercepts
  l_icpt <- vapply(l_mod[c("cics", "dics", "dids.pmse")], function(x) {
    list(get_icpt(model = x, response_vbl = response_vbl,
                  time_vbl = time_vbl, batch_vbl = batch_vbl,
                  xform = xform, shift = shift))
  },
  list(1))
  tmp <- vapply(l_mod[["dids"]], function(x) {
    list(get_icpt(model = x, response_vbl = response_vbl,
                  time_vbl = time_vbl, batch_vbl = batch_vbl,
                  xform = xform, shift = shift))
  },
  list(1))
  tmp <- unlist(tmp)
  names(tmp) <- sub("\\.\\(Intercept\\)", "", names(tmp))
  names(tmp) <- sub("\\.icpt", "", names(tmp))
  l_icpt <- c(l_icpt, list(dids = list(icpt = tmp)))

  # POIs
  l_poi <- get_poi_list(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                        batch_vbl = batch_vbl, model_list = l_mod,
                        sl = 95, srch_range = c(0, 500), alpha = 0.05,
                        ivl = "confidence", ivl_type = "one.sided",
                        ivl_side = "lower")

  # Worst case batches
  wc_batch <- vapply(l_poi, function(x) {
    ifelse(!length(which.min(x)), NA, which.min(x))
  },
  numeric(1))
  wc_batch["cics"] <- NA

  # Intercepts of worst case batches
  re <- get_wc_icpt(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                    batch_vbl = batch_vbl, icpt_list = l_icpt, poi_list = l_poi,
                    wc_batch = wc_batch, xform = xform)

  # <-><-><-><->

  expect_equivalent(signif(re, 12), c(100.566878981, 100.363753745,
                                      100.781872268, 100.781872268))
})

test_that("get_wc_icpt_succeeds_with_a_single_batch", {
  response_vbl <- "Potency"
  time_vbl <- "Month"
  batch_vbl <- "Batch"
  xform <- c("no", "no")
  shift <- c(0, 0)

  # Models
  l_mod <-
    get_linear_models(data = exp1[exp1$Batch == "b2", ],
                      response_vbl = response_vbl, time_vbl = time_vbl,
                      batch_vbl = batch_vbl)$Models

  # Intercepts
  l_icpt <- list(cics = NA, dics = NA, dids.pmse = NA)
  tmp <- vapply(l_mod[["dids"]], function(x) {
    list(get_icpt(model = x, response_vbl = response_vbl,
                  time_vbl = time_vbl, batch_vbl = batch_vbl,
                  xform = xform, shift = shift))
  },
  list(1))
  tmp <- unlist(tmp)
  names(tmp) <- sub("\\.\\(Intercept\\)", "", names(tmp))
  names(tmp) <- sub("\\.icpt", "", names(tmp))
  l_icpt <- c(l_icpt, list(dids = list(icpt = tmp)))

  # POIs
  l_poi <- get_poi_list(data = exp1[exp1$Batch == "b2", ],
                        batch_vbl = batch_vbl, model_list = l_mod,
                        sl = 95, srch_range = c(0, 500), alpha = 0.05,
                        ivl = "confidence", ivl_type = "one.sided",
                        ivl_side = "lower")

  # Worst case batches
  wc_batch <- vapply(l_poi, function(x) {
    ifelse(!length(which.min(x)), NA, which.min(x))
  },
  numeric(1))
  wc_batch["cics"] <- NA

  # Intercepts of worst case batches
  re <- get_wc_icpt(data = exp1[exp1$Batch == "b2", ],
                    batch_vbl = batch_vbl, icpt_list = l_icpt, poi_list = l_poi,
                    wc_batch = wc_batch, xform = xform)

  # <-><-><-><->

  expect_equivalent(signif(re, 12), c(NA, NA, NA, 100.249139280))
})

test_that("get_wc_icpt_succeeds_with_transformations", {
  t_dat <- exp1[exp1$Batch %in% c("b2", "b5", "b7"), ]

  response_vbl <- "Potency"
  time_vbl <- "Month"
  batch_vbl <- "Batch"
  xform <- c("no", "log")
  shift <- c(0, 1)
  sf_option <- "loose"

  t_dat <-
    get_xformed_variables(data = t_dat, response_vbl = response_vbl,
                          time_vbl = time_vbl, xform = xform, shift = shift)

  l_variables <-
    get_variable_list(response_vbl = response_vbl, time_vbl = time_vbl,
                      batch_vbl = batch_vbl, xform = xform)
  response_vbl <- l_variables[["response"]]

  # Models
  l_mod <-
    get_linear_models(data = t_dat, response_vbl = response_vbl,
                      time_vbl = time_vbl, batch_vbl = batch_vbl)$Models

  # Limits
  l_lim <-
    set_limits(rl = NA, rl_sf = NA, sl = 95, sl_sf = 2,
               sf_option = sf_option, xform = xform, shift = shift,
               ivl_side = "lower")
  sl_orig <- l_lim[["sl.orig"]]
  sl <- l_lim[["sl.trfmd"]][1]

  # Intercepts
  l_icpt <- vapply(l_mod[c("cics", "dics", "dids.pmse")], function(x) {
    list(get_icpt(model = x, response_vbl = response_vbl,
                  time_vbl = time_vbl, batch_vbl = batch_vbl,
                  xform = xform, shift = shift))
  },
  list(1))
  tmp <- vapply(l_mod[["dids"]], function(x) {
    list(get_icpt(model = x, response_vbl = response_vbl,
                  time_vbl = time_vbl, batch_vbl = batch_vbl,
                  xform = xform, shift = shift))
  },
  list(1))
  tmp <- unlist(tmp)
  names(tmp) <- sub("\\.\\(Intercept\\)", "", names(tmp))
  t_i_orig <- grep("orig", names(tmp))
  names(tmp) <- sub("\\.icpt", "", names(tmp))
  names(tmp) <- sub("\\.orig", "", names(tmp))

  l_icpt <- c(l_icpt, list(dids = list(icpt = tmp[-t_i_orig],
                                       icpt.orig = tmp[t_i_orig])))

  # POIs
  l_poi <- get_poi_list(data = t_dat, batch_vbl = batch_vbl, model_list = l_mod,
                        sl = sl, srch_range = c(0, 5000), alpha = 0.05,
                        ivl = "confidence", ivl_type = "one.sided",
                        ivl_side = "lower")

  # Worst case batches
  wc_batch <- vapply(l_poi, function(x) {
    ifelse(!length(which.min(x)), NA, which.min(x))
  },
  numeric(1))
  wc_batch["cics"] <- NA

  # Intercepts of worst case batches
  re <- get_wc_icpt(data = t_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                    poi_list = l_poi, wc_batch = wc_batch, xform = xform)

  # <-><-><-><->

  expect_equivalent(signif(re, 12), c(100.569425768, 100.364043220,
                                      100.785095948, 100.785095948))
})

test_that("get_wc_icpt_succeeds_with_transformations_with_a_single_batch", {
  t_dat <- exp1[exp1$Batch == "b2", ]

  response_vbl <- "Potency"
  time_vbl <- "Month"
  batch_vbl <- "Batch"
  xform <- c("no", "log")
  shift <- c(0, 1)
  sf_option <- "loose"

  t_dat <-
    get_xformed_variables(data = t_dat, response_vbl = response_vbl,
                          time_vbl = time_vbl, xform = xform, shift = shift)

  l_variables <-
    get_variable_list(response_vbl = response_vbl, time_vbl = time_vbl,
                      batch_vbl = batch_vbl, xform = xform)
  response_vbl <- l_variables[["response"]]

  # Models
  l_mod <-
    get_linear_models(data = t_dat, response_vbl = response_vbl,
                      time_vbl = time_vbl, batch_vbl = batch_vbl)$Models

  # Limits
  l_lim <-
    set_limits(rl = NA, rl_sf = NA, sl = 95, sl_sf = 2,
               sf_option = sf_option, xform = xform, shift = shift,
               ivl_side = "lower")
  sl_orig <- l_lim[["sl.orig"]]
  sl <- l_lim[["sl.trfmd"]][1]

  # Intercepts
  l_icpt <- list(cics = NA, dics = NA, dids.pmse = NA)
  tmp <- vapply(l_mod[["dids"]], function(x) {
    list(get_icpt(model = x, response_vbl = response_vbl,
                  time_vbl = time_vbl, batch_vbl = batch_vbl,
                  xform = xform, shift = shift))
  },
  list(1))
  tmp <- unlist(tmp)
  names(tmp) <- sub("\\.\\(Intercept\\)", "", names(tmp))
  t_i_orig <- grep("orig", names(tmp))
  names(tmp) <- sub("\\.icpt", "", names(tmp))
  names(tmp) <- sub("\\.orig", "", names(tmp))

  l_icpt <- c(l_icpt, list(dids = list(icpt = tmp[-t_i_orig],
                                       icpt.orig = tmp[t_i_orig])))

  # POIs
  l_poi <- get_poi_list(data = t_dat, batch_vbl = batch_vbl, model_list = l_mod,
                        sl = sl, srch_range = c(0, 5000), alpha = 0.05,
                        ivl = "confidence", ivl_type = "one.sided",
                        ivl_side = "lower")

  # Worst case batches
  wc_batch <- vapply(l_poi, function(x) {
    ifelse(!length(which.min(x)), NA, which.min(x))
  },
  numeric(1))
  wc_batch["cics"] <- NA

  # Intercepts of worst case batches
  re <- get_wc_icpt(data = t_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                    poi_list = l_poi, wc_batch = wc_batch, xform = xform)

  # <-><-><-><->

  expect_equivalent(signif(re, 12), c(NA, NA, NA, 100.249965235))
})

test_that("get_wc_icpt_fails", {
  t_dat <- exp1[exp1$Batch %in% c("b2", "b5", "b7"), ]

  response_vbl <- "Potency"
  time_vbl <- "Month"
  batch_vbl <- "Batch"
  xform <- c("no", "no")
  shift <- c(0, 0)

  # Models
  l_mod <-
    get_linear_models(data = t_dat, response_vbl = response_vbl,
                      time_vbl = time_vbl, batch_vbl = batch_vbl)$Models

  # POIs
  l_poi <- get_poi_list(data = t_dat, batch_vbl = batch_vbl, model_list = l_mod,
                        sl = 95, srch_range = c(0, 500), alpha = 0.05,
                        ivl = "confidence", ivl_type = "one.sided",
                        ivl_side = "lower")

  # Intercepts
  l_icpt <- vapply(l_mod[c("cics", "dics", "dids.pmse")], function(x) {
    list(get_icpt(model = x, response_vbl = response_vbl,
                  time_vbl = time_vbl, batch_vbl = batch_vbl,
                  xform = xform, shift = shift))
  },
  list(1))
  tmp <- vapply(l_mod[["dids"]], function(x) {
    list(get_icpt(model = x, response_vbl = response_vbl,
                  time_vbl = time_vbl, batch_vbl = batch_vbl,
                  xform = xform, shift = shift))
  },
  list(1))
  tmp <- unlist(tmp)
  names(tmp) <- sub("\\.\\(Intercept\\)", "", names(tmp))
  names(tmp) <- sub("\\.icpt", "", names(tmp))
  l_icpt <- c(l_icpt, list(dids = list(icpt = tmp)))

  # Worst case batches
  wc_batch <- vapply(l_poi, function(x) {
    ifelse(!length(which.min(x)), NA, which.min(x))
  },
  numeric(1))
  wc_batch["cics"] <- NA

  # <-><-><-><->
  # Data frames and lists with unexpected formats
  t_dal <- t_dat
  t_dal$Batch <- as.character(t_dal$Batch)

  l_icpt2 <- l_icpt
  names(l_icpt2) <- gsub("dids.pmse", "individual", names(l_icpt))

  l_poi2 <- l_poi
  names(l_poi2) <- gsub("dids.pmse", "individual", names(l_poi))

  wc_batch3 <- wc_batch2 <- wc_batch
  wc_batch2 <- as.character(wc_batch2[!is.na(wc_batch2)])
  names(wc_batch3) <- gsub("dids.pmse", "individual", names(wc_batch))

  # <-><-><-><->

  expect_error(
    get_wc_icpt(data = as.matrix(t_dat[, c("Month", "Potency")]),
                batch_vbl = batch_vbl, icpt_list = l_icpt, poi_list = l_poi,
                wc_batch = wc_batch, xform = xform),
    "data must be provided as data frame")
  expect_error(
    get_wc_icpt(data = t_dat, batch_vbl = 3, icpt_list = l_icpt,
                poi_list = l_poi, wc_batch = wc_batch, xform = xform),
    "batch_vbl must be a string")
  expect_error(
    get_wc_icpt(data = t_dat, batch_vbl = "Lot", icpt_list = l_icpt,
                poi_list = l_poi, wc_batch = wc_batch, xform = xform),
    "batch_vbl was not found in the provided data frame")
  expect_error(
    get_wc_icpt(data = t_dal,  batch_vbl = batch_vbl, icpt_list = l_icpt,
                poi_list = l_poi, wc_batch = wc_batch, xform = xform),
    "column in data specified by batch_vbl")
  expect_error(
    get_wc_icpt(data = t_dat, batch_vbl = batch_vbl, icpt_list = "l_icpt",
                poi_list = l_poi, wc_batch = wc_batch, xform = xform),
    "icpt_list must be a list")
  expect_error(
    get_wc_icpt(data = t_dat, batch_vbl = batch_vbl, icpt_list = l_icpt2,
                poi_list = l_poi, wc_batch = wc_batch, xform = xform),
    "icpt_list must have four elements named ")
  expect_error(
    get_wc_icpt(data = t_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                poi_list = "l_poi", wc_batch = wc_batch, xform = xform),
    "poi_list must be a list")
  expect_error(
    get_wc_icpt(data = t_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                poi_list = l_poi2, wc_batch = wc_batch, xform = xform),
    "poi_list must have four elements named ")
  expect_error(
    get_wc_icpt(data = t_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                poi_list = l_poi, wc_batch = wc_batch2, xform = xform),
    "wc_batch must be a numeric vector")
  expect_error(
    get_wc_icpt(data = t_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                poi_list = l_poi, wc_batch = wc_batch3, xform = xform),
    "wc_batch must have four elements named ")
  expect_error(
    get_wc_icpt(data = t_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                poi_list = l_poi, wc_batch = wc_batch, xform = "no"),
    "specify xform appropriately")
  expect_error(
    get_wc_icpt(data = t_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                poi_list = l_poi, wc_batch = wc_batch, xform = c("yes", "no")),
    "specify xform appropriately")
  expect_error(
    get_wc_icpt(data = t_dat, batch_vbl = batch_vbl, icpt_list = l_icpt,
                poi_list = l_poi, wc_batch = wc_batch, xform = c("no", "yes")),
    "specify xform appropriately")
})
