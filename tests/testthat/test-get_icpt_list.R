context("Compilation of list of intercepts")

test_that("get_icpt_list_succeeds", {
  d_dat <- exp1[exp1$Batch %in% c("b2", "b5", "b7"), ]
  response_vbl <- "Potency"
  time_vbl <- "Month"
  batch_vbl <- "Batch"
  xform <- c("no", "no")
  shift <- c(0, 0)

  l_models <-
    get_model_list(data = d_dat, response_vbl = response_vbl,
                   time_vbl = time_vbl, batch_vbl = batch_vbl)$Models

  # <-><-><-><->

  l_icpt <- get_icpt_list(data = d_dat, response_vbl = response_vbl,
                          time_vbl = time_vbl, batch_vbl = batch_vbl,
                          model_list = l_models, xform = xform, shift = shift)

  # <-><-><-><->

  expect_equivalent(signif(l_icpt[["cics"]][["icpt"]], 12), 100.566878981)
  expect_equivalent(signif(l_icpt[["dics"]][["icpt"]], 12),
                    c(100.363753745, 100.647543439, 100.673753745))
  expect_equivalent(signif(l_icpt[["dids.pmse"]][["icpt"]], 12),
                    c(100.249139280, 100.781872268, 100.634357661))
  expect_equivalent(signif(l_icpt[["dids"]][["icpt"]], 12),
                    c(100.249139280, 100.781872268, 100.634357661))
})

test_that("get_icpt_list_fails", {
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

  t_dat1 <- as.matrix(d_dat[, c("Month", "Potency")])
  t_dat2 <- d_dat
  t_dat2$Batch <- as.character(t_dat2$Batch)
  l_models_err <- l_models[1:3]

  # <-><-><-><->

  expect_error(
    get_icpt_list(data = t_dat1, response_vbl = response_vbl,
                  time_vbl = time_vbl, batch_vbl = batch_vbl,
                  model_list = l_models, xform = xform, shift = shift),
    "data must be provided as data frame")
  expect_error(
    get_icpt_list(data = d_dat, response_vbl = 3,
                  time_vbl = time_vbl, batch_vbl = batch_vbl,
                  model_list = l_models, xform = xform, shift = shift),
    "response_vbl must be a string")
  expect_error(
    get_icpt_list(data = d_dat, response_vbl = "Assay",
                  time_vbl = time_vbl, batch_vbl = batch_vbl,
                  model_list = l_models, xform = xform, shift = shift),
    "response_vbl was not found in the provided data frame")
  expect_error(
    get_icpt_list(data = d_dat, response_vbl = response_vbl,
                  time_vbl = 2, batch_vbl = batch_vbl,
                  model_list = l_models, xform = xform, shift = shift),
    "time_vbl must be a string")
  expect_error(
    get_icpt_list(data = d_dat, response_vbl = response_vbl,
                  time_vbl = "Day", batch_vbl = batch_vbl,
                  model_list = l_models, xform = xform, shift = shift),
    "time_vbl was not found in the provided data frame")
  expect_error(
    get_icpt_list(data = d_dat, response_vbl = response_vbl,
                  time_vbl = time_vbl, batch_vbl = 4,
                  model_list = l_models, xform = xform, shift = shift),
    "batch_vbl must be a string")
  expect_error(
    get_icpt_list(data = d_dat, response_vbl = response_vbl,
                  time_vbl = time_vbl, batch_vbl = "Lot",
                  model_list = l_models, xform = xform, shift = shift),
    "batch_vbl was not found in the provided data frame")
  expect_error(
    get_icpt_list(data = t_dat2, response_vbl = response_vbl,
                  time_vbl = time_vbl, batch_vbl = batch_vbl,
                  model_list = l_models, xform = xform, shift = shift),
    "column in data specified by batch_vbl")
  expect_error(
    get_icpt_list(data = d_dat, response_vbl = response_vbl,
                  time_vbl = time_vbl, batch_vbl = batch_vbl,
                  model_list = "l_models", xform = xform, shift = shift),
    "model_list must be a list")
  expect_error(
    get_icpt_list(data = d_dat, response_vbl = response_vbl,
                  time_vbl = time_vbl, batch_vbl = batch_vbl,
                  model_list = l_models_err, xform = xform, shift = shift),
    "model_list must have four elements")
  expect_error(
    get_icpt_list(data = d_dat, response_vbl = response_vbl,
                  time_vbl = time_vbl, batch_vbl = batch_vbl,
                  model_list = l_models, xform = "no", shift = shift),
    "specify xform appropriately")
  expect_error(
    get_icpt_list(data = d_dat, response_vbl = response_vbl,
                  time_vbl = time_vbl, batch_vbl = batch_vbl,
                  model_list = l_models, xform = c("yes", "no"), shift = shift),
    "specify xform appropriately")
  expect_error(
    get_icpt_list(data = d_dat, response_vbl = response_vbl,
                  time_vbl = time_vbl, batch_vbl = batch_vbl,
                  model_list = l_models, xform = c("no", "yes"), shift = shift),
    "specify xform appropriately")
  expect_error(
    get_icpt_list(data = d_dat, response_vbl = response_vbl,
                  time_vbl = time_vbl, batch_vbl = batch_vbl,
                  model_list = l_models, xform = xform, shift = c("no", "no")),
    "shift must be a numeric vector of length 2")
  expect_error(
    get_icpt_list(data = d_dat, response_vbl = response_vbl,
                  time_vbl = time_vbl, batch_vbl = batch_vbl,
                  model_list = l_models, xform = xform, shift = 1),
    "shift must be a numeric vector of length 2")
})
