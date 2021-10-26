context("Getting intercepts")

test_that("get_icpt_succeeds", {
  response_vbl = "Potency"
  time_vbl = "Month"
  batch_vbl = "Batch"

  l_models <- list()

  # ---------
  # common intercept / common slope
  t_formula <- paste(response_vbl, "~", time_vbl)
  l_models[[length(l_models) + 1]] <-
    do.call("lm", list(as.formula(t_formula),
                       data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ]))

  # ---------
  # different intercept / common slope
  t_formula <- paste(response_vbl, "~", paste(batch_vbl, time_vbl, sep = " + "))
  l_models[[length(l_models) + 1]] <-
    do.call("lm", list(as.formula(t_formula),
                       data = exp1[exp1$Batch %in% c("b3", "b4", "b5"), ]))

  # ---------
  # different intercept / different slope
  t_formula <- paste(response_vbl, "~", paste(batch_vbl, time_vbl, sep = " * "))
  l_models[[length(l_models) + 1]] <-
    do.call("lm", list(as.formula(t_formula),
                       data = exp1[exp1$Batch %in% c("b4", "b5", "b8"), ]))

  names(l_models) <- c("cics", "dics", "dids")

  # <-><-><-><->

  tmp1 <- get_icpt(model = l_models[["cics"]], response_vbl = "Potency",
                   time_vbl = "Month",  batch_vbl = "Batch",
                   xform = c("no", "no"), shift = c(0, 0))$icpt
  tmp2 <- get_icpt(model = l_models[["dics"]], response_vbl = "Potency",
                   time_vbl = "Month",  batch_vbl = "Batch",
                   xform = c("no", "no"), shift = c(0, 0))$icpt
  tmp3 <- get_icpt(model = l_models[["dids"]], response_vbl = "Potency",
                   time_vbl = "Month",  batch_vbl = "Batch",
                   xform = c("no", "no"), shift = c(0, 0))$icpt

  # <-><-><-><->

  expect_equivalent(signif(tmp1, 12), 100.566878981)
  expect_equivalent(signif(tmp2, 12),
                    c(102.175653110, 104.255189423, 100.820021871))
  expect_equivalent(signif(tmp3, 12),
                    c(104.070645793, 100.781872268, 101.259375000))
})

test_that("get_icpt_succeeds_with_transformations", {
  l_models <- list()

  # ---------
  # Log transformation of y
  t_dat <- exp1[exp1$Batch %in% c("b2", "b5", "b7"), ]
  t_dat$Potency <- log(t_dat$Potency + 1)

  l_models[[length(l_models) + 1]] <- lm(Potency ~ Month, data = t_dat)

  # ---------
  # Sqrt transformation of y
  t_dat <- exp1[exp1$Batch %in% c("b2", "b5", "b7"), ]
  t_dat$Potency <- sqrt(t_dat$Potency + 1)

  l_models[[length(l_models) + 1]] <- lm(Potency ~ Month, data = t_dat)

  # ---------
  # Sq transformation of y
  t_dat <- exp1[exp1$Batch %in% c("b2", "b5", "b7"), ]
  t_dat$Potency <- (t_dat$Potency + 1)^2

  l_models[[length(l_models) + 1]] <- lm(Potency ~ Month, data = t_dat)

  # ---------
  # Log transformation of x and sq transformation of y
  t_dat <- exp2[exp2$Batch %in% c("b4", "b5"), ]
  t_dat$Month <- log(t_dat$Month + 1)
  t_dat$Related <- (t_dat$Related)^2

  l_models[[length(l_models) + 1]] <- lm(Related ~ Month, data = t_dat)

  # <-><-><-><->

  tmp1 <- get_icpt(model = l_models[[1]], response_vbl = "Potency",
                   time_vbl = "Month",  batch_vbl = "Batch",
                   xform = c("no", "log"), shift = c(0, 1))
  tmp2 <- get_icpt(model = l_models[[2]], response_vbl = "Potency",
                   time_vbl = "Month",  batch_vbl = "Batch",
                   xform = c("no", "sqrt"), shift = c(0, 1))
  tmp3 <- get_icpt(model = l_models[[3]], response_vbl = "Potency",
                   time_vbl = "Month",  batch_vbl = "Batch",
                   xform = c("no", "sq"), shift = c(0, 1))
  tmp4 <- get_icpt(model = l_models[[4]], response_vbl = "Related",
                   time_vbl = "Month",  batch_vbl = "Batch",
                   xform = c("log", "sq"), shift = c(1, 0))

  # <-><-><-><->

  expect_equivalent(
    signif(c(tmp1$icpt, tmp2$icpt, tmp3$icpt, tmp4$icpt), 12),
    c(4.62074256238, 10.0781010131, 10315.3651287, -0.000602359481633))
  expect_equivalent(
    signif(c(tmp1$icpt.orig, tmp2$icpt.orig, tmp3$icpt.orig, tmp4$icpt.orig),
           12),
    c(100.569425768, 100.568120031, 100.564585997, NA))
})

test_that("get_icpt_fails", {
  r_potency <- stats::lm(Potency ~ Batch + Month,
                         data = exp1[exp1$Batch %in% c("b3", "b4", "b5"), ])

  # <-><-><-><->

  expect_error(
    get_icpt(model = "r_potency", response_vbl = "Potency", time_vbl = "Month",
             batch_vbl = "Batch", xform = c("no", "no"),
             shift = c(0, 0)),
    "Please provide a model of type \"lm\"")
  expect_error(
    get_icpt(model = r_potency, response_vbl = 1, time_vbl = "Month",
             batch_vbl = "Batch", xform = c("no", "no"),
             shift = c(0, 0)),
    "response_vbl must be a string")
  expect_error(
    get_icpt(model = r_potency, response_vbl = "Mass", time_vbl = "Month",
             batch_vbl = "Batch", xform = c("no", "no"),
             shift = c(0, 0)),
    "response_vbl was not found in the provided model")
  expect_error(
    get_icpt(model = r_potency, response_vbl = "Potency", time_vbl = 2,
             batch_vbl = "Batch", xform = c("no", "no"),
             shift = c(0, 0)),
    "time_vbl must be a string")
  expect_error(
    get_icpt(model = r_potency, response_vbl = "Potency", time_vbl = "Time",
             batch_vbl = "Batch", xform = c("no", "no"),
             shift = c(0, 0)),
    "time_vbl was not found in the provided model")
  expect_error(
    get_icpt(model = r_potency, response_vbl = "Potency", time_vbl = "Month",
             batch_vbl = 3, xform = c("no", "no"),
             shift = c(0, 0)),
    "batch_vbl must be a string")
  expect_error(
    get_icpt(model = r_potency, response_vbl = "Potency", time_vbl = "Month",
             batch_vbl = "Lot", xform = c("no", "no"),
             shift = c(0, 0)),
    "batch_vbl was not found in the provided model")
  expect_error(
    get_icpt(model = r_potency, response_vbl = "Potency", time_vbl = "Month",
             batch_vbl = "Batch", xform = "no",
             shift = c(0, 0)),
    "specify xform appropriately")
  expect_error(
    get_icpt(model = r_potency, response_vbl = "Potency", time_vbl = "Month",
             batch_vbl = "Batch", xform = c("no", "yes"),
             shift = c(0, 0)),
    "specify xform appropriately")
  expect_error(
    get_icpt(model = r_potency, response_vbl = "Potency", time_vbl = "Month",
             batch_vbl = "Batch", xform = c("no", "no"),
             shift = "no"),
    "shift must be a numeric vector of length 2")
  expect_error(
    get_icpt(model = r_potency, response_vbl = "Potency", time_vbl = "Month",
             batch_vbl = "Batch", xform = c("no", "no"),
             shift = 1),
    "shift must be a numeric vector of length 2")
})
