context("Fitting of linear models")

test_that("get_linear_models_succeeds", {
  l_res <-
    get_linear_models(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                      response_vbl = "Potency", time_vbl = "Month",
                      batch_vbl = "Batch")

  t_icpt <- vapply(l_res$Models[1:3], function(x) coef(x)["(Intercept)"],
                   numeric(1))
  t_slp <- vapply(l_res$Models[1:3], function(x) coef(x)["Month"],
                  numeric(1))

  t_indiv_icpt <-
    vapply(l_res$Models$dids, function(x) coef(x)["(Intercept)"],
           numeric(1))
  t_indiv_slp <-
    vapply(l_res$Models$dids, function(x) coef(x)["Month"],
           numeric(1))

  # <-><-><-><->

  expect_equivalent(signif(t_icpt, 12),
               c(100.566878981, 100.363753745, 100.249139280))
  expect_equivalent(signif(t_slp, 12),
               c(-0.192993630573, -0.192720191732, -0.180125195618))

  expect_equivalent(signif(t_indiv_icpt, 12),
               c(100.249139280, 100.781872268, 100.634357661))
  expect_equivalent(signif(t_indiv_slp, 12),
               c(-0.180125195618, -0.208608547839, -0.188390951771))
})

test_that("get_linear_models_succeeds_with_a_single_batch", {
  l_res <-
    get_linear_models(data = exp1[exp1$Batch == "b2", ],
                      response_vbl = "Potency", time_vbl = "Month",
                      batch_vbl = "Batch")

  t_icpt <- vapply(l_res$Models[["dids"]], function(x) coef(x)["(Intercept)"],
                   numeric(1))
  t_slp <- vapply(l_res$Models[["dids"]], function(x) coef(x)["Month"],
                  numeric(1))

  # <-><-><-><->

  expect_equivalent(signif(t_icpt, 12), 100.249139280)
  expect_equivalent(signif(t_slp, 12), -0.180125195618)
})

test_that("get_linear_models_fails", {
  tmp <- exp1
  tmp$Batch <- as.character(tmp$Batch)

  # <-><-><-><->

  expect_error(
    get_linear_models(
      data = as.matrix(exp1[exp1$Batch %in% c("b2", "b5", "b7"), 2:3]),
      response_vbl = "Potency", time_vbl = "Month", batch_vbl = "Batch"),
    "data must be provided as data frame")
  expect_error(
    get_linear_models(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                      response_vbl = 4, time_vbl = "Month",
                      batch_vbl = "Batch"),
    "response_vbl must be a string")
  expect_error(
    get_linear_models(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                      response_vbl = "Mass", time_vbl = "Month",
                      batch_vbl = "Batch"),
    "response_vbl was not found in the provided data frame")
  expect_error(
    get_linear_models(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                      response_vbl = "Potency", time_vbl = 3,
                      batch_vbl = "Batch"),
    "time_vbl must be a string")
  expect_error(
    get_linear_models(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                      response_vbl = "Potency", time_vbl = "Time",
                      batch_vbl = "Batch"),
    "time_vbl was not found in the provided data frame")
  expect_error(
    get_linear_models(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                      response_vbl = "Potency", time_vbl = "Month",
                      batch_vbl = 2),
    "batch_vbl must be a string")
  expect_error(
    get_linear_models(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                      response_vbl = "Potency", time_vbl = "Month",
                      batch_vbl = "Lot"),
    "batch_vbl was not found in the provided data frame")
  expect_error(
    get_linear_models(data = tmp[tmp$Batch %in% c("b2", "b5", "b7"), ],
                      response_vbl = "Potency", time_vbl = "Month",
                      batch_vbl = "Batch"),
    "column in data specified by batch_vbl")
  expect_error(
    get_linear_models(data = tmp[tmp$Batch %in% c("b2", "b5", "b7"), ],
                      response_vbl = "Potency", time_vbl = "Month",
                      batch_vbl = "Batch"),
    "column in data specified by batch_vbl")
})
