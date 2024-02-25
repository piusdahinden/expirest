context("Get predictions for graphical illustration")

test_that("get_predictions_succeeds", {
  re1 <-
    expirest_osle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", sl = c(0.5, 4.5), sl_sf = c(1, 2),
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "upper")
  re2 <-
    expirest_wisle(
      data = exp3, response_vbl = "Moisture", time_vbl = "Month",
      batch_vbl = "Batch", rl = 3.00, rl_sf = 3, sl = c(0.5, 4.5),
      sl_sf = c(1, 2), srch_range = c(0, 5000), alpha = 0.05,
      alpha_pool = 0.25, xform = c("sq", "sq"), shift = c(0, 0),
      sf_option = "tight", ivl = "confidence", ivl_type = "one.sided",
      ivl_side = "upper")

  # <-><-><-><->

  tmp1 <-
    get_predictions(model = re1, model_name = "dids", x_range = c(-1, 30))
  tmp2 <-
    get_predictions(model = re2, model_name = "dids", x_range = c(-1, 30))


  # <-><-><-><->

  expect_equivalent(signif(tmp1[1, c("Moisture", "LL", "UL")], 12),
                    c(2.31227901409, 1.71161162484, 2.91294640333))
  expect_equivalent(signif(tmp1[300, c("Moisture", "LL", "UL")], 12),
                    c(2.96758174357, 2.26107897182, 3.67408451531))

  expect_equivalent(signif(tmp2[1, c("Moisture", "LL", "UL")], 12),
                    c(2.40747963887, 1.87032949836, 2.84495764304))
  expect_equivalent(signif(tmp2[300, c("Moisture", "LL", "UL")], 12),
                    c(3.12873144406, 2.11151448251, 3.88837080128))
})

test_that("get_predictions_fails", {
  re <-
    expirest_osle(
      data = exp4, response_vbl = "Conc", time_vbl = "Month",
      batch_vbl = "Batch", sl = c(95.0, 105.0), sl_sf = c(3, 4),
      srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
      xform = c("no", "no"), shift = c(0, 0), sf_option = "loose",
      ivl = "confidence", ivl_type = "one.sided", ivl_side = "lower")

  # <-><-><-><->

  expect_error(
    get_predictions(model = "re", model_name = "dids", x_range = c(-1, 30)),
    "model must be an object of class expirest_osle or expirest_wisle")
  expect_error(
    get_predictions(model = re, model_name = "both", x_range = c(-1, 30)),
    "specify model_name either as \"cics\", \"dics\", ")
  expect_error(
    get_predictions(model = re, model_name = "dids", x_range = "x_range"),
    "x_range must be a vector")
  expect_error(
    get_predictions(model = re, model_name = "dids", x_range = c(-1, 15, 30)),
    "x_range must be a vector of length 2")
  expect_error(
    get_predictions(model = re, model_name = "dids", x_range = c(30, -1)),
    "x_range must be of the form")
 })
