context("Data transformation")

test_that("get_xformed_variables_succeeds", {
  tmp1 <- get_xformed_variables(data = exp2, response_vbl = "Related",
                                time_vbl = "Month", xform = c("log", "log"),
                                shift = c(1, 1))
  tmp2 <- get_xformed_variables(data = exp2, response_vbl = "Related",
                                time_vbl = "Month", xform = c("sqrt", "sqrt"),
                                shift = c(0, 1))
  tmp3 <- get_xformed_variables(data = exp2, response_vbl = "Related",
                                time_vbl = "Month", xform = c("sq", "sq"),
                                shift = c(0, 1))
  tmp4 <- get_xformed_variables(data = exp2, response_vbl = "Related",
                                time_vbl = "Month", xform = c("no", "no"),
                                shift = c(0, 0))

  # <-><-><-><->

  expect_equal(signif(tmp1[c(1, 2, 3, 5, 7), "log.Month"], 9),
               c(0.000000000, 1.38629436, 1.94591015, 2.56494936, 3.218875825))
  expect_equal(signif(tmp1[c(1, 2, 3, 5, 7), "log.Related"], 9),
               c(0.0295588022, 0.0525924501, 0.0639133257, 0.0751074725,
                 0.1629688283))

  expect_equal(signif(tmp2[c(1, 2, 3, 5, 7), "sqrt.Month"], 9),
               c(0.00000000, 1.73205081, 2.44948974, 3.46410162, 4.89897949))
  expect_equal(signif(tmp2[c(1, 2, 3, 5, 7), "sqrt.Related"], 9),
               c(1.01488916, 1.02664502, 1.03247276, 1.03826779, 1.08489631))

  expect_equal(signif(tmp3[c(1, 2, 3, 5, 7), "sq.Month"], 9),
               c(0.00000000, 9.00000000, 36.0000000, 144.000000, 576.000000))
  expect_equal(signif(tmp3[c(1, 2, 3, 5, 7), "sq.Related"], 9),
               c(1.06090000, 1.11091600, 1.13635600, 1.16208400, 1.38532900))

  expect_equal(signif(tmp4[c(1, 2, 3, 5, 7), "Month"], 9),
               c(0.00000000, 3.00000000, 6.00000000, 12.0000000, 24.0000000))
  expect_equal(signif(tmp4[c(1, 2, 3, 5, 7), "Related"], 9),
               c(0.0300000000, 0.0540000000, 0.0660000000, 0.0780000000,
                 0.177000000))
})

test_that("get_xformed_variables_fails", {
  expect_error(
    get_xformed_variables(data = "exp2", response_vbl = "Related",
                        time_vbl = "Month", xform = c("log", "sq"),
                        shift = c(1, 0)),
    "data must be provided as data frame")
  expect_error(
    get_xformed_variables(data = exp2, response_vbl = 3,
                        time_vbl = "Month", xform = c("log", "sq"),
                        shift = c(1, 0)),
    "response_vbl must be a string")
  expect_error(
    get_xformed_variables(data = exp2, response_vbl = "Mass",
                        time_vbl = "Month", xform = c("log", "sq"),
                        shift = c(1, 0)),
    "response_vbl was not found in the provided data frame")
  expect_error(
    get_xformed_variables(data = exp2, response_vbl = "Related",
                        time_vbl = 2, xform = c("log", "sq"),
                        shift = c(1, 0)),
    "time_vbl must be a string")
  expect_error(
    get_xformed_variables(data = exp2, response_vbl = "Related",
                        time_vbl = "Time", xform = c("log", "sq"),
                        shift = c(1, 0)),
    "time_vbl was not found in the provided data frame")
  expect_error(
    get_xformed_variables(data = exp2, response_vbl = "Related",
                        time_vbl = "Month", xform = "no",
                        shift = c(1, 0)),
    "specify xform appropriately")
  expect_error(
    get_xformed_variables(data = exp2, response_vbl = "Related",
                        time_vbl = "Month", xform = c("yes", "no"),
                        shift = c(1, 0)),
    "specify xform appropriately")
  expect_error(
    get_xformed_variables(data = exp2, response_vbl = "Related",
                        time_vbl = "Month", xform = c("no", "yes"),
                        shift = c(1, 0)),
    "specify xform appropriately")
  expect_error(
    get_xformed_variables(data = exp2, response_vbl = "Related",
                        time_vbl = "Month", xform = c("log", "no"),
                        shift = c(2, 0)),
    "log xform of x")
  expect_error(
    get_xformed_variables(data = exp2, response_vbl = "Related",
                        time_vbl = "Month", xform = c("sqrt", "no"),
                        shift = c(2, 0)),
    "sqrt xform of x")
  expect_error(
    get_xformed_variables(data = exp2, response_vbl = "Related",
                        time_vbl = "Month", xform = c("sq", "no"),
                        shift = c(2, 0)),
    "sq xform of x")
  expect_error(
    get_xformed_variables(data = exp2, response_vbl = "Related",
                        time_vbl = "Month", xform = c("no", "no"),
                        shift = "no"),
    "shift must be a numeric vector of length 2")
  expect_error(
    get_xformed_variables(data = exp2, response_vbl = "Related",
                        time_vbl = "Month", xform = c("no", "no"),
                        shift = 1),
    "shift must be a numeric vector of length 2")
})
