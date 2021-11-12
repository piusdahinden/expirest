context("ANCOVA model check")

test_that("check_ancova_succeeds", {
  tmp <- rep(NA, 12)

  tmp[1:2] <- check_ancova(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                           response_vbl = "Potency", time_vbl = "Month",
                           batch_vbl = "Batch", alpha = 0.25)
  tmp[3:4] <- check_ancova(data = exp1[exp1$Batch %in% c("b3", "b4", "b5"), ],
                           response_vbl = "Potency", time_vbl = "Month",
                           batch_vbl = "Batch", alpha = 0.25)
  tmp[5:6] <- check_ancova(data = exp1[exp1$Batch %in% c("b4", "b5", "b8"), ],
                           response_vbl = "Potency", time_vbl = "Month",
                           batch_vbl = "Batch", alpha = 0.25)
  tmp[7:8] <- check_ancova(data = exp2, response_vbl = "Related",
                           time_vbl = "Month", batch_vbl = "Batch",
                           alpha = 0.25)
  tmp[9:10] <- check_ancova(data = exp3, response_vbl = "Moisture",
                           time_vbl = "Month", batch_vbl = "Batch",
                           alpha = 0.25)
  tmp[11:12] <- check_ancova(data = exp4, response_vbl = "Conc",
                           time_vbl = "Month", batch_vbl = "Batch",
                           alpha = 0.25)

  # <-><-><-><->

  expect_equal(tmp, c(1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1))
})

test_that("check_ancova_fails", {
  tmp <- exp1
  tmp$Batch <- as.character(tmp$Batch)

  # <-><-><-><->

  expect_error(
    check_ancova(data =
                   as.matrix(exp1[exp1$Batch %in% c("b2", "b5", "b7"), 2:3]),
                 response_vbl = "Potency", time_vbl = "Month",
                 batch_vbl = "Batch", alpha = 0.25),
    "data must be provided as data frame")
  expect_error(
    check_ancova(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                 response_vbl = 4, time_vbl = "Month",
                 batch_vbl = "Batch", alpha = 0.25),
    "response_vbl must be a string")
  expect_error(
    check_ancova(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                 response_vbl = "Mass", time_vbl = "Month",
                 batch_vbl = "Batch", alpha = 0.25),
    "response_vbl was not found in the provided data frame")
  expect_error(
    check_ancova(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                 response_vbl = "Potency", time_vbl = 3,
                 batch_vbl = "Batch", alpha = 0.25),
    "time_vbl must be a string")
  expect_error(
    check_ancova(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                 response_vbl = "Potency", time_vbl = "Time",
                 batch_vbl = "Batch", alpha = 0.25),
    "time_vbl was not found in the provided data frame")
  expect_error(
    check_ancova(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                 response_vbl = "Potency", time_vbl = "Month",
                 batch_vbl = 2, alpha = 0.25),
    "batch_vbl must be a string")
  expect_error(
    check_ancova(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                 response_vbl = "Potency", time_vbl = "Month",
                 batch_vbl = "Lot", alpha = 0.25),
    "batch_vbl was not found in the provided data frame")
  expect_error(
    check_ancova(data = tmp[tmp$Batch %in% c("b2", "b5", "b7"), ],
                 response_vbl = "Potency", time_vbl = "Month",
                 batch_vbl = "Batch", alpha = 0.25),
    "column in data specified by batch_vbl")
  expect_error(
    check_ancova(data = tmp[tmp$Batch %in% c("b2", "b5", "b7"), ],
                 response_vbl = "Potency", time_vbl = "Month",
                 batch_vbl = "Batch", alpha = 0.25),
    "column in data specified by batch_vbl")
  expect_error(
    check_ancova(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                 response_vbl = "Potency", time_vbl = "Month",
                 batch_vbl = "Batch", alpha = 5),
    "specify alpha")
  expect_error(
    check_ancova(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                 response_vbl = "Potency", time_vbl = "Month",
                 batch_vbl = "Batch", alpha = -1),
    "specify alpha")
})
