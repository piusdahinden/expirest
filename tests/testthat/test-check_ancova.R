context("ANCOVA model check")

test_that("check_ancova_succeeds", {
  l_res <- vector(mode = "list", length = 8)

  l_res[[1]] <- check_ancova(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                           response_vbl = "Potency", time_vbl = "Month",
                           batch_vbl = "Batch", alpha = 0.25)
  l_res[[2]] <- check_ancova(data = exp1[exp1$Batch %in% c("b3", "b4", "b5"), ],
                           response_vbl = "Potency", time_vbl = "Month",
                           batch_vbl = "Batch", alpha = 0.25)
  l_res[[3]] <- check_ancova(data = exp1[exp1$Batch %in% c("b4", "b5", "b8"), ],
                           response_vbl = "Potency", time_vbl = "Month",
                           batch_vbl = "Batch", alpha = 0.25)
  l_res[[4]] <- check_ancova(data = exp2, response_vbl = "Related",
                           time_vbl = "Month", batch_vbl = "Batch",
                           alpha = 0.25)
  l_res[[5]] <- check_ancova(data = exp3, response_vbl = "Moisture",
                            time_vbl = "Month", batch_vbl = "Batch",
                            alpha = 0.25)
  l_res[[6]] <- check_ancova(data = exp4, response_vbl = "Conc",
                             time_vbl = "Month", batch_vbl = "Batch",
                             alpha = 0.25)
  l_res[[7]] <- check_ancova(data = exp1[exp1$Batch == "b2", ],
                             response_vbl = "Potency", time_vbl = "Month",
                             batch_vbl = "Batch", alpha = 0.25)
  l_res[[8]] <- check_ancova(data = exp11, response_vbl = "release",
                             time_vbl = "month", batch_vbl = "batch",
                             alpha = 0.25)

  tmp1 <- vapply(l_res, function(x) x[[1]], numeric(2))
  tmp2 <- vapply(l_res, function(x) x[[2]], character(1))

  # <-><-><-><->

  expect_equal(tmp1["common.icpt", ], c(1, 0, 0, 0, 1, 0, NA, 0))
  expect_equal(tmp1["common.slp", ], c(1, 1, 0, 0, 1, 1, NA, 0))
  expect_equal(tmp2, c("cics", "dics", "dids", "dids", "cics", "dics", "n.a.",
                       "dids"))
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
