context("Determine the level of nesting of a list")

test_that("get_n_list_levels_succeeds", {
  no_list <- "This is text"
  l1 <- list(1)
  l2 <- list(l1)
  l3 <- list(l2)
  l4 <- list(l3)

  tmp <- rep(0, 5)

  # <-><-><-><->

  tmp[1] <- get_n_list_levels("text")
  tmp[2] <- get_n_list_levels(l1)
  tmp[3] <- get_n_list_levels(l2)
  tmp[4] <- get_n_list_levels(l3)
  tmp[5] <- get_n_list_levels(l4)

  # <-><-><-><->

  expect_equivalent(tmp, c(0, 1, 2, 3, 4))
})
