context("Extract worst case x value")

test_that("extract_wc_x_succeeds", {
  d_dat <- exp1[exp1$Batch %in% c("b4", "b5", "b8"), ]
  response_vbl <- "Potency"
  time_vbl <- "Month"
  batch_vbl <- "Batch"
  rl <- c(100, 99, 98)
  rl_sf <- rep(3, 3)
  sl <- 95
  sl_sf <- 2
  srch_range <- c(0, 500)
  alpha <- 0.05
  xform <- c("no", "no")
  shift <- c(0, 0)
  sf_option <- "tight"
  ivl <- "confidence"
  ivl_type <- "one.sided"
  ivl_side <- "lower"

  re <- expirest_osle(data = d_dat, response_vbl = response_vbl,
                      time_vbl = time_vbl, batch_vbl = batch_vbl, sl = sl,
                      sl_sf = 2, srch_range = srch_range, alpha = alpha,
                      alpha_pool = 0.25, xform = xform, shift = shift,
                      sf_option = sf_option, ivl = ivl, ivl_type = ivl_type,
                      ivl_side = ivl_side, rl = rl, rl_sf = rep(3, 3))

  # <-><-><-><->

  rel_lim <- get_relevant_limits(limits_list = re[["Limits"]],
                                 xform = xform, ivl_side = ivl_side)

  l_wisle <-
    get_wisle_poi_list(icpt_list = re[["Intercepts"]],
                       model_list = re[["Models"]], rl = rl, sl = sl,
                       srch_range = srch_range, alpha = alpha,  xform = xform,
                       shift = shift, ivl = ivl, ivl_type = ivl_type,
                       ivl_side = ivl_side)

  # Worst case batches
  l_wcb1 <- list(cics = rep(NA, 3),
                 dics = rep(1, 3),
                 dids.pmse = rep(1, 3),
                 dids = rep(1, 3))
  l_wcb2 <- list(cics = rep(NA, 3),
                 dics = rep(2, 3),
                 dids.pmse = rep(2, 3),
                 dids = rep(2, 3))
  l_wcb3 <- list(cics = rep(NA, 3),
                 dics = rep(3, 3),
                 dids.pmse = rep(3, 3),
                 dids = rep(3, 3))

  # <-><-><-><->

  m_poi1 <- extract_wc_x(l1 = l_wisle[["all.poi"]], l2 = l_wcb1)
  m_poi2 <- extract_wc_x(l1 = l_wisle[["all.poi"]], l2 = l_wcb2)
  m_poi3 <- extract_wc_x(l1 = l_wisle[["all.poi"]], l2 = l_wcb3)

  # <-><-><-><->

  expect_equivalent(signif(m_poi1[1, ], 12),
                    c(21.6549171662, 3.17687698779, 4.96381362600,
                      22.9097844894))
  expect_equivalent(signif(m_poi1[2, ], 12),
                    c(17.4818579854, NA, 1.16584959141, 18.4046501763))
  expect_equivalent(signif(m_poi1[3, ], 12),
                    c(13.0333154143, NA, NA, 13.7263312141))

  expect_equivalent(signif(m_poi2[1, ], 12),
                    c(21.6549171662, 18.7469763190, 13.7589542372,
                      20.0994160261))
  expect_equivalent(signif(m_poi2[2, ], 12),
                    c(17.4818579854, 14.3890859543, 11.3444065974,
                      16.1214018480))
  expect_equivalent(signif(m_poi2[3, ], 12),
                    c(13.0333154143, 9.9092777658, 8.79836497613,
                      11.9517450346))

  expect_equivalent(signif(m_poi3[1, ], 12),
                    c(21.6549171662, 20.1820835353, 12.6158455989,
                      12.8002095693))
  expect_equivalent(signif(m_poi3[2, ], 12),
                    c(17.4818579854, 15.8554616662, 10.1533440336,
                      10.3030302984))
  expect_equivalent(signif(m_poi3[3, ], 12),
                    c(13.0333154143, 11.4214113200, 7.4832233337,
                      7.61966072158))
})

test_that("extract_wc_x_fails", {
  ll1a <- list(cics = list(NA),
               dics = list(1),
               dids.pmse = list(1),
               dids = list(1))
  ll1b <- list(csci = list(NA),
               csdi = list(1),
               dsdi.pmse = list(1),
               dsdi = list(1))
  ll1c1 <- list(cics = NA,
                dics = matrix(rep(1, 3), nrow = 1),
                dids.pmse = matrix(rep(1, 3), nrow = 1),
                dids = matrix(rep(1, 3), nrow = 1))
  ll1c2 <- list(cics = "cics",
                dics = list(rep(1, 3)),
                dids.pmse = list(rep(1, 3)),
                dids = list(rep(1, 3)))
  ll1d <- list(cics = matrix(rep(1, 3), nrow = 1),
               dics = matrix(rep(1, 3), nrow = 1),
               dids.pmse = matrix(rep(1, 3), nrow = 1),
               dids = matrix(rep(1, 3), nrow = 1))

  ll2a <- list(cics = NA,
               dics = 2,
               dids.pmse = 2,
               dids = 2)
  ll2b <- list(csci = NA,
               csdi = 1,
               dsdi.pmse = 1,
               dsdi = 1)
  ll2c <- list(cics = list(NA),
               dics = 1,
               dids.pmse = 1,
               dids = 1)
  ll2d <- list(cics = c(1, 2, 3),
               dics = c(1, 2, 3),
               dids.pmse = c(1, 2, 3),
               dids = c(1, 2, 3))

  # <-><-><-><->

  expect_error(
    extract_wc_x(l1 = "ll1a", l2 = ll2a),
    "l1 must be a list")
  expect_error(
    extract_wc_x(l1 = ll1a, l2 = "ll2a"),
    "l2 must be a list")
  expect_error(
    extract_wc_x(l1 = ll1b, l2 = ll2a),
    "list l1 must have four elements named")
  expect_error(
    extract_wc_x(l1 = ll1a, l2 = ll2b),
    "list l2 must have four elements named")
  expect_error(
    extract_wc_x(l1 = ll1c1, l2 = ll2a),
    "elements of l1 must be matrices or lists of vectors")
  expect_error(
    extract_wc_x(l1 = ll1c2, l2 = ll2a),
    "elements of l1 must be matrices or lists of vectors")
  expect_error(
    extract_wc_x(l1 = ll1a, l2 = ll2c),
    "elements of l2 must be numeric vectors")
  expect_error(
    extract_wc_x(l1 = ll1d, l2 = ll2d),
    "number of rows of the matrices in l1")
})
