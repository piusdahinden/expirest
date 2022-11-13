context("Extract worst case x value")

test_that("extract_wc_x_succeeds", {
  srch_range <- c(0, 500)
  alpha <- 0.05
  alpha_pool <- 0.25
  xform <- c("no", "no")
  shift <- c(0, 0)
  sf_option <- "tight"
  ivl <- "confidence"
  ivl_type <- "one.sided"
  ivl_side <- "lower"

  sl <- 95
  sl_sf <- 2
  rl <- c(100, 99, 98)
  rl_sf <- rep(3, 3)

  re <-
    expirest_osle(data = exp1[exp1$Batch %in% c("b4", "b5", "b8"), ],
                  response_vbl = "Potency", time_vbl = "Month",
                  batch_vbl = "Batch", sl = sl, sl_sf = sl_sf,
                  srch_range = srch_range, alpha = alpha,
                  alpha_pool = alpha_pool, xform = xform, shift = shift,
                  sf_option = sf_option, ivl = ivl, ivl_type = ivl_type,
                  ivl_side = ivl_side)

  l_models <- re[["Models"]]
  l_icpt <- re[["Intercepts"]]

  # Determination of worst case scenario (wcs) limits
  ll_wcsl <- lapply(seq_along(l_icpt), function(i) {
    lapply(l_icpt[[i]]$icpt, function(xx) {
      lapply(rl, function(j) {
        get_wcs_limit(rl = j, sl = 95, intercept = xx,  xform = xform,
                      shift = shift, ivl_side = ivl_side)
      })
    })
  })

  names(ll_wcsl) <- names(l_icpt)
  l_wcsl <- extract_from_ll_wcsl(ll_wcsl, "wcs.lim")

  # Calculation of POI values for all wcs limits
  # Determination of worst case POI values
  l_poi <- vector(mode = "list", length = length(l_icpt))
  names(l_poi) <- c("cics", "dics", "dids")

  l_prl <- vector(mode = "list", length = length(l_icpt))
  names(l_prl) <- c("cics", "dics", "dids")

  for (variety in names(l_icpt)) {
    # Initialise empty arrays
    m_poi <-
      matrix(NA, nrow = length(rl), ncol = length(l_icpt[[variety]][["icpt"]]))
    colnames(m_poi) <- names(l_icpt[[variety]][["icpt"]])

    a_prl <- array(NA, dim = c(length(rl), length(l_icpt[[variety]][["icpt"]]),
                               length(l_icpt[[variety]][["icpt"]])),
                   dimnames = list(as.character(seq_along(rl)),
                                   names(l_icpt[[variety]][["icpt"]]),
                                   names(l_icpt[[variety]][["icpt"]])))

    # Fill arrays
    for (j in seq_along(rl)) {
      for (k in seq_len(ncol(l_wcsl[[variety]]))) {
        if (variety != "dids") {
          tmp_poi <- try_get_model(
            find_poi(srch_range = srch_range,
                     model = l_models[[variety]],
                     sl = l_wcsl[[variety]][j, k], alpha = alpha,
                     ivl_type = ivl_type, ivl_side = ivl_side, ivl = ivl))
        } else {
          tmp_poi <- try_get_model(
            find_poi(srch_range = srch_range,
                     model = l_models[["individual"]][[k]],
                     sl = l_wcsl[[variety]][j, k], alpha = alpha,
                     ivl_type = ivl_type, ivl_side = ivl_side, ivl = ivl))
        }

        if (is.null(tmp_poi[["Error"]])) {
          m_poi[j, k] <- tmp_poi[["Model"]]

          if (variety != "dids") {
            tmp_prl <- try_get_model(
              get_intvl_limit(
                x_new = tmp_poi[["Model"]],
                model = l_models[[variety]], alpha = alpha,
                ivl_type = ivl_type, ivl_side = ivl_side, ivl = ivl)
            )

            if (is.null(tmp_prl[["Error"]])) {
              a_prl[j, k, ] <- tmp_prl[["Model"]]
            }
          } else {
            t_prl <- rep(NA, ncol(l_wcsl[[variety]]))

            for (kk in seq_len(ncol(l_wcsl[["dids"]]))) {
              tmp_prl <- try_get_model(
                get_intvl_limit(
                  x_new = tmp_poi[["Model"]],
                  model = l_models[["individual"]][[kk]], alpha = alpha,
                  ivl_type = ivl_type, ivl_side = ivl_side, ivl = ivl)
              )

              if (is.null(tmp_prl[["Error"]])) {
                t_prl[kk] <- tmp_prl[["Model"]]
              }
            }

            a_prl[j, k, ] <- t_prl
          }
        }
      }
    }

    # Put the resulting arrays into the corresponding list entries
    l_poi[[variety]] <- m_poi
    l_prl[[variety]] <- a_prl
  }

  # Worst case batches
  l_wcb1 <- list(cics = rep(NA, 3),
                 dics = rep(1, 3),
                 dids = rep(1, 3))
  l_wcb2 <- list(cics = rep(NA, 3),
                 dics = rep(2, 3),
                 dids = rep(2, 3))
  l_wcb3 <- list(cics = rep(NA, 3),
                 dics = rep(3, 3),
                 dids = rep(3, 3))

  # <-><-><-><->

  m_poi1 <- extract_wc_x(l1 = l_poi, l2 = l_wcb1)
  m_poi2 <- extract_wc_x(l1 = l_poi, l2 = l_wcb2)
  m_poi3 <- extract_wc_x(l1 = l_poi, l2 = l_wcb3)

  # <-><-><-><->

  expect_equivalent(signif(m_poi1[1, ], 12),
                    c(21.6549171662, 3.17687698779, 22.9097844894))
  expect_equivalent(signif(m_poi1[2, ], 12),
                    c(17.4818579854, NA, 18.4046501763))
  expect_equivalent(signif(m_poi1[3, ], 12),
                    c(13.0333154143, NA, 13.7263312141))

  expect_equivalent(signif(m_poi2[1, ], 12),
                    c(21.6549171662, 18.7469763190, 20.0994160261))
  expect_equivalent(signif(m_poi2[2, ], 12),
                    c(17.4818579854, 14.3890859543, 16.1214018480))
  expect_equivalent(signif(m_poi2[3, ], 12),
                    c(13.0333154143, 9.9092777658, 11.9517450346))

  expect_equivalent(signif(m_poi3[1, ], 12),
                    c(21.6549171662, 20.1820835353, 12.8002095693))
  expect_equivalent(signif(m_poi3[2, ], 12),
                    c(17.4818579854, 15.8554616662, 10.3030302984))
  expect_equivalent(signif(m_poi3[3, ], 12),
                    c(13.0333154143, 11.4214113200, 7.61966072158))
})

test_that("extract_wc_x_fails", {
  ll1a <- list(cics = list(NA),
                 dics = list(1),
                 dids = list(1))
  ll1b <- list(csci = list(NA),
               csdi = list(1),
               dsdi = list(1))
  ll1c1 <- list(cics = NA,
               dics = matrix(rep(1, 3), nrow = 1),
               dids = matrix(rep(1, 3), nrow = 1))
  ll1c2 <- list(cics = "cics",
                dics = list(rep(1, 3)),
                dids = list(rep(1, 3)))
  ll1d <- list(cics = matrix(rep(1, 3), nrow = 1),
               dics = matrix(rep(1, 3), nrow = 1),
               dids = matrix(rep(1, 3), nrow = 1))

  ll2a <- list(cics = NA,
                 dics = 2,
                 dids = 2)
  ll2b <- list(csci = NA,
               csdi = 1,
               dsdi = 1)
  ll2c <- list(cics = list(NA),
               dics = 1,
               dids = 1)
  ll2d <- list(cics = c(1, 2, 3),
               dics = c(1, 2, 3),
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
    "list l1 must have three elements named")
  expect_error(
    extract_wc_x(l1 = ll1a, l2 = ll2b),
    "list l2 must have three elements named")
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
