context("Extract information from list of all wcs_limit lists (ll_wcsl)")

test_that("extract_from_ll_wcsl_succeeds", {
  re <-
    expirest_osle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                  response_vbl = "Potency", time_vbl = "Month",
                  batch_vbl = "Batch", sl = 95, sl_sf = 2,
                  srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
                  xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
                  ivl = "confidence", ivl_type = "one.sided",
                  ivl_side = "lower")

  l_icpt <- re[["Intercepts"]]
  rl <- 98

  ll_wcsl <- lapply(seq_along(l_icpt), function(i) {
    lapply(l_icpt[[i]]$icpt, function(xx) {
      lapply(rl, function(j) {
        get_wcs_limit(rl = j, sl = 95, intercept = xx, xform = c("no", "no"),
                      shift = c(0, 0), ivl_side = "lower")
      })
    })
  })

  names(ll_wcsl) <- names(l_icpt)

  # <-><-><-><->

  l_dlim <- extract_from_ll_wcsl(ll_wcsl, "delta.lim")
  l_dlimo <- extract_from_ll_wcsl(ll_wcsl, "delta.lim.orig")
  l_wcsl <- extract_from_ll_wcsl(ll_wcsl, "wcs.lim")
  l_wcslo <- extract_from_ll_wcsl(ll_wcsl, "wcs.lim.orig")

  # <-><-><-><->

  expect_equivalent(names(l_dlim), c("cics", "dics", "dids.pmse", "dids"))
  expect_equivalent(vapply(l_dlim, is.matrix, logical(1)), rep(TRUE, 4))
  expect_equivalent(colnames(l_dlimo[["dics"]]), c("b2", "b5", "b7"))
  expect_equivalent(l_wcsl, l_wcslo)
})

test_that("extract_from_ll_wcsl_fails", {
  re <-
    expirest_osle(data = exp1[exp1$Batch %in% c("b2", "b5", "b7"), ],
                  response_vbl = "Potency", time_vbl = "Month",
                  batch_vbl = "Batch", sl = 95, sl_sf = 2,
                  srch_range = c(0, 500), alpha = 0.05, alpha_pool = 0.25,
                  xform = c("no", "no"), shift = c(0, 0), sf_option = "tight",
                  ivl = "confidence", ivl_type = "one.sided",
                  ivl_side = "lower")

  l_icpt <- re[["Intercepts"]]
  rl <- 98

  ll1 <- lapply(seq_along(l_icpt), function(i) {
    lapply(l_icpt[[i]]$icpt, function(xx) {
      lapply(rl, function(j) {
        get_wcs_limit(rl = j, sl = 95, intercept = xx, xform = c("no", "no"),
                      shift = c(0, 0), ivl_side = "lower")
      })
    })
  })

  ll2 <- ll1
  names(ll2) <- names(l_icpt)

  ll3 <- list(cics = ll2[[1]][[1]],
              dics = ll2[[2]][[1]],
              dids = ll2[[3]][[1]])

  ll4 <- ll2
  names(ll4[[1]][[1]][[1]]) <-
    c("xform", "shift", "delta.limit", "delta.limit.original",
      "wcs.limit", "wcs.limit.original")

  # <-><-><-><->

  expect_error(
    extract_from_ll_wcsl(ll = ll1, element = "wcs.lim"),
    "ll must have three elements named")
  expect_error(
    extract_from_ll_wcsl(ll = ll2, element = "worst.case.limit"),
    "specify element either as")
  expect_error(
    extract_from_ll_wcsl(ll = ll3, element = "wcs.lim"),
    "parameter ll must be a list of lists")
  expect_error(
    extract_from_ll_wcsl(ll = ll4, element = "wcs.lim"),
    "element was not found in the element names")
})
