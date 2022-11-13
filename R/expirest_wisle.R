#' What-if (approach for) shelf life estimation (wisle)
#'
#' Based on a linear regression model fitted to a stability data set the
#' function \code{expirest_wisle()} estimates the shelf life, or retest period,
#' for the specified release and specification limit following the ARGPM
#' guidance \dQuote{Stability testing for prescription medicines}. The
#' abbreviation \dQuote{wisle} stands for \dQuote{what-if shelf life estimation}
#' (because it estimates the shelf life (\dQuote{what}) for a given release
#' limit (\dQuote{if})).
#'
#' @param rl A numeric value or a numeric vector specifying the release
#'   specification limit(s) for which the corresponding expiry should be
#'   estimated.
#' @param rl_sf A positive integer or a vector of positive integers specifying
#'   the number of \dQuote{significant figures} (sf) of \code{rl}. It must have
#'   the same length as \code{rl}.
#' @inheritParams expirest_osle
#'
#' @details For the shelf life estimation for submissions in Australia the
#' Australian Regulatory Guidelines for Prescription Medicines (ARGPM), i.e.
#' the guidance \dQuote{Stability testing for prescription medicines}, is
#' binding. In chapter 14.3.1, \dQuote{Predicting shelf life from stability
#' data}, it is described how the estimation should be done. It recommends
#' to take the worst case situation at batch release into account. The
#' following examples are listed:
#' \describe{
#'  \item{1a)}{For medicine that has a lower Assay release limit of 95 per cent
#'    and a lower assay expiry limit of 90 per cent, the maximum decrease in
#'    assay allowed over the shelf-life is 5 per cent.}
#'  \item{2a)}{Similarly, for a medicine that has a release limit for an
#'    individual impurity of 0.2 per cent and an expiry limit of 0.5 per cent,
#'    the maximum increase in the impurity allowed over the shelf life is
#'    0.3 per cent.}
#'  \item{1b)}{For the same medicine, if the actual release assay result is
#'    101 per cent, then the shelf life should be determined at the time the
#'    medicine (or confidence interval of the regression line) decreases by
#'    5 per cent and reaches 96 per cent, rather than the expiry limit
#'    (90 per cent). This takes into account the possibility of batches being
#'    released at the lower release limit (i.e. 95 per cent) and ensures they
#'    will comply with the expiry limit throughout the shelf life.}
#'  \item{2b)}{Similarly, if the actual impurity content is 0.1 per cent at
#'    batch release, the shelf life should be determined as the time the
#'    95 per cent confidence interval reaches 0.4 per cent (i.e. increases by
#'    0.3 per cent).}
#' }
#'
#' Consequently, it is necessary to define
#' \itemize{
#'  \item a release limit and
#'  \item an expiry limit or shelf life limit (usually the specification limit).
#' }
#' If both of these limits were the same the shelf life would be zero, i.e. no
#' change allowed. \cr
#'
#' For the estimation of the shelf life or expiry limit it is necessary to find
#' the point where the upper or lower 95\% confidence interval limit of the
#' linear model fitted to the data (by aid of \code{lm}) intersects the
#' \dQuote{worst case scenario limit} (WCSL), which is defined by the ARGPM
#' guidance \dQuote{Stability testing for prescription medicines} as the
#' intercept \eqn{\pm} difference between the specification limit and the
#' release limit, where this differences is added (\eqn{+}) if the upper limit
#' is relevant or it is subtracted (\eqn{-}) if the lower limit is relevant.
#' In this package, this point is called the \dQuote{point of intersection} or
#' \dQuote{point of interest}, abbreviated POI.
#'
#' Before performing the retest period or shelf life estimation the most
#' suitable model should be determined. It should particularly be verified
#' if data of all test batches are poolable or not. Details on this are
#' described in section \dQuote{Checking batch poolability} below.
#'
#' @inheritSection expirest_osle Checking batch poolability
#'
#' @return An object of class \sQuote{\code{expirest_wisle}} is returned,
#' containing the following elements:
#' \item{Data}{Data frame of the original data including new columns with
#'   transformed variables, if applicable.}
#' \item{Parameters}{A list of the parameters with the elements \code{alpha},
#'   \code{alpha.pool}, \code{ivl}, \code{ivl.type} and \code{ivl.side}.}
#' \item{Variables}{A list of the variable names, i.e. the original names of
#'   \code{batch_vbl}, \code{time_vbl} and \code{response_vbl} and, if
#'   applicable, of the transformed variables.}
#' \item{Model.Type}{A list of five elements specifying which model, based on
#'   the ANCOVA analysis, suits best. The first element (\code{type.spec})
#'   is a numeric vector of length 2 specifying the best model accepted at the
#'   significance level of 0.25. The first number represents the decision on
#'   the intercept and the second on the slope, where \code{1} stands for
#'   \dQuote{common} and \code{2} stands for \dQuote{different}.}
#' \item{Models}{A list of four elements named \code{cics}, \code{dics},
#'   \code{dids} and \code{individual}. The first three elements contain the
#'   \sQuote{\code{lm}} objects of the \dQuote{common intercept / common slope}
#'   (\code{cics}), \dQuote{different intercept / common slope} (\code{dics})
#'   and \dQuote{different intercept / different slope} (\code{dids}) models.
#'   The fourth element is a list of the \sQuote{\code{lm}} objects of the
#'   models obtained from fitting the data of each batch individually.}
#' \item{AIC}{A numeric named vector of the Akaike Information Criterion (AIC)
#'   values of each of the three fitted models.}
#' \item{BIC}{A numeric named vector of the Bayesian Information Criterion (BIC)
#'   values of each of the three fitted models.}
#' \item{wc.icpt}{A data frame of the worst case intercepts of each of the
#'   three fitted models.}
#' \item{wc.batch}{A list of numeric value(s) of the worst case batch(es) per
#'   model type. In case of the \code{dids} model type, the estimation is done
#'   using the models obtained from fitting the data of each batch
#'   individually.}
#' \item{Limits}{A list of all limits.}
#' \item{POI}{A data frame of the intercepts, the differences between release
#'   and shelf life limits, the WCSLs, the expiry and release specification
#'   limits, the shelf lives and POI values. In case of the \code{dids} model
#'   type, the estimation of the POI values is done using the models obtained
#'   from fitting the data of each batch individually.}
#'
#' The \code{POI} data frame has the following columns:
#' \item{Intercept.cics}{The intercept of the worst case batch for the cics
#'   model.}
#' \item{Intercept.dics}{The intercept of the worst case batch for the dics
#'   model.}
#' \item{Intercept.dids}{The intercept of the worst case batch for the dids
#'   model.}
#' \item{Delta.cics}{Absolute difference between the release and and the shelf
#'   life specification for the cics model.}
#' \item{Delta.dics}{Absolute difference between the release and and the shelf
#'   life specification for the dics model.}
#' \item{Delta.dids}{Absolute difference between the release and and the shelf
#'   life specification for the dids model.}
#' \item{WCSL.cics}{WCSL for the cics model.}
#' \item{WCSL.dics}{WCSL for the dics model.}
#' \item{WCSL.dids}{WCSL for the dids model.}
#' \item{Exp.Spec}{The (expiry) specification, i.e. the specification which is
#'   relevant for the determination of the expiry.}
#' \item{Rel.Spec}{The calculated release specification.}
#' \item{Shelf.Life.cics}{The estiamted shelf life for the cics model.}
#' \item{Shelf.Life.dics}{The estiamted shelf life for the dics model.}
#' \item{Shelf.Life.dids}{The estiamted shelf life for the dids model.}
#' \item{POI.Model.cics}{The POI  of the cics model.}
#' \item{POI.Model.dics}{The POI of the dics model.}
#' \item{POI.Model.dids}{The POI of the dids model.}
#'
#' @references
#' Therapeutic Goods Administration (TGA) of the Department of Health of the
#' Australian Government, Australian Regulatory Guidelines for Prescription
#' Medicines (ARGPM), Stability testing for prescription medicines,
#' Version 1.1, March 2017\cr
#' \url{https://www.tga.gov.au/stability-testing-prescription-medicines}
#'
#' International Council for Harmonisation of Technical Requirements for
#' Registration of Pharmaceuticals for Human (ICH), Harmonised Tripartite
#' Guideline, Evaluation of Stability Data Q1E, step 4, February 2003
#' (CPMP/ICH/420/02).\cr
#' \url{https://www.ich.org/page/quality-guidelines}
#'
#' @seealso \code{\link{expirest_osle}}, \code{\link{find_poi}},
#' \code{\link[stats]{uniroot}}, \code{\link[stats]{lm}},
#' \code{\link[stats]{AIC}}, \code{\link[stats]{BIC}}.
#'
#' @example man/examples/examples_expirest_wisle.R
#'
#' @importFrom stats lm
#' @importFrom stats as.formula
#' @importFrom stats coef
#'
#' @export

expirest_wisle <- function(data, response_vbl, time_vbl, batch_vbl, rl, rl_sf,
                       sl, sl_sf, srch_range, alpha = 0.05, alpha_pool = 0.25,
                       xform = c("no", "no"), shift = c(0, 0),
                       sf_option = "loose", ivl = "confidence",
                       ivl_type = "one.sided", ivl_side = "lower", ...) {
  if (!is.data.frame(data)) {
    stop("The data must be provided as data frame.")
  }
  if (!is.character(response_vbl)) {
    stop("The parameter response_vbl must be a string.")
  }
  if (!(response_vbl %in% colnames(data))) {
    stop("The response_vbl was not found in the provided data frame.")
  }
  if (!is.character(time_vbl)) {
    stop("The parameter time_vbl must be a string.")
  }
  if (!(time_vbl %in% colnames(data))) {
    stop("The time_vbl was not found in the provided data frame.")
  }
  if (!is.character(batch_vbl)) {
    stop("The parameter batch_vbl must be a string.")
  }
  if (!(batch_vbl %in% colnames(data))) {
    stop("The batch_vbl was not found in the provided data frame.")
  }
  if (!is.factor(data[, batch_vbl])) {
    stop("The column in data specified by batch_vbl must be a factor.")
  }
  if (!is.numeric(rl)) {
    stop("The parameter rl must be a numeric.")
  }
  if (!is.numeric(rl_sf) & all(!is.na(rl_sf))) {
    stop("The parameter rl_sf must be a positive integer of length rl, or NA.")
  }
  if (sum(rl_sf < 0) > 0) {
    stop("The parameter rl_sf must be a positive integer of length rl, or NA.")
  }
  if (length(rl_sf) != length(rl)) {
    stop("The parameter rl_sf must be a positive integer of length rl, or NA.")
  }
  if (!isTRUE(all.equal(rl_sf, as.integer(rl_sf)))) {
    stop("The parameter rl_sf must be a positive integer of length rl, or NA.")
  }
  if (!is.numeric(sl) | length(sl) > 2) {
    stop("The parameter sl must be a numeric or vector of length 1 or 2.")
  }
  if (length(sl) == 2) {
    if (sl[2] < sl[1]) {
      stop("The parameter sl must be of the form c(lower, upper).")
    }
  }
  if (!is.numeric(sl_sf) & all(!is.na(sl_sf))) {
    stop("The parameter sl_sf must be a positive integer of length sl.")
  }
  if (sum(sl_sf < 0) > 0) {
    stop("The parameter sl_sf must be a positive integer of length sl.")
  }
  if (length(sl_sf) != length(sl)) {
    stop("The parameter sl_sf must be a positive integer of length sl.")
  }
  if (!isTRUE(all.equal(sl_sf, as.integer(sl_sf)))) {
    stop("The parameter sl_sf must be a positive integer of length sl.")
  }
  if (!is.numeric(srch_range) | length(srch_range) != 2) {
    stop("The parameter srch_range must be a vector of length 2.")
  }
  if (alpha <= 0 | alpha > 1) {
    stop("Please specify alpha as (0, 1].")
  }
  if (alpha_pool <= 0 | alpha_pool > 1) {
    stop("Please specify alpha_pool as (0, 1].")
  }
  if (length(xform) != 2) {
    stop("Please specify xform appropriately.")
  }
  if (!(xform[1] %in% c("no", "log", "sqrt", "sq")) |
      !(xform[2] %in% c("no", "log", "sqrt", "sq"))) {
    stop("Please specify xform appropriately.")
  }
  if (length(shift) != 2) {
    stop("The parameter shift must be a numeric vector of length 2.")
  }
  if (!is.numeric(shift)) {
    stop("The parameter shift must be a numeric vector of length 2.")
  }
  if (!(sf_option %in% c("tight", "loose"))) {
    stop("Please specify sf_option either as \"tight\" or \"loose\".")
  }
  if (!(ivl %in% c("confidence", "prediction"))) {
    stop("Please specify ivl either as \"confidence\" or \"prediction\".")
  }
  if (!(ivl_type %in% c("one.sided", "two.sided"))) {
    stop("Please specify ivl_type either as \"one.sided\" or \"two.sided\".")
  }
  if (!(ivl_side %in% c("lower", "upper"))) {
    stop("Please specify ivl_side either as \"lower\" or \"upper\".")
  }

  if (length(sl) == 1) {
    if (ivl_side == "lower" & !all(rl > sl)) {
      stop("If ivl_side is \"lower\" rl must be > sl.")
    }
    if (ivl_side == "upper" & !all(rl < sl)) {
      stop("If ivl_side is \"upper\" rl must be < sl.")
    }
  } else {
    if (ivl_side == "lower" & !all(rl > sl[1])) {
      stop("If ivl_side is \"lower\" rl must be > sl.")
    }
    if (ivl_side == "upper" & !all(rl < sl[2])) {
      stop("If ivl_side is \"upper\" rl must be < sl.")
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Call function expirest_osle() to perform the ICH relevant calculations

  r_ret <- expirest_osle(data = data, response_vbl = response_vbl,
                         time_vbl = time_vbl, batch_vbl = batch_vbl,
                         sl = sl, sl_sf = sl_sf, srch_range = srch_range,
                         alpha = alpha, alpha_pool = alpha_pool, xform = xform,
                         shift = shift, sf_option = sf_option, ivl = ivl,
                         ivl_type = ivl_type, ivl_side = ivl_side,
                         rl = rl, rl_sf = rl_sf)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Linearisation of data by variable transformation
  # Transformations:
  #   log: natural logarithm of the variable
  #   sqrt: square root of the variable variable
  #   sq: square of the variable
  #
  # Note: The log and sqrt transformations include adding the value defined by
  #       the shift parameter before performing the transformation.

  if (sum(xform %in% "no") == 0) {
    time_vbl <- r_ret[["Variables"]][["time"]]
    response_vbl <- r_ret[["Variables"]][["response"]]
  }
  if (sum(xform %in% "no") == 1) {
    if (xform[1] != "no") {
      old_time_vbl <- r_ret[["Variables"]][["time.orig"]]
      time_vbl <- r_ret[["Variables"]][["time"]]
    }
    if (xform[2] != "no") {
      old_response_vbl <- r_ret[["Variables"]][["response.orig"]]
      response_vbl <- r_ret[["Variables"]][["response"]]
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Fit of all possible models that are relevant and their intercepts

  l_models <- r_ret[["Models"]]
  l_icpt <- r_ret[["Intercepts"]]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determination of limits

  l_lim <- r_ret$Limits

  # For the assessments to follow only the relevant limits are used.
  # ---------
  # Specification limits
  if (length(sl) == 2) {
    switch(ivl_side,
           "lower" = {
             sl_orig <- l_lim[["sl.orig"]][1]

             if (xform[2] == "no") {
               sl <- l_lim[["sl"]][1]
             } else {
               sl <- l_lim[["sl.trfmd"]][1]
               sl_bt <- l_lim[["sl"]][1]
             }
           },
           "upper" = {
             sl_orig <- l_lim[["sl.orig"]][2]

             if (xform[2] == "no") {
               sl <- l_lim[["sl"]][2]
             } else {
               sl <- l_lim[["sl.trfmd"]][2]
               sl_bt <- l_lim[["sl"]][2]
             }
           })
  } else {
    sl_orig <- l_lim[["sl.orig"]]

    if (xform[2] == "no") {
      sl <- l_lim[["sl"]]
    } else {
      sl <- l_lim[["sl.trfmd"]]
      sl_bt <- l_lim[["sl"]]
    }
  }

  # ---------
  # Release limits
  rl_orig <- l_lim[["rl.orig"]]

  if (xform[2] == "no") {
    rl <- l_lim[["rl"]]
  } else {
    rl <- l_lim[["rl.trfmd"]]
    rl_bt <- l_lim[["rl"]]
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculation of POI values for all models (according to ARGPM)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determination of worst case scenario (wcs) limits for all intercepts of
  # all models (on the transformed scale, if data have been transformed)

  # List of all wcs_limit lists
  ll_wcsl <- lapply(seq_along(l_icpt), function(i) {
    lapply(l_icpt[[i]]$icpt, function(xx) {
      lapply(rl, function(j) {
        get_wcs_limit(rl = j, sl = sl, intercept = xx,
                      xform = xform, shift = shift,
                      ivl_side = ivl_side)
      })
    })
  })
  names(ll_wcsl) <- names(l_icpt)

  l_wcsl <- extract_from_ll_wcsl(ll_wcsl, "wcs.lim")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculation of POI values for all wcs limits
  # Determination of worst case POI values

  # The worst case POI value is the POI value obtained with the batch whose
  # lower or upper confidence or prediction interval limit is closest to the
  # corresponding specification limit, i.e. the worst case batch.

  # Example: the response is the assay, and the lower specification limit is
  # the relevant limit. A batch may have a shorter POI than the other batches,
  # but because it has a higher intercept or/and smaller variability than one
  # or more of the other batches, the lower confidence or prediction interval
  # limit of one of the other batches still may be closer to the lower
  # specification limit so that their POI values are the POI values of
  # relevance.

  # Note: in case of the "dids" model the POI is not determined using the model
  # that was determined using the data from all batches (i.e. the full model
  # with the batch_vbl * time_vbl interaction term). Instead, separate models
  # are fitted to the data of each individual batch and the POI values are
  # determined for each of these models. Of these POI values, the smallest is
  # returned.

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

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determination of the batches with the confidence or prediction interval
  # limits that are closest to the respective specification limit for each
  # model and each POI

  switch(ivl_side,
         "lower" = {
           l_min_dist <- lapply(l_prl, FUN = function(x) {
             apply(x, c(1, 2), FUN = function(y) {
               ifelse(length(which.min(y)) != 0, which.min(abs(y)), NA)
             })
           })
         },
         "upper" = {
           l_min_dist <- lapply(l_prl, FUN = function(x) {
             apply(x, c(1, 2), FUN = function(y) {
               ifelse(length(which.max(y)) != 0, which.max(abs(y)), NA)
             })
           })
         })

  # Determination of the smallest POI value for each model and each rl value
  l_min_poi <- lapply(l_poi, FUN = function(x) {
    apply(x, 1, function(y) {
      ifelse(length(which.min(y)) != 0, which.min(y), NA)
    })
  })

  # Determination of the worst case batches for each model and each rl value:
  #   The worst case batches are the ones with the confidence or prediction
  #   interval limits that are closest to the respective specification limit
  #   where the POI values are smallest.
  # In case of cics model: wc_icpt_ich is the common intercept of all batches
  #   and none of the batches is the worst case batch.

  l_wc_batch <- vector(mode = "list", length = length(l_min_poi))
  names(l_wc_batch) <- names(l_min_poi)

  for (i in seq_along(l_min_dist)) {
    if (names(l_min_dist)[i] == "cics") {
      l_wc_batch[[i]] <- rep(NA, length(rl))
    } else {
      l_wc_batch[[i]] <-
        vapply(seq_along(rl), function(j) {
          ifelse(!is.na(l_min_poi[[i]][j]),
                 l_min_dist[[i]][j, l_min_poi[[i]][j]],
                 NA)
        },
        numeric(1))
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Matrix of the worst case POI values for each model and each rl value

  m_poi <- extract_wc_x(l1 = l_poi, l2 = l_wc_batch)

  # Depending on the transformation of the time variable the POI values have to
  # be back-transformed.

  if (xform[1] != "no") {
    switch(xform[1],
           "log" = {
             m_poi <- exp(m_poi) - shift[1]
           },
           "sqrt" = {
             m_poi <- m_poi^2 - shift[1]
           },
           "sq" = {
             m_poi <- sqrt(m_poi) - shift[1]
           })
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Collection of data and compilation of summary data frame

  # ---------
  # Worst case intercepts (wc_icpt_argpm) (on the original scale)

  if (xform[2] == "no") {
    wc_icpt_argpm <- extract_wc_x(l1 = l_icpt, l2 = l_wc_batch)
  } else {
    l_icpt_sub <- lapply(l_icpt, function(x) list(x$icpt.orig))
    wc_icpt_argpm <- extract_wc_x(l1 = l_icpt_sub, l2 = l_wc_batch)
  }

  # ---------
  # Delta and WCSL

  if (xform[2] == "no") {
    l_delta <- extract_from_ll_wcsl(ll_wcsl, "delta.lim")
    l_wcsl <- extract_from_ll_wcsl(ll_wcsl, "wcs.lim")

    m_delta <- extract_wc_x(l1 = l_delta, l2 = l_wc_batch)
    m_wcsl <- extract_wc_x(l1 = l_wcsl, l2 = l_wc_batch)
  } else {
    l_delta_orig <- extract_from_ll_wcsl(ll_wcsl, "delta.lim.orig")
    l_wcsl_orig <- extract_from_ll_wcsl(ll_wcsl, "wcs.lim.orig")

    m_delta <- extract_wc_x(l1 = l_delta_orig, l2 = l_wc_batch)
    m_wcsl <- extract_wc_x(l1 = l_wcsl_orig, l2 = l_wc_batch)
  }

  # ---------
  # Summary data frame compilation

  d_poi <- data.frame(
    Intercept.cics = wc_icpt_argpm[, "cics"],
    Intercept.dics = wc_icpt_argpm[, "dics"],
    Intercept.dids = wc_icpt_argpm[, "dids"],
    Delta.cics = m_delta[, "cics"],
    Delta.dics = m_delta[, "dics"],
    Delta.dids = m_delta[, "dids"],
    WCSL.cics = m_wcsl[, "cics"],
    WCSL.dics = m_wcsl[, "dics"],
    WCSL.dids = m_wcsl[, "dids"],
    Exp.Spec.Report = rep(sl_orig, nrow(m_poi)),
    Exp.Spec = rep(sl, nrow(m_poi)),
    Rel.Spec.Report = rl_orig,
    Rel.Spec = rl,
    Shelf.Life.cics = m_poi[, "cics"],
    Shelf.Life.dics = m_poi[, "dics"],
    Shelf.Life.dids = m_poi[, "dids"],
    POI.Model.cics = rep(r_ret[["POI"]]["cics"], nrow(m_poi)),
    POI.Model.dics = rep(r_ret[["POI"]]["dics"], nrow(m_poi)),
    POI.Model.dids = rep(r_ret[["POI"]]["dids"], nrow(m_poi)))

  if (xform[2] != "no") {
    d_poi[, "Exp.Spec"] <- rep(sl_bt, nrow(m_poi))
    d_poi[, "Rel.Spec"] <- rl_bt
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Putting results into a list

  structure(list(Data = r_ret[["Data"]],
                 Parameters = r_ret[["Parameters"]],
                 Variables = r_ret[["Variables"]],
                 Model.Type = r_ret[["Model.Type"]],
                 Models = r_ret[["Models"]],
                 AIC = r_ret[["AIC"]],
                 BIC = r_ret[["BIC"]],
                 wc.icpt = wc_icpt_argpm,
                 wc.batch = l_wc_batch,
                 Limits = l_lim,
                 POI = d_poi),
            class = "expirest_wisle")
}

#' Illustrating the what-if (approach for) shelf life estimate (wisle)
#'
#' The function \code{plot_expirest_wisle()} makes a graphical display of the
#' shelf life estimate done by the \code{\link{expirest_wisle}()} function.
#'
#' @param model An \sQuote{\code{expirest_wisle}} object, i.e. a list returned
#'   by the \code{\link{expirest_wisle}()} function.
#' @param rl_index A positive integer specifying which of the release limit
#'   values that have been handed over to \code{\link{expirest_wisle}()} should
#'   be displayed. The default value is \code{1}.
#' @param scenario A character string specifying if the plot should be extended
#'   (with respect to the \eqn{x} axis) up to the \dQuote{standard scenario}
#'   (\code{"standard"}) or up to the \dQuote{worst case scenario}
#'   (\code{"worst"}). The default is \code{"standard"}.
#' @param plot_option A character string of either \code{"full"},
#'   \code{"lean1"}, \code{"lean2"}, \code{"basic1"} and \code{"basic2"},
#'   specifying if all the information should be shown in the plot (option
#'   \code{"full"}) or only basic information (options \code{"lean"} and
#'   \code{"basic"}). Full means the data points, the fitted regression line
#'   with the confidence or prediction interval, the specification limit(s)
#'   and the estimated shelf life. The default is \code{"full"}.
#' @inheritParams plot_expirest_osle
#'
#' @details The function \code{plot_expirest_wisle()} uses the data and the
#' information about the linear model that was used for the estimation of
#' the shelf life by aid of the \code{\link{expirest_wisle}()} function. It
#' plots a graph of the time course of a parameter, a linear regression line
#' fitted to the data and the associated confidence or prediction interval.
#' In addition, it shows features of the worst case scenario shelf life
#' estimation.
#'
#' For plotting, the \code{\link[ggplot2]{ggplot}()} function from the
#' \sQuote{\code{ggplot2}} package is used. The various arguments can be
#' used to control the appearance of the plot. The \sQuote{\code{ggplot2}}
#' object of the generated plot is contained in the \code{Graph} element of
#' the list that is returned by \code{\link{plot_expirest_wisle}()} and can be
#' used to modify the appearance of the graph.
#'
#' @return An object of class \sQuote{\code{plot_expirest_wisle}} is returned
#' invisibly consisting of the following elements:
#' \item{Model}{The \sQuote{\code{expirest_wisle}} object that was passed via
#'   the \code{model} argument.}
#' \item{Expiery}{A data frame of type \code{expiry}.}
#' \item{Graph}{A \sQuote{\code{ggplot2}} object for the graphical display.}
#' \item{Prediction}{A data frame of the predicted values.}
#' \item{text}{A data frame of the text elements on the plot.}
#' \item{hlines}{A data frame of the horizontal line elements on the plot.}
#' \item{vlines}{A data frame of the vertical line elements on the plot.}
#' \item{segments}{A data frame of segment line elements on the plot.}
#' \item{arrow}{A data frame of arrow elements on the plot.}
#'
#' @seealso \code{\link{expirest_wisle}}, \code{\link{expirest_osle}}.
#'
#' @example man/examples/examples_plot_expirest_wisle.R
#'
#' @importFrom stats lm
#' @importFrom stats as.formula
#' @importFrom stats coef
#' @importFrom stats predict
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 geom_curve
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 arrow
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 unit
#'
#' @export

plot_expirest_wisle <- function(
  model, rl_index = 1, show_grouping = "yes", response_vbl_unit = NULL,
  y_range, x_range = NULL, scenario = "standard", plot_option = "full",
  ci_app = "line") {
  if (!inherits(model, "expirest_wisle")) {
    stop("The model must be an object of class expirest_wisle.")
  }
  if (!is.numeric(rl_index) | length(rl_index) > 1) {
    stop("The parameter rl_index must be a positive integer of length 1.")
  }
  if (rl_index != as.integer(rl_index)) {
    stop("The parameter rl_index must be a positive integer of length 1.")
  }
  if (rl_index < 1 | rl_index > nrow(model[["POI"]])) {
    stop("The parameter rl_index must be between 1 and the number of rl ",
         "values.")
  }
  if (!(show_grouping %in% c("yes", "no"))) {
    stop("Please specify show_grouping either as \"yes\" or \"no\".")
  }
  if (!is.numeric(y_range) | length(y_range) != 2) {
    stop("The parameter y_range must be a vector of length 2.")
  }
  if (y_range[1] > y_range[2]) {
    stop("The parameter y_range must be of the form c(min, max).")
  }
  if (!is.null(x_range)) {
    if (!is.numeric(x_range) | length(x_range) != 2) {
      stop("The parameter x_range must be a vector of length 2.")
    }
    if (x_range[1] > x_range[2]) {
      stop("The parameter x_range must be of the form c(min, max).")
    }
  }
  if (!(scenario %in% c("standard", "worst"))) {
    stop("Please specify scenario either as \"standard\" or \"worst\".")
  }
  if (!(plot_option %in% c("full", "lean1", "lean2", "basic1", "basic2"))) {
    stop("Please specify plot_option either as \"full\", \"lean1\", ",
         "\"lean2\", \"basic1\" or \"basic2\".")
  }
  if (!(ci_app %in% c("line", "ribbon"))) {
    stop("Please specify ci_app either as \"line\" or \"ribbon\".")
  }

  if (is.null(response_vbl_unit)) {
    rvu <- ""
  } else {
    if (!is.character(response_vbl_unit)) {
      stop("The parameter response_vbl_unit must be a string.")
    } else {
      rvu <- response_vbl_unit
    }
  }

  expob <- model

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extraction of models and of the model type
  # If show_grouping = "no" the model_type is "cics"

  l_models <- expob[["Models"]]

  if (show_grouping == "yes") {
    model_name <- expob[["Model.Type"]][["type.acronym"]]
  } else {
    model_name <- "cics"
  }

  # Most appropriate model based on the ANCOVA analysis (show_grouping = "yes")
  # or marginal model (show_grouping = "no")
  model <- l_models[[model_name]]
  poi_model_name <- paste("POI.Model", model_name, sep = ".")
  sl_model_name <- paste("Shelf.Life", model_name, sep = ".")
  wcsl_model_name <- paste("WCSL", model_name, sep = ".")

  # <-><-><->
  # Checking if estimation of POI.Model or Shelf.Life was successful (for the
  # release limit value that was deemed relevant, i.e. the one specified by
  # the rl_index parameter)

  t_exp <- expob[["POI"]]

  if (sum(is.na(
    t_exp[rl_index,
          c(grep(paste("Shelf.Life", model_name, sep = "."), names(t_exp)),
            grep(paste("POI.Model", model_name, sep = "."), names(t_exp)))]))
    > 0) {
    stop("Expiry determination was not successful.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extraction of data and parameters

  d_dat <- expob[["Data"]]

  alpha <- expob[["Parameters"]][["alpha"]]
  ivl <- expob[["Parameters"]][["ivl"]]
  ivl_type <- expob[["Parameters"]][["ivl.type"]]
  ivl_side <- expob[["Parameters"]][["ivl.side"]]

  if (expob[["Limits"]][["sf.option"]] == "tight") {
    rl_sf <- expob[["Limits"]][["rl.sf"]]
    sl_sf <- expob[["Limits"]][["sl.sf"]]
  } else {
    rl_sf <- expob[["Limits"]][["rl.sf"]] + 1
    sl_sf <- expob[["Limits"]][["sl.sf"]] + 1
  }

  xform <- expob[["Limits"]][["xform"]]
  shift <- expob[["Limits"]][["shift"]]
  wc_icpt <- expob[["wc.icpt"]][, model_name]

  if (sum(xform %in% "no") == 2) {
    response_vbl <- expob[["Variables"]][["response"]]
    time_vbl <- expob[["Variables"]][["time"]]
    batch_vbl <- expob[["Variables"]][["batch"]]
  }
  if (sum(xform %in% "no") == 0) {
    old_response_vbl <- expob[["Variables"]][["response.orig"]]
    response_vbl <- expob[["Variables"]][["response"]]
    old_time_vbl <- expob[["Variables"]][["time.orig"]]
    time_vbl <- expob[["Variables"]][["time"]]
    batch_vbl <- expob[["Variables"]][["batch"]]
  }
  if (sum(xform %in% "no") == 1) {
    if (xform[1] != "no") {
      response_vbl <- expob[["Variables"]][["response"]]
      old_time_vbl <- expob[["Variables"]][["time.orig"]]
      time_vbl <- expob[["Variables"]][["time"]]
      batch_vbl <- expob[["Variables"]][["batch"]]
    }
    if (xform[2] != "no") {
      old_response_vbl <- expob[["Variables"]][["response.orig"]]
      response_vbl <- expob[["Variables"]][["response"]]
      time_vbl <- expob[["Variables"]][["time"]]
      batch_vbl <- expob[["Variables"]][["batch"]]
    }
  }

  # Note: since the predict.lm() function from the 'stats' package always
  # calculates two-sided limits the value of alpha must be doubled in case
  # that the ivl_type is "one-sided".
  if (ivl_type == "one.sided") {
    alpha <- alpha * 2
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extraction of limits and the intercept

  l_lim <- expob[["Limits"]]

  sl <- l_lim[["sl"]]
  rl <- l_lim[["rl"]]

  # POI with the upper or lower confidence or prediction interval of the
  # linear regression model and worst case scenario (woca) case
  # Most appropriate model
  poi_model <- t_exp[rl_index, poi_model_name]
  poi_woca <- t_exp[rl_index, sl_model_name]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Setting ranges and ticks

  # Find a sequence for a graph with 10 ticks displaying numbers divisible by
  # 3 and which contains 10 numbers between each tick, i.e. a sequence with a
  # maximal length of 100. The value of scenario ("standard" or "worst") must
  # be taken into account.
  # For the setting of the optimal x range t_min and t_max are determined.

  if (!is.null(x_range)) {
    t_min <- x_range[1]
  } else {
    if (xform[1] == "no") {
      t_min <- pretty(d_dat[, time_vbl], n = 1)[1]
    } else {
      t_min <- pretty(d_dat[, old_time_vbl], n = 1)[1]
    }
  }

  if (!is.null(x_range)) {
    t_max <- x_range[2]
  } else {
    switch(scenario,
           "standard" = {
             t_max <- pretty(poi_model, n = 1)[2]
           },
           "worst" = {
             t_max <- pretty(poi_woca, n = 1)[2]
           })
  }

  # Setting x_range and the x_breaks where the number of ticks should be 5
  x_range <- c(t_min, t_max)
  x_breaks <- pretty(x_range, 5)

  if (plot_option == "full") {
    x_range[1] <- x_breaks[1] - x_breaks[2]
  }

  # Setting the y_breaks where the number of ticks should be 5.
  y_breaks <- pretty(y_range, 5)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prediction based on linear model

  # Generate the new x values (on the original scale) for prediction
  x_new <- seq(t_min, t_max, length.out = 100)

  # Transformation of new x values, if necessary
  switch(xform[1],
         "no" = {
         },
         "log" = {
           x_new_trfmd <- log(x_new + shift[1])
         },
         "sqrt" = {
           x_new_trfmd <- sqrt(x_new + shift[1])
         },
         "sq" = {
           x_new_trfmd <- (x_new + shift[1])^2
         })

  # Creation of data frame for the predict.lm() function
  t_batches <- levels(d_dat[, batch_vbl])

  if (xform[1] == "no") {
    d_new <- data.frame(rep(t_batches, each = length(x_new)),
                        rep(x_new, times = length(t_batches)))
  } else {
    d_new <- data.frame(rep(t_batches, each = length(x_new_trfmd)),
                        rep(x_new_trfmd, times = length(t_batches)))
  }

  colnames(d_new) <- c(batch_vbl, time_vbl)

  # Prediction
  if (model_name != "dids") {
    m_pred <- predict(model, newdata = d_new, interval = ivl,
                      level = 1 - alpha)
  } else {
    l_pred <- lapply(t_batches, function(x)
      predict(l_models$individual[[x]],
              newdata = d_new[d_new[, batch_vbl] == x, ],
              interval = ivl, level = 1 - alpha))
    m_pred <- do.call(rbind, l_pred)
  }

  # Back-transformation of predicted (response) values, if necessary
  switch(xform[2],
         "no" = {
         },
         "log" = {
           m_pred <- exp(m_pred) - shift[2]
         },
         "sqrt" = {
           m_pred <- m_pred^2 - shift[2]
         },
         "sq" = {
           m_pred[m_pred < 0] <- NA
           m_pred <- sqrt(m_pred) - shift[2]
         })

  # Generation of data frame for plotting (with x values in original scale)
  if (sum(xform %in% "no") == 2) {
    d_pred <- as.data.frame(cbind(d_new, m_pred))
    colnames(d_pred) <- c(batch_vbl, time_vbl, response_vbl, "LL", "UL")
  }
  if (sum(xform %in% "no") == 0) {
    d_pred <- data.frame(rep(t_batches, each = length(x_new)),
                         rep(x_new, times = length(t_batches)),
                         m_pred)
    colnames(d_pred) <- c(batch_vbl, old_time_vbl, old_response_vbl, "LL", "UL")
  }
  if (sum(xform %in% "no") == 1) {
    if (xform[1] != "no") {
      d_pred <- data.frame(rep(t_batches, each = length(x_new)),
                           rep(x_new, times = length(t_batches)),
                           m_pred)
      colnames(d_pred) <- c(batch_vbl, old_time_vbl, response_vbl, "LL", "UL")
    }
    if (xform[2] != "no") {
      d_pred <- as.data.frame(cbind(d_new, m_pred))
      colnames(d_pred) <- c(batch_vbl, time_vbl, old_response_vbl, "LL", "UL")
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Generation of ancillary data frames for plotting

  # <-><-><->
  # d_text - display of text elements
  # The rows in data frame d_text have the following meaning and position
  # (position in brackets):
  # LSL (lower right), USL (upper right),
  # WCSL (left, at RL), Intercept (left, at intercept),
  # POI worst case (low at poi.woca), POI model (low at poi.model)
  # RL (lower right or upper right)

  if (length(sl) == 2) {
    d_text <- data.frame(
      Time = c(rep(x_range[2], 2), 0, 0, poi_woca, poi_model),
      Response = c(sl, t_exp[rl_index, wcsl_model_name], wc_icpt[rl_index],
                   rep(sl[1], 2)),
      Label = c(print_val("LSL: ", sl[1], rvu, sl_sf[1]),
                print_val("USL: ", sl[2], rvu, sl_sf[2]),
                print_val("", t_exp[rl_index, wcsl_model_name], rvu,
                          rl_sf[rl_index], suffix = " "),
                print_val("", wc_icpt[rl_index], rvu, rl_sf[rl_index]),
                ifelse(plot_option %in% "lean2",
                       print_val("", poi_woca, "",
                                 get_n_whole_part(poi_woca) + 1),
                       print_val("", poi_woca, "",
                                 get_n_whole_part(poi_woca) + 1,
                                 suffix = "\n(worst case scenario)")),
                ifelse(plot_option %in% "lean2",
                       print_val("", poi_model, "",
                                 get_n_whole_part(poi_model) + 1),
                       print_val("", poi_model, "",
                                 get_n_whole_part(poi_model) + 1,
                                 suffix = "\n(standard scenario)"))),
      Colour = c("black", "black", "red", "royalblue", "forestgreen", "grey50"),
      stringsAsFactors = FALSE)

    switch(ivl_side,
           "lower" = {
             d_text <- rbind(d_text, d_text[nrow(d_text), ])
             d_text[nrow(d_text), "Time"] <- x_range[2]
             d_text[nrow(d_text), "Response"] <- t_exp[rl_index, "Rel.Spec"]
             d_text[nrow(d_text), "Label"] <-
               print_val("LRL: ", t_exp[rl_index, "Rel.Spec"], rvu,
                         rl_sf[rl_index])
             d_text[nrow(d_text), "Colour"] <- "grey0"

             d_text$Response <- d_text$Response +
               c(rep(diff(y_breaks[1:2]), 2), 0, 0,
                 rep(diff(y_breaks[1:2]), 2),
                 diff(y_breaks[1:2])) * 1 / c(-10, 10, 1, 1, -2, -2, -10)
           },
           "upper" = {
             d_text <- rbind(d_text, d_text[nrow(d_text), ])
             d_text[nrow(d_text), "Time"] <- x_range[2]
             d_text[nrow(d_text), "Response"] <- t_exp[rl_index, "Rel.Spec"]
             d_text[nrow(d_text), "Label"] <-
               print_val("URL: ", t_exp[rl_index, "Rel.Spec"], rvu,
                         rl_sf[rl_index])
             d_text[nrow(d_text), "Colour"] <- "grey0"
             d_text[5:6, "Response"] <- rep(sl[2], 2)

             d_text$Response <- d_text$Response +
               c(rep(diff(y_breaks[1:2]), 2), 0, 0,
                 rep(diff(y_breaks[1:2]), 2),
                 diff(y_breaks[1:2])) * 1 / c(10, 10, 1, 1, 2, 2, 1)
           })
  } else {
    switch(ivl_side,
           "lower" = {
             d_text <- data.frame(
               Time = c(x_range[2], 0, 0, poi_woca, poi_model, x_range[2]),
               Response = c(sl, t_exp[rl_index, wcsl_model_name],
                            wc_icpt[rl_index], rep(sl, 2),
                            t_exp[rl_index, "Rel.Spec"]),
               Label =
                 c(print_val("LSL: ", sl, rvu, sl_sf),
                   print_val("", t_exp[rl_index, wcsl_model_name], rvu,
                             rl_sf[rl_index], suffix = " "),
                   print_val("", wc_icpt[rl_index], rvu, rl_sf[rl_index],
                             suffix = " "),
                   ifelse(plot_option %in% "lean2",
                          print_val("", poi_woca, "",
                                    get_n_whole_part(poi_woca) + 1),
                          print_val("", poi_woca, "",
                                    get_n_whole_part(poi_woca) + 1,
                                    suffix = "\n(worst case scenario)")),
                   ifelse(plot_option %in% "lean2",
                          print_val("", poi_model, "",
                                    get_n_whole_part(poi_model) + 1),
                          print_val("", poi_model, "",
                                    get_n_whole_part(poi_model) + 1,
                                    suffix = "\n(standard scenario)")),
                   print_val("LRL: ", t_exp[rl_index, "Rel.Spec"], rvu,
                             rl_sf[rl_index])),
               Colour = c("black", "red", "royalblue", "forestgreen", "grey50",
                          "grey0"),
               stringsAsFactors = FALSE)
             d_text$Response <- d_text$Response +
               c(diff(y_breaks[1:2]), 0, 0,
                 rep(diff(y_breaks[1:2]), 2),
                 diff(y_breaks[1:2])) * 1 / c(-10, 1, 1, -2, -2, -10)
           },
           "upper" = {
             d_text <- data.frame(
               Time = c(x_range[2], 0, 0, poi_woca, poi_model, x_range[2]),
               Response = c(sl, t_exp[rl_index, wcsl_model_name],
                            wc_icpt[rl_index], rep(sl, 2),
                            t_exp[rl_index, "Rel.Spec"]),
               Label =
                 c(print_val("USL: ", sl, rvu, sl_sf),
                   print_val("",  t_exp[rl_index, wcsl_model_name], rvu,
                             rl_sf[rl_index], suffix = " "),
                   print_val("", wc_icpt[rl_index], rvu, rl_sf[rl_index],
                             suffix = " "),
                   ifelse(plot_option %in% "lean2",
                          print_val("", poi_woca, "",
                                    get_n_whole_part(poi_woca) + 1),
                          print_val("", poi_woca, "",
                                    get_n_whole_part(poi_woca) + 1,
                                    suffix = "\n(worst case scenario)")),
                   ifelse(plot_option %in% "lean2",
                          print_val("", poi_model, "",
                                    get_n_whole_part(poi_model) + 1),
                          print_val("", poi_model, "",
                                    get_n_whole_part(poi_model) + 1,
                                    suffix = "\n(standard scenario)")),
                   print_val("URL: ", t_exp[rl_index, "Rel.Spec"], rvu,
                             rl_sf[rl_index])),
               Colour = c("black", "red", "royalblue", "forestgreen", "grey50",
                          "grey0"),
               stringsAsFactors = FALSE)
             d_text$Response <- d_text$Response +
               c(diff(y_breaks[1:2]), 0, 0,
                 rep(diff(y_breaks[1:2]), 2),
                 diff(y_breaks[1:2])) * 1 / c(10, 1, 1, 2, 2, 10)
           })
  }

  if (sum(xform %in% "no") == 2) {
    colnames(d_text) <- c(time_vbl, response_vbl, "Label", "Colour")
  }
  if (sum(xform %in% "no") == 0) {
    colnames(d_text) <- c(old_time_vbl, old_response_vbl, "Label", "Colour")
  }
  if (sum(xform %in% "no") == 1) {
    if (xform[1] != "no") {
      colnames(d_text) <- c(old_time_vbl, response_vbl, "Label", "Colour")
    }
    if (xform[2] != "no") {
      colnames(d_text) <- c(time_vbl, old_response_vbl, "Label", "Colour")
    }
  }

  # If plot_option is "full": plot the complete information.
  # If plot_option is "lean1" or "lean2": plot LRL or URL, USL and / or LSL,
  # worst case and standard scenario.
  # If plot_option is "basic1" or "basic2": plot USL and / or LSL.
  switch(plot_option,
         "full" = {
           show_text <- rep(TRUE, nrow(d_text))
         },
         "lean1" = {
           show_text <- d_text$Colour %in% c("black", "forestgreen", "grey50")
         },
         "lean2" = {
           show_text <- d_text$Colour %in% c("black", "forestgreen", "grey50")
         },
         "basic1" = {
           show_text <- d_text$Colour %in% c("black")
         },
         "basic2" = {
           show_text <- rep(FALSE, nrow(d_text))
         })

  # <-><-><->
  # d_hlines - display of horizontal lines

  if (length(sl) == 2) {
    d_hlines <- data.frame(Response = sl,
                           Item = c("LSL", "USL"),
                           Colour = as.character(c("black", "black")),
                           Type = as.character(c("dotted", "dotted")),
                           stringsAsFactors = FALSE)
  } else {
    switch(ivl_side,
           "lower" = {
             d_hlines <-
               data.frame(Response = sl,
                          Item = c("LSL"),
                          Colour = as.character(c("black")),
                          Type = as.character(c("dotted")),
                          stringsAsFactors = FALSE)
           },
           "upper" = {
             d_hlines <-
               data.frame(Response = sl,
                          Item = c("USL"),
                          Colour = as.character(c("black")),
                          Type = as.character(c("dotted")),
                          stringsAsFactors = FALSE)
           })
  }

  if (xform[2] != "no") {
    colnames(d_hlines) <- c(old_response_vbl, "Item", "Colour", "Type")
  } else {
    colnames(d_hlines) <- c(response_vbl, "Item", "Colour", "Type")
  }

  # <-><-><->
  # d_vlines - display of vertical lines

  d_vlines <- data.frame(Month = c(poi_woca, poi_model),
                         Item = c("poi.woca", "poi.model"),
                         Colour = c("forestgreen", "grey50"),
                         Type = c("dashed", "dotdash"),
                         stringsAsFactors = FALSE)

  if (xform[1] != "no") {
    colnames(d_vlines) <- c(old_time_vbl, "Item", "Colour", "Type")
  } else {
    colnames(d_vlines) <- c(time_vbl, "Item", "Colour", "Type")
  }

  # <-><-><->
  # d_seg - display of segments explaining the TGA method
  # The columns in data frame d_seg have the following meaning and position
  # (position in brackets):
  # Maximal allowed difference over time from intercept (horizontal),
  # Release Limit (horizontal),
  # Maximal allowed difference over time from SL (vertical),
  # Maximal allowed difference over time from intercept (vertical)
  # The elements of d_seg are not displayed if plot_option is "basic".

  if (length(sl) == 2) {
    switch(ivl_side,
           "lower" = {
             d_seg <- data.frame(
               Time.1 =
                 c(0, 0,
                   -round(sqrt(t_exp[rl_index, sl_model_name]) / 3, 0) / 3,
                   -round(sqrt(t_exp[rl_index, sl_model_name]) / 3, 0) / 9),
               Time.2 =
                 c(poi_woca, x_range[2],
                   -round(sqrt(t_exp[rl_index, sl_model_name]) / 3, 0) / 3,
                   -round(sqrt(t_exp[rl_index, sl_model_name]) / 3, 0) / 9),
               Response.1 =
                 c(t_exp[rl_index, wcsl_model_name], rl[rl_index], sl[1],
                   t_exp[rl_index, wcsl_model_name]),
               Response.2 =
                 c(t_exp[rl_index, wcsl_model_name], rl[rl_index], rl[rl_index],
                   wc_icpt[rl_index]),
               Item = c("x.delta", "x.delta.shifted", "y.delta",
                        "y.delta.shifted"),
               Colour = c("red", "grey0", "grey50", "grey50"),
               Type = c("dashed", "dotted", rep("solid", 2)),
               Size = c(rep(0.5, 2), rep(1, 2)),
               stringsAsFactors = FALSE)
           },
           "upper" = {
             d_seg <- data.frame(
               Time.1 =
                 c(0, 0,
                   -round(sqrt(t_exp[rl_index, sl_model_name]) / 3, 0) / 3,
                   -round(sqrt(t_exp[rl_index, sl_model_name]) / 3, 0) / 9),
               Time.2 =
                 c(poi_woca, x_range[2],
                   -round(sqrt(t_exp[rl_index, sl_model_name]) / 3, 0) / 3,
                   -round(sqrt(t_exp[rl_index, sl_model_name]) / 3, 0) / 9),
               Response.1 =
                 c(t_exp[rl_index, wcsl_model_name], rl[rl_index], sl[2],
                   t_exp[rl_index, wcsl_model_name]),
               Response.2 =
                 c(t_exp[rl_index, wcsl_model_name], rl[rl_index], rl[rl_index],
                   wc_icpt[rl_index]),
               Item = c("x.delta", "x.delta.shifted", "y.delta",
                        "y.delta.shifted"),
               Colour = c("red", "grey0", "grey50", "grey50"),
               Type = c("dashed", "dotted", rep("solid", 2)),
               Size = c(rep(0.5, 2), rep(1, 2)),
               stringsAsFactors = FALSE)
           })
  } else {
    d_seg <- data.frame(
      Time.1 = c(0, 0,
                 -round(sqrt(t_exp[rl_index, sl_model_name]) / 3, 0) / 3,
                 -round(sqrt(t_exp[rl_index, sl_model_name]) / 3, 0) / 9),
      Time.2 = c(poi_woca, x_range[2],
                 -round(sqrt(t_exp[rl_index, sl_model_name]) / 3, 0) / 3,
                 -round(sqrt(t_exp[rl_index, sl_model_name]) / 3, 0) / 9),
      Response.1 =
        c(t_exp[rl_index, wcsl_model_name], rl[rl_index], sl,
          t_exp[rl_index, wcsl_model_name]),
      Response.2 =
        c(t_exp[rl_index, wcsl_model_name], rl[rl_index], rl[rl_index],
          wc_icpt[rl_index]),
      Item = c("x.delta", "x.delta.shifted", "y.delta", "y.delta.shifted"),
      Colour = c("red", "grey0", "grey50", "grey50"),
      Type = c("dashed", "dotted", rep("solid", 2)),
      Size = c(rep(0.5, 2), rep(1, 2)),
      stringsAsFactors = FALSE)
  }

  if (sum(xform %in% "no") == 2) {
    colnames(d_seg) <- c(paste(time_vbl, 1:2, sep = "."),
                         paste(response_vbl, 1:2, sep = "."),
                         "Item", "Colour", "Type", "Size")
  }
  if (sum(xform %in% "no") == 0) {
    colnames(d_seg) <- c(paste(old_time_vbl, 1:2, sep = "."),
                         paste(old_response_vbl, 1:2, sep = "."),
                         "Item", "Colour", "Type", "Size")
  }
  if (sum(xform %in% "no") == 1) {
    if (xform[1] != "no") {
      colnames(d_seg) <- c(paste(old_time_vbl, 1:2, sep = "."),
                           paste(response_vbl, 1:2, sep = "."),
                           "Item", "Colour", "Type", "Size")
    }
    if (xform[2] != "no") {
      colnames(d_seg) <- c(paste(time_vbl, 1:2, sep = "."),
                           paste(old_response_vbl, 1:2, sep = "."),
                           "Item", "Colour", "Type", "Size")
    }
  }

  # If plot_option is "full": plot the complete information.
  # If plot_option is "lean1" or "lean2": do not plot vertical grey segments.
  # If plot_option is "basic1" or "basic2": no segments are plotted (and thus
  # the object show_seg is not needed.
  if (plot_option == "full") {
    show_seg <- rep(TRUE, nrow(d_seg))
  }

  if (plot_option %in% c("lean1", "lean2")) {
    show_seg <- d_seg$Colour != "grey50"
  }

  # <-><-><->
  # d_arr - display of arrow explaining the TGA method
  # The elements of d_arr are not displayed if plot_option is not "full".

  if (length(sl) == 2) {
    switch(ivl_side,
           "lower" = {
             d_arr <- data.frame(
               Time.1 = -round(sqrt(t_exp[rl_index, sl_model_name]) / 3, 0) / 9,
               Time.2 = -round(sqrt(t_exp[rl_index, sl_model_name]) / 3, 0) / 2,
               Response.1 = c((t_exp[rl_index, wcsl_model_name] +
                                 wc_icpt[rl_index]) / 2),
               Response.2 = c((sl[1] + rl[rl_index]) / 2),
               Item = c("arrow"),
               Colour = c("grey50"),
               Line.Type = c("solid"),
               Arrow.Type = c("closed"),
               Size = 0.5,
               Curvature = 0.5,
               Angle = 90,
               Length = ceiling(log2(x_range[2])),
               stringsAsFactors = FALSE)
           },
           "upper" = {
             d_arr <- data.frame(
               Time.1 = -round(sqrt(t_exp[rl_index, sl_model_name]) / 3, 0) / 9,
               Time.2 = -round(sqrt(t_exp[rl_index, sl_model_name]) / 3, 0) / 2,
               Response.1 = c((t_exp[rl_index, wcsl_model_name] +
                                 wc_icpt[rl_index]) / 2),
               Response.2 = c((sl[2] + rl[rl_index]) / 2),
               Item = c("arrow"),
               Colour = c("grey50"),
               Line.Type = c("solid"),
               Arrow.Type = c("closed"),
               Size = 0.5,
               Curvature = -0.5,
               Angle = 90,
               Length = ceiling(log2(x_range[2])),
               stringsAsFactors = FALSE)
           })
  } else {
    switch(ivl_side,
           "lower" = {
             d_arr <- data.frame(
               Time.1 = -round(sqrt(t_exp[rl_index, sl_model_name]) / 3, 0) / 9,
               Time.2 = -round(sqrt(t_exp[rl_index, sl_model_name]) / 3, 0) / 2,
               Response.1 = c((t_exp[rl_index, wcsl_model_name] +
                                 wc_icpt[rl_index]) / 2),
               Response.2 = c((sl + rl[rl_index]) / 2),
               Item = c("arrow"),
               Colour = c("grey50"),
               Line.Type = c("solid"),
               Arrow.Type = c("closed"),
               Size = 0.5,
               Curvature = 0.5,
               Angle = 90,
               Length = ceiling(log2(x_range[2])),
               stringsAsFactors = FALSE)
           },
           "upper" = {
             d_arr <- data.frame(
               Time.1 = -round(sqrt(t_exp[rl_index, sl_model_name]) / 3, 0) / 9,
               Time.2 = -round(sqrt(t_exp[rl_index, sl_model_name]) / 3, 0) / 2,
               Response.1 = c((t_exp[rl_index, wcsl_model_name] +
                                 wc_icpt[rl_index]) / 2),
               Response.2 = c((sl + rl[rl_index]) / 2),
               Item = c("arrow"),
               Colour = c("grey50"),
               Line.Type = c("solid"),
               Arrow.Type = c("closed"),
               Size = 0.5,
               Curvature = -0.5,
               Angle = 90,
               Length = ceiling(log2(x_range[2])),
               stringsAsFactors = FALSE)
           })
  }

  if (sum(xform %in% "no") == 2) {
    colnames(d_arr) <- c(paste(time_vbl, 1:2, sep = "."),
                         paste(response_vbl, 1:2, sep = "."),
                         "Item", "Colour", "Line.Type", "Arrow.Type",
                         "Size", "Curvature", "Angle", "Length")
  }
  if (sum(xform %in% "no") == 0) {
    colnames(d_arr) <- c(paste(old_time_vbl, 1:2, sep = "."),
                         paste(old_response_vbl, 1:2, sep = "."),
                         "Item", "Colour", "Line.Type", "Arrow.Type",
                         "Size", "Curvature", "Angle", "Length")
  }
  if (sum(xform %in% "no") == 1) {
    if (xform[1] != "no") {
      colnames(d_arr) <- c(paste(old_time_vbl, 1:2, sep = "."),
                           paste(response_vbl, 1:2, sep = "."),
                           "Item", "Colour", "Line.Type", "Arrow.Type",
                           "Size", "Curvature", "Angle", "Length")
    }
    if (xform[2] != "no") {
      colnames(d_arr) <- c(paste(time_vbl, 1:2, sep = "."),
                           paste(old_response_vbl, 1:2, sep = "."),
                           "Item", "Colour", "Line.Type", "Arrow.Type",
                           "Size", "Curvature", "Angle", "Length")
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Generation of ggplot object

  # Resetting time_vbl and response_vbl, if necessary
  if (sum(xform %in% "no") == 0) {
    time_vbl <- old_time_vbl
    response_vbl <- old_response_vbl
  }
  if (sum(xform %in% "no") == 1) {
    if (xform[1] != "no") {
      time_vbl <- old_time_vbl
    }
    if (xform[2] != "no") {
      response_vbl <- old_response_vbl
    }
  }

  if (show_grouping == "no") {
    ggraph <- ggplot(d_dat, aes_string(x = time_vbl, y = response_vbl)) +
      geom_point(size = 2, shape = 1) +
      geom_line(data = d_pred,
                aes_string(x = time_vbl, y = response_vbl),
                colour = "royalblue", linetype = "solid")

    switch(ci_app,
           "line" = {
             ggraph <- ggraph +
               geom_line(data = d_pred, aes_string(x = time_vbl, y = "LL"),
                         colour = "royalblue", linetype = "solid",
                         size = 0.5) +
               geom_line(data = d_pred, aes_string(x = time_vbl, y = "UL"),
                         colour = "royalblue", linetype = "solid",
                         size = 0.5)
           },
           "ribbon" = {
             ggraph <- ggraph +
               geom_ribbon(data = d_pred, aes_string(ymin = "LL", ymax = "UL"),
                           fill = "royalblue", alpha = 0.25)
           })

    ggraph <- ggraph + theme(legend.position = "none")
  } else {
    ggraph <- ggplot(d_dat, aes_string(x = time_vbl, y = response_vbl)) +
      geom_point(size = 2, aes_string(colour = batch_vbl, shape = batch_vbl)) +
      geom_line(data = d_pred,
                aes_string(x = time_vbl, y = response_vbl, colour = batch_vbl),
                linetype = "solid")

    switch(ci_app,
           "line" = {
             ggraph <- ggraph +
               geom_line(data = d_pred, aes_string(x = time_vbl, y = "LL",
                                                   colour = batch_vbl),
                         linetype = "solid", size = 0.5) +
               geom_line(data = d_pred, aes_string(x = time_vbl, y = "UL",
                                                   colour = batch_vbl),
                         linetype = "solid", size = 0.5)
           },
           "ribbon" = {
             ggraph <- ggraph +
               geom_ribbon(data = d_pred, aes_string(ymin = "LL", ymax = "UL",
                                                     fill = batch_vbl),
                           alpha = 0.25)
           })

    ggraph <- ggraph + theme(legend.position = c(0.04, 0.96),
                             legend.justification = c(0.1, 1),
                             legend.text = element_text(size = 11),
                             legend.title = element_blank(),
                             legend.key = element_rect(colour = "white"),
                             legend.key.size = unit(1.5, "lines"),
                             legend.key.height = unit(0.9, "line"))
  }

  ggraph <- ggraph +
    geom_hline(yintercept = d_hlines[, response_vbl],
               colour = d_hlines$Colour, linetype = d_hlines$Type) +
    scale_x_continuous(limits = x_range, breaks = x_breaks) +
    scale_y_continuous(limits = y_range, breaks = y_breaks) +
    theme_bw()  +
    theme(panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(0.2, 0.4, 0.2, 0.2), "lines"),
          plot.title = element_text(size = 12, hjust = 0.5, vjust = -1),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.ticks.length = unit(0.5, "lines"))

  if (plot_option %in% c("basic1", "lean1", "lean2", "full")) {
    ggraph <- ggraph +
      geom_text(data = d_text[show_text, ],
                aes_string(x = time_vbl, y = response_vbl),
                label = d_text[show_text, "Label"], hjust = "right",
                size = 4, lineheight = 0.8,
                colour = d_text[show_text, "Colour"])
  }

  if (plot_option %in% c("lean1", "lean2", "full")) {
    ggraph <- ggraph +
      geom_vline(xintercept = d_vlines[, time_vbl],
                 colour = d_vlines$Colour, linetype = d_vlines$Type) +
      geom_segment(data = d_seg[show_seg, ],
                   aes_string(x = paste(time_vbl, 1, sep = "."),
                              y = paste(response_vbl, 1, sep = "."),
                              xend = paste(time_vbl, 2, sep = "."),
                              yend = paste(response_vbl, 2, sep = ".")),
                   colour = d_seg$Colour[show_seg],
                   linetype = d_seg$Type[show_seg],
                   size = d_seg$Size[show_seg])
  }

  if (plot_option == "full") {
    ggraph <- ggraph +
      geom_curve(data = d_arr,
                 aes_string(x = paste(time_vbl, 1, sep = "."),
                            y = paste(response_vbl, 1, sep = "."),
                            xend = paste(time_vbl, 2, sep = "."),
                            yend = paste(response_vbl, 2, sep = ".")),
                 colour = d_arr$Colour, linetype = d_arr$Line.Type,
                 size = d_arr$Size, curvature = d_arr$Curvature,
                 angle = d_arr$Angle, ncp = 20, lineend = "round",
                 arrow = arrow(length = unit(d_arr$Length, "points"),
                               type = d_arr$Arrow.Type))
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Collecting the results

  invisible(structure(list(Model = model,
                           Expiry = t_exp,
                           Graph = ggraph,
                           Prediction = d_pred,
                           text = d_text,
                           hlines = d_hlines,
                           vlines = d_vlines,
                           segments = d_seg,
                           arrow = d_arr),
                      class = "plot_expirest_wisle"))
}
