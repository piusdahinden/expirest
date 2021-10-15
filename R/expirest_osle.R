#' Ordinary shelf life estimation (osle)
#'
#' Based on a linear regression model fitted to a stability data set the
#' function \code{expirest_osle()} estimates the shelf life, or retest period,
#' following the ICH Q1E guideline. The abbreviation \dQuote{osle} stands for
#' \dQuote{ordinary shelf life estimation}.
#'
#' @param data A data frame with the columns specified by \code{response_vbl},
#'   \code{time_vbl} and \code{batch_vbl}.
#' @param response_vbl A character string specifying the response variable name
#'   that must be a column of \code{data}.
#' @param time_vbl A character string specifying the time variable name that
#'   must be a column of \code{data}.
#' @param batch_vbl A character string specifying the column in \code{data}
#'   with the grouping information (i.e. a factorial variable) for the
#'   differentiation of the observations of the different batches.
#' @param sl A numeric value or a numeric vector of length \code{2} specifying
#'   the specification limit or limits. If a vector is provided it must be of
#'   the form \code{c(lower limit, upper limit)}.
#' @param sl_sf A positive integer or a vector of positive integers specifying
#'   the number of \dQuote{significant figures} (sf) of \code{sl}. It must have
#'   the same length as \code{sf}.
#' @param srch_range A vector of length \code{2} specifying the end-points of
#'   the (time) range that is supposed to contain the shelf life or retest
#'   period.
#' @param alpha A numeric value specifying the significance level for the
#'   calculation of confidence or prediction intervals. The default value is
#'   \code{0.05}.
#' @param alpha_pool A numeric value specifying the type I error rate for the
#'   test of the poolability of the batches. The default value is \code{0.25},
#'   following ICH Q1E guideline. The value should not be changed unless
#'   supported by well-founded reasons.
#' @param xform A vector of two character strings specifying the transformation
#'   of the response and the time variable. The default is \dQuote{no}
#'   transformation, i.e. \code{c("no", "no")}, where the first element
#'   specifies the transformation of the \eqn{x} variable and the second
#'   element the transformation of the \eqn{y} variable. Valid alternatives
#'   for \eqn{x} and/or \eqn{y} variable transformation are \code{"log"}
#'   (natural logarithm), \code{"sqrt"} (square root) and \code{"sq"} (square).
#' @param shift A vector of two values which will be added to the variables
#'   \eqn{x} and/or \eqn{y} before they are transformed as specified by the
#'   \code{xform} parameter, where the first element will be added to the
#'   \eqn{x} variable and the second element to the \eqn{y} variable. The
#'   purpose is to prevent an undefined state which could arise when variables
#'   with values of \eqn{\leq 0} are log or square root transformed. The
#'   default is \code{c(0, 0)}. For transformation of the \eqn{x} variable
#'   \eqn{1} should be chosen as shift parameter in case of \code{log} and
#'   \eqn{0} in case of \code{sqrt} and \code{sq} transformation because
#'   then the intercept will be at \eqn{x = 0}. For transformation of the
#'   \eqn{y} variable any value can be chosen as shift parameter.
#' @param sf_option A character string specifying if the limits (\code{rl}
#'   or \code{sl}) should be regarded as \dQuote{tight} or \dQuote{loose}, i.e.
#'   either \code{"tight"} or \code{"loose"}, respectively. The option
#'   \code{"tight"} means that the limits are rounded to the specified number
#'   of significant figures specified by the parameters \code{rl_sf} and
#'   \code{sl_sf}. In case of the option \code{"loose"} the limits are rounded
#'   to the specified number of significant figures (\eqn{n}), followed by the
#'   subtraction of \eqn{1} from the \eqn{n^{th}} digit and addition of
#'   \eqn{5} to the \eqn{(n + 1)^{th}} digit if \code{ivl_side} is
#'   \code{"lower"}, or followed by the addition of \eqn{4} to the
#'   \eqn{(n + 1)^{th}} digit if \code{ivl_side} is \code{"upper"}.
#' @param ivl A character string of either \code{"confidence"} or
#'   \code{"prediction"}, i.e. specifying the type of interval of interest.
#'   The default is \code{"confidence"}.
#' @param ivl_type A character string specifying if a \dQuote{one sided} or
#'   a \dQuote{two sided} interval should be calculated, i.e. either
#'   \code{"one.sided"} or \code{"two.sided"}, respectively. The default is
#'   \code{"one.sided"}.
#' @param ivl_side A character string specifying if the \dQuote{upper} or the
#'   \dQuote{lower} limit is the relevant limit, i.e. either \code{"upper"} or
#'   \code{"lower"}, respectively. The default is \code{"lower"}.
#' @param ... Additional named or unnamed arguments passed on to
#'   \code{uniroot()}.
#'
#' @details According to ICH Q1E guideline, regression analysis coupled with
#' confidence interval is the recommended \dQuote{appropriate method} for
#' determining expiries. This method should be used to determine the
#' \dQuote{\emph{earliest time point at which the 95 percent confidence limit
#' for the mean intersects the proposed acceptance criterion}} (in this package,
#' this point is called the \dQuote{point of intersection} (POI)). Furthermore,
#' it says that \dQuote{\emph{for an attribute known to increase with time, the
#' upper one-sided 95 percent confidence limit should be compared to the
#' acceptance criterion. For an attribute that can either increase or decrease,
#' or whose direction of change is not known, two-sided 95 percent confidence
#' limits should be calculated and compared to the upper and lower acceptance
#' criteria.}} With respect to the number of batches to be included in the
#' analysis it says that \dQuote{\emph{the retest period or shelf life is
#' generally estimated based on the stability data from a minimum of three
#' batches.}}
#'
#' @section Checking batch poolability:
#' According to ICH Q1E guideline, construction of the 95\% confidence interval
#' on the basis of the combined data of all test batches is allowed only if it
#' has been confirmed by aid of a statistical test whether the regression lines
#' from the different batches have a common slope and a common intercept. A
#' significance level of \code{alpha_pool = 0.25} has to be used for both
#' batch-related terms, and the test of the slopes has to precede the test of
#' the intercepts. From these tests, three possible models may be appropriate,
#' i.e.
#' \itemize{
#'  \item a \emph{common intercept / common slope} model (cics),
#'  \item a \emph{different intercept / common slope} model (dics) or
#'  \item a \emph{different intercept / different slope} model (dids).
#' }
#' The \emph{common intercept / different slope} model is not of practical
#' relevance because the corresponding model is missing an effect. If the slopes
#' are significantly different, there is no point comparing intercepts.
#'
#' These requirements can be checked by aid of an \dQuote{ANalysis of
#' COVAriance} (ANCOVA) including the batch variable as interaction term. The
#' full ANCOVA model simultaneously tests all the effects, and non-significant
#' effects can be identified and removed for fitting of the final regression
#' model that is used for the estimation of the shelf life or retest period.
#'
#' The significance level (\code{alpha_pool = 0.25}, Type I error) is used to
#' increase the power of the test to detect cases where the data should not be
#' pooled. Setting \code{alpha_pool = 0.25} decreases the probability of
#' incorrectly concluding that stability data from multiple batches can be
#' pooled. On the other hand, though, it increases the probability of using a
#' single batch to determine expiry when pooling batches would be more
#' appropriate.
#'
#' @return An object of class \sQuote{\code{expirest_osle}} is returned,
#' containing the following list elements:
#' \item{Data}{Data frame of the original data including new columns with
#'   transformed variables, if applicable.}
#' \item{Parameters}{A list of the parameters with the elements \code{alpha},
#'   \code{alpha.pool}, \code{ivl}, \code{ivl.type} and \code{ivl.side}.}
#' \item{Variables}{A list of the variable names, i.e. the original names of
#'   the \code{batch_vbl}, the \code{time_vbl} and the \code{response_vbl} and,
#'   if applicable, of the transformed variables.}
#' \item{Model.Type}{A list of five elements specifying which model, based on
#'   the ANCOVA analysis, suits best. The first element (\code{type.spec})
#'   is a numeric vector of length 2 specifying the best model accepted at the
#'   significance level of 0.25. The first number represents the decision on
#'   the intercept and the second on the slope, where \code{1} stands for
#'   \dQuote{common} and \code{2} stands for \dQuote{different}. The second
#'   element (\code{type.acronym}) is an acronym representing the first item.
#'   The third to fifth elements contain the names of the model variables, i.e.
#'   \code{response.vbl}, \code{time.vbl} and \code{batch.vbl}.}
#' \item{Models}{A list of all possible models (i.e. \sQuote{\code{lm}}
#'   objects) that are relevant, i.e. \code{cics}, \code{dics} and \code{dids}.
#'   The second element of the list is a string summarising the information in
#'   the first element, i.e. either \code{cics}, \code{dics} or \code{dids}.}
#' \item{AIC}{A numeric named vector of the Akaike Information Criterion (AIC)
#'   values of each of the three fitted models.}
#' \item{BIC}{A numeric named vector of the Bayesian Information Criterion (BIC)
#'   values of each of the three fitted models.}
#' \item{wc.icpt}{A numeric value of the worst case intercept.}
#' \item{wc.batch}{A numeric value of the worst case batch.}
#' \item{Limits}{A list of all limits.}
#' \item{Intercepts}{A list of the intercepts of the three fitted models.}
#' \item{POI}{A numeric named vector of the POI values of each of the three
#'   fitted models.}
#'
#' @references
#' International Council for Harmonisation of Technical Requirements for
#' Registration of Pharmaceuticals for Human (ICH), Harmonised Tripartite
<<<<<<< HEAD
#' Guideline, Evaluation of Stability Data Q1E, step 4, February 2003
#' (CPMP/ICH/420/02).\cr
#' \url{https://database.ich.org/sites/default/files/Q1E\%20Guideline.pdf}
=======
#' Guideline, Evaluation of Stability Data Q1E, step 4 version 2003
#' (CPMP/ICH/420/02).\cr
#' \url{https://database.ich.org/sites/default/files/Q1E%20Guideline.pdf}
>>>>>>> c76ed324658ebe549a5521e33e82f3af23392fad
#'
#' @seealso \code{\link{expirest_wisle}}, \code{\link{find_poi}},
#' \code{\link[stats]{uniroot}}, \code{\link[stats]{lm}},
#' \code{\link[stats]{AIC}}.
#'
#' @example man/examples/examples_expirest_osle.R
#'
#' @importFrom stats lm
#' @importFrom stats as.formula
#' @importFrom stats coef
#' @importFrom stats AIC
#' @importFrom stats BIC
#'
#' @export

expirest_osle <- function(data, response_vbl, time_vbl, batch_vbl,
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
  if (xform[1] == "log" & shift[1] != 1) {
    stop("For log transformation of x select a shift of 1.")
  }
  if (xform[1] == "sqrt" & shift[1] != 0) {
    stop("For sqrt transformation of x select a shift of 0.")
  }
  if (xform[1] == "sq" & shift[1] != 0) {
    stop("For sq transformation of x select a shift of 0.")
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

  if (length(sl) == 2) {
    if (ivl_side == "lower" & sum(data[, response_vbl] < sl[1]) > 0) {
      warning("You specified ivl_side = \"lower\". But ",
              round(100 / nrow(data) * sum(data[, response_vbl] < sl[1]), 1),
              "% of the response values are < sl[1]. ",
              "Are you sure that you did not want to set ivl_side = \"upper\"?")
    }
    if (ivl_side == "upper" & sum(data[, response_vbl] > sl[2]) > 0) {
      warning("You specified ivl_side = \"upper\". But ",
              round(100 / nrow(data) * sum(data[, response_vbl] > sl[2]), 1),
              "% of the response values are > sl[2]. ",
              "Are you sure that you did not want to set ivl_side = \"lower\"?")
    }
  } else {
    if (ivl_side == "lower" & sum(data[, response_vbl] < sl) > 0) {
      warning("You specified ivl_side = \"lower\". But ",
              round(100 / nrow(data) * sum(data[, response_vbl] < sl), 1),
              "% of the response values are < sl. ",
              "Are you sure that you did not want to set ivl_side = \"upper\"?")
    }
    if (ivl_side == "upper" & sum(data[, response_vbl] > sl) > 0) {
      warning("You specified ivl_side = \"upper\". But ",
              round(100 / nrow(data) * sum(data[, response_vbl] > sl), 1),
              "% of the response values are > sl. ",
              "Are you sure that you did not want to set ivl_side = \"lower\"?")
    }
  }

  # Check if rl and rl_sf have been provided via the three dot ellipsis and,
  # if so, pass them down to set_limits()
  mf <- list(...)
  mrl <- match(c("rl"), names(mf), 0L)
  mrlsf <- match(c("rl_sf"), names(mf), 0L)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Linearisation of data by variable transformation
  # Transformations:
  #   log: natural logarithm of the variable
  #   sqrt: square root of the variable variable
  #   sq: square of the variable
  #
  # Note: The log and sqrt transformations include adding the value defined by
  #       the shift parameter before performing the transformation.

  d_dat <-
    get_xformed_variables(data = droplevels(data), response_vbl = response_vbl,
                          time_vbl = time_vbl, xform = xform, shift = shift)

  l_variables <-
    get_variable_list(response_vbl = response_vbl, time_vbl = time_vbl,
                      batch_vbl = batch_vbl, xform = xform)

  if (sum(xform %in% "no") == 0) {
    response_vbl <- l_variables[["response"]]
    time_vbl <- l_variables[["time"]]
  }
  if (sum(xform %in% "no") == 1) {
    if (xform[1] != "no") {
      time_vbl <- l_variables[["time"]]
    }
    if (xform[2] != "no") {
      response_vbl <- l_variables[["response"]]
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ANCOVA to figure out which kind of model suits the data best

  tmp <- check_ancova(data = d_dat, response_vbl = response_vbl,
                      time_vbl = time_vbl, batch_vbl = batch_vbl,
                      alpha = alpha_pool)

  common_icpt <- tmp[1]
  common_slp <- tmp[2]

  l_model_type <- list(type.spec = c(common_icpt, common_slp),
                       type.acronym =
                         paste0(c("di", "ci")[common_icpt + 1],
                                c("ds", "cs")[common_slp + 1]))

  if (sum(xform %in% "no") == 2) {
    l_model_type[[length(l_model_type) + 1]] <- l_variables[["response"]]
    l_model_type[[length(l_model_type) + 1]] <- l_variables[["time"]]
    l_model_type[[length(l_model_type) + 1]] <- l_variables[["batch"]]

    names(l_model_type) <- c("type.spec", "type.acronym", "response.vbl",
                             "time.vbl", "batch.vbl")
  }
  if (sum(xform %in% "no") == 0) {
    l_model_type[[length(l_model_type) + 1]] <- l_variables[["response.orig"]]
    l_model_type[[length(l_model_type) + 1]] <- l_variables[["response"]]
    l_model_type[[length(l_model_type) + 1]] <- l_variables[["time.orig"]]
    l_model_type[[length(l_model_type) + 1]] <- l_variables[["time"]]
    l_model_type[[length(l_model_type) + 1]] <- l_variables[["batch"]]

    names(l_model_type) <- c("type.spec", "type.acronym", "response.vbl.orig",
                             "response.vbl", "time.vbl.orig", "time.vbl",
                             "batch.vbl")
  }
  if (sum(xform %in% "no") == 1) {
    if (xform[1] != "no") {
      l_model_type[[length(l_model_type) + 1]] <- l_variables[["response"]]
      l_model_type[[length(l_model_type) + 1]] <- l_variables[["time.orig"]]
      l_model_type[[length(l_model_type) + 1]] <- l_variables[["time"]]
      l_model_type[[length(l_model_type) + 1]] <- l_variables[["batch"]]

      names(l_model_type) <- c("type.spec", "type.acronym", "response.vbl",
                               "time.vbl.orig", "time.vbl", "batch.vbl")
    }
    if (xform[2] != "no") {
      l_model_type[[length(l_model_type) + 1]] <- l_variables[["response.orig"]]
      l_model_type[[length(l_model_type) + 1]] <- l_variables[["response"]]
      l_model_type[[length(l_model_type) + 1]] <- l_variables[["time"]]
      l_model_type[[length(l_model_type) + 1]] <- l_variables[["batch"]]

      names(l_model_type) <- c("type.spec", "type.acronym", "response.vbl.orig",
                               "response.vbl", "time.vbl", "batch.vbl")
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Fitting of all possible models that are relevant

  l_models <- vector(mode = "list", length = 4)
  names(l_models) <- c("cics", "dics", "dids", "individual")

  # ---------
  # Common Intercept / Common Slope
  t_formula <- paste(response_vbl, "~", time_vbl)
  l_models[["cics"]] <-
    do.call("lm", list(as.formula(t_formula), data = as.name("d_dat")))

  # ---------
  # Different Intercept / Common Slope
  t_formula <- paste(response_vbl, "~", paste(batch_vbl, time_vbl, sep = " + "))
  l_models[["dics"]] <-
    do.call("lm", list(as.formula(t_formula), data = as.name("d_dat")))

  # ---------
  # Different Intercept / Different Slope
  t_formula <- paste(response_vbl, "~", paste(batch_vbl, time_vbl, sep = " * "))
  l_models[["dids"]] <-
    do.call("lm", list(as.formula(t_formula), data = as.name("d_dat")))

  # ---------
  # Individual
  t_formula <- paste(response_vbl, "~", time_vbl)
  l_models[["individual"]] <-
    by(data = d_dat, INDICES = d_dat[, batch_vbl], FUN = function(dat)
    do.call("lm", list(as.formula(t_formula), data = as.name("dat"))))

  # ---------
  # Determination of the Akaike Information Criterion (AIC) and Bayesian
  # Information Criterion (BIC) of each of the relevant models

  t_AIC <- t_BIC <- numeric(3)

  for(i in 1:(length(l_models) - 1)) {
    t_AIC[i] <- AIC(l_models[[i]])
    t_BIC[i] <- BIC(l_models[[i]])
  }

  names(t_AIC) <- c("cics", "dics", "dids")
  names(t_BIC) <- c("cics", "dics", "dids")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determination of limits

  # Check if rl and rl_sf have been handed over via the three dot ellipsis
  # (...) argument.
  if (mrl > 0 & mrlsf > 0) {
    l_lim <-
      set_limits(rl = mf[[mrl]], rl_sf = mf[[mrlsf]], sl = sl, sl_sf = sl_sf,
                 sf_option = sf_option, xform = xform, shift = shift,
                 ivl_side = ivl_side)
  } else {
    l_lim <-
      set_limits(rl = NA, rl_sf = NA, sl = sl, sl_sf = sl_sf,
                 sf_option = sf_option, xform = xform, shift = shift,
                 ivl_side = ivl_side)
  }

  # For the following assessments only the relevant specification limit is used.
  if (length(sl) == 2) {
    switch(ivl_side,
           "lower" = {
             sl_orig <- l_lim[["sl.orig"]][1]

             if (xform[2] == "no") {
               sl <- l_lim[["sl"]][1]
             } else {
               sl <- l_lim[["sl.trfmd"]][1]
             }
           },
           "upper" = {
             sl_orig <- l_lim[["sl.orig"]][2]

             if (xform[2] == "no") {
               sl <- l_lim[["sl"]][2]
             } else {
               sl <- l_lim[["sl.trfmd"]][2]
             }
           })
  } else {
    sl_orig <- l_lim[["sl.orig"]]

    if (xform[2] == "no") {
      sl <- l_lim[["sl"]]
    } else {
      sl <- l_lim[["sl.trfmd"]]
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determination of intercepts of all models

  l_icpt <- vector(mode = "list", length = length(l_models) - 1)
  names(l_icpt) <- names(l_models)[1:(length(l_models) - 1)]

  for(i in 1:(length(l_models) - 1)) {
    l_icpt[[i]] <-
      get_icpt(model = l_models[[i]], response_vbl = response_vbl,
               time_vbl = time_vbl, batch_vbl = batch_vbl,
               xform = xform, shift = shift)
  }

  # ---------
  # Check if determination was successful
  if (sum(vapply(l_icpt, is.null, logical(1))) > 0) {
    stop("Not for all models intercepts extracted.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculation of POI values for all three model types

  t_poi <- rep(NA, 3)
  names(t_poi) <- names(l_icpt)

  t_poi_dids <- rep(NA, length(l_models[["individual"]]))
  names(t_poi_dids) <- names(l_models[["individual"]])

  # ---------
  # Common Intercept / Common Slope
  tmp <- try_get_model(
    find_poi(srch_range = srch_range, model = l_models[["cics"]], sl = sl,
             alpha = alpha, ivl_type = ivl_type, ivl_side = ivl_side,
             ivl = ivl)
  )

  if (is.null(tmp[["Error"]])) {
    t_poi["cics"] <- tmp[["Model"]]
  }

  # ---------
  # Different Intercept / Common Slope
  tmp <- try_get_model(
    find_poi(srch_range = srch_range, model = l_models[["dics"]], sl = sl,
             alpha = alpha, ivl_type = ivl_type, ivl_side = ivl_side,
             ivl = ivl)
  )

  if (is.null(tmp[["Error"]])) {
    t_poi["dics"] <- tmp[["Model"]]
  }

  # ---------
  # Different Intercept / Different Slope (i.e. Individual)

  # Note: in this case, the POI is not determined using the model that was
  # determined using the data from all batches (i.e. the full model with the
  # batch_vbl * time_vbl interaction term). Instead, separate models are fitted
  # to the data of each individual batch and the POI values are determined for
  # each of these models. Of these POI values, the smallest is returned.
  for(i in seq_along(l_models[["individual"]])) {
    tmp <- try_get_model(
      find_poi(srch_range = srch_range, model = l_models[["individual"]][[i]],
               sl = sl, alpha = alpha, ivl_type = ivl_type, ivl_side = ivl_side,
               ivl = ivl)
    )

    if (is.null(tmp[["Error"]])) {
      t_poi_dids[i] <- tmp[["Model"]]
    }
  }

  # ---------
  # Check if estimation was successful
  if (sum(is.na(t_poi_dids)) == 0) {
    t_poi["dids"] <- as.numeric((t_poi_dids[which.min(t_poi_dids)]))
  }
  if (sum(is.na(t_poi)) != 0) {
    warning("Not for all model types POI values obtained. ",
            "Possibly, changing srch_range could solve the issue.")
  }

  # ---------
  # Determination of upper or lower confidence or prediction interval limits at
  # t_poi of the best fitting model in order to determine the worst case batch
  # (wc_batch_ich) and its intercept (wc_icpt_ich). If the cics model should be
  # the best fitting model, wc_icpt_ich is the common intercept of all batches.

  if (!is.na(t_poi[l_model_type[[2]]])) {
    pred_lim <- get_intvl_limit(x_new = t_poi[l_model_type[[2]]],
                                model = l_models[[l_model_type[[2]]]],
                                alpha = alpha, ivl_type = ivl_type,
                                ivl_side = ivl_side, ivl = ivl)
  } else {
    pred_lim <- NA
  }

  # ---------
  # Check if estimation was successful
  if (sum(is.na(pred_lim)) == 0) {
    wc_batch_ich <- which.min(abs(sl - pred_lim))
  } else {
    wc_batch_ich <- NA
  }

  # Note: if the model type of the best fitting model is dids, the batch
  # determined as worst case batch may not be the correct batch because it
  # was determined using the model that is based on the data of all batches
  # (i.e. the full model with the batch_vbl * time_vbl interaction term).
  # Therefore, the corresponding wc_batch_ich number is replaced by the number
  # of the batch that had the smallest POI when separate models were fitted to
  # the data of each individual batch.

  if (!is.na(t_poi[l_model_type[[2]]]) & t_poi[l_model_type[[2]]] == "dids") {
    if (sum(is.na(t_poi_dids)) == 0) {
      wc_batch_ich <- which.min(t_poi_dids)
    }
  }

  # Getting wc_icpt_ich in the original scale, if necessary
  if (!is.na(wc_batch_ich)) {
    if (xform[2] != "no") {
      wc_icpt_ich <- l_icpt[[l_model_type[[2]]]][["icpt.orig"]][wc_batch_ich]
    } else {
      wc_icpt_ich <- l_icpt[[l_model_type[[2]]]][["icpt"]][wc_batch_ich]
    }
  } else {
    wc_icpt_ich <- NA
  }

  # ---------
  # Back-transformation of POI values, if the time variable has been transformed
  if (xform[1] != "no") {
    switch(xform[1],
           "log" = {
             t_poi <- exp(t_poi) - shift[1]
           },
           "sqrt" = {
             t_poi <- t_poi^2 - shift[1]
           },
           "sq" = {
             t_poi <- sqrt(t_poi) - shift[1]
           })
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Putting parameters into a list

  prms <- list(alpha = alpha,
               alpha.pool = alpha_pool,
               ivl = ivl,
               ivl.type = ivl_type,
               ivl.side = ivl_side)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Putting results into a list

  structure(list(Data = d_dat,
                 Parameters = prms,
                 Variables = l_variables,
                 Model.Type = l_model_type,
                 Models = l_models,
                 AIC = t_AIC,
                 BIC = t_BIC,
                 wc.icpt = wc_icpt_ich,
                 wc.batch = wc_batch_ich,
                 Limits = l_lim,
                 Intercepts = l_icpt,
                 POI = t_poi),
            class = "expirest_osle")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Illustrating the ordinary shelf life estimate (osle)
#'
#' The function \code{print_expirest_osle()} makes a graphical display of the
#' shelf life estimate done by the \code{expirest_osle()} function.
#'
#' @param model An \sQuote{\code{expirest_osle}} object, i.e. a list returned
#'   by the \code{expirest_osle()} function.
#' @param show_grouping A character string specifying if the grouping of the
#'   data should be taken into account (\dQuote{yes}) or not (\dQuote{no}),
#'   i.e. if the results of the most appropriate model should be shown or
#'   the results from the marginal analysis ignoring the grouping (being
#'   equivalent with the \emph{common intercept / common slope case}). The
#'   default value is \code{yes}.
#' @param response_vbl_unit A character string specifying the unit associated
#'   with the response variable. The default is \code{NULL}.
#' @param y_range A numeric vector of the form \code{c(min, max)} specifying
#'   the range of the response variable to be plotted.
#' @param x_range A numeric vector of the form \code{c(min, max)} specifying
#'   the range of the time  variable to be plotted. The default is \code{NULL}
#'   and the \eqn{x} range is calculated automatically on the basis of the
#'   estimated shelf life.
#' @param plot_option A character string of either \code{full} or \code{lean},
#'   i.e. specifying if all the information should be put out on the plot
#'   (option \code{full}) or only basic information (option \code{lean}),
#'   i.e. the data points, the fitted regression line with the confidence
#'   interval, the specification limit(s) and the estimated shelf life
#'   limit(s). The default is \code{full}.
#' @param ci_app A character string of either \code{line} or \code{ribbon},
#'   specifying the appearance of the confidence interval, i.e. if the limits
#'   should be plotted as lines (option \code{line}) or as a shaded ribbon
#'   (option \code{ribbon}). The default is \code{line}.
#'
#' @details The function \code{plot.expirest_osle()} uses the data and the
#' information about the linear model that was used for the estimation of
#' the shelf life by aid of the \code{\link{expirest_osle}()} function. It
#' plots a graph of the time course of a parameter, a linear regression line
#' fitted to the data and the associated confidence or prediction interval.
#' In addition, it shows features of the shelf life estimation as proposed in
#' ICH Guidance Q1E.
#' It uses the \code{\link[ggplot2]{ggplot}()} function from the
#' \sQuote{\code{ggplot2}} package for plotting. The various arguments can be
#' used to control the appearance of the plot. The \sQuote{\code{ggplot2}}
#' object of the plot is contained in the \code{Graph} element of the list
#' that is returned by \code{plot.expirest_osle()} and can be used to modify
#' the appearance of the graph.
#'
#' @return A list with the following elements is returned invisibly:
#' \item{Model}{The \sQuote{\code{expirest_osle}} object that was passed via
#'   the \code{model} argument.}
#' \item{Expiry}{A data frame of type \code{expiry}.}
#' \item{Graph}{A \sQuote{\code{ggplot2}} object for the graphical display.}
#' \item{Prediction}{A data frame of the predicted values.}
#' \item{text}{A data frame of the text elements on the plot.}
#' \item{hlines}{A data frame of the horizontal line elements on the plot.}
#' \item{vlines}{A data frame of the vertical line elements on the plot.}
#'
#' @seealso \code{\link{expirest_osle}}, \code{\link{expirest_wisle}}.
#'
#' @example man/examples/examples_plot_expirest_osle.R
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
#' @importFrom ggplot2 geom_ribbon
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

plot_expirest_osle <- function(
  model, show_grouping = "yes", response_vbl_unit = NULL, y_range,
  x_range = NULL, plot_option = "full", ci_app = "line") {
  if (sum(names(model) %in% c("Data", "Parameters", "Model.Type", "Models",
                              "AIC", "BIC", "wc.icpt", "Limits", "POI")) != 9) {
    stop("The model must be an object of class expirest_osle.")
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
  if (!(plot_option %in% c("full", "lean"))) {
    stop("Please specify plot_option either as \"full\" or \"lean\".")
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

  # <-><-><->
  # Checking if estimation of POI.Model or Shelf.Life was successful

  t_exp <- expob[["POI"]]

  if (sum(is.na(t_exp)) > 0) {
    stop("Expiry determination was not successful.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extraction of data and parameters

  d_dat <- expob[["Data"]]

  alpha <- expob[["Parameters"]][["alpha"]]
  alpha_pool <- expob[["Parameters"]][["alpha.pool"]]
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

  if (sum(xform %in% "no") == 2) {
    response_vbl <- expob[["Model.Type"]][["response.vbl"]]
    time_vbl <- expob[["Model.Type"]][["time.vbl"]]
    batch_vbl <- expob[["Model.Type"]][["batch.vbl"]]
  }
  if (sum(xform %in% "no") == 0) {
    old_response_vbl <- expob[["Model.Type"]][["response.vbl.orig"]]
    response_vbl <- expob[["Model.Type"]][["response.vbl"]]
    old_time_vbl <- expob[["Model.Type"]][["time.vbl.orig"]]
    time_vbl <- expob[["Model.Type"]][["time.vbl"]]
    batch_vbl <- expob[["Model.Type"]][["batch.vbl"]]
  }
  if (sum(xform %in% "no") == 1) {
    if (xform[1] != "no") {
      response_vbl <- expob[["Model.Type"]][["response.vbl"]]
      old_time_vbl <- expob[["Model.Type"]][["time.vbl.orig"]]
      time_vbl <- expob[["Model.Type"]][["time.vbl"]]
      batch_vbl <- expob[["Model.Type"]][["batch.vbl"]]
    }
    if (xform[2] != "no") {
      old_response_vbl <- expob[["Model.Type"]][["response.vbl.orig"]]
      response_vbl <- expob[["Model.Type"]][["response.vbl"]]
      time_vbl <- expob[["Model.Type"]][["time.vbl"]]
      batch_vbl <- expob[["Model.Type"]][["batch.vbl"]]
    }
  }

  # Note: since the predict.lm() function from the 'stats' package always
  # calculates two-sided limits the value of alpha must be doubled in case that
  # the ivl_type is "one-sided".

  if (ivl_type == "one.sided") {
    alpha <- alpha * 2
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extraction of limits

  l_lim <- expob[["Limits"]]

  sl_orig <- l_lim[["sl.orig"]]
  sl <- l_lim[["sl"]]

  # POI with the upper or lower confidence or prediction interval of the
  # linear regression model
  # Most appropriate model
  poi_model <- t_exp[model_name]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Setting ranges and ticks

  # Find a sequence for a graph with 10 ticks displaying numbers divisible by
  # 3 and which contains 10 numbers between each tick, i.e. a sequence with a
  # maximal length of 100. The value of t_min is assumed to be the minimal
  # observed time_vbl value and t_max the maximal one.

  if (!is.null(x_range)) {
    t_min <- x_range[1]
    t_max <- x_range[2]
  } else {
    if (xform[1] == "no") {
      t_min <- pretty(d_dat[, time_vbl], n = 1)[1]
      t_max <- pretty(poi_model, n = 1)[2]
    } else {
      t_min <- pretty(d_dat[, old_time_vbl], n = 1)[1]
      t_max <- pretty(poi_model, n = 1)[2]
    }
  }

  # Setting the plotting area's x range and setting the ticks where the number
  # of ticks should be approximately 10 and they should be divisible by 3.
  x_tick_distance <- round(((t_max - t_min) / 10) / 3, 0) * 3
  if (x_tick_distance < 1) x_tick_distance <- 1
  x_range <- c(t_min, t_max)
  x_breaks <- seq(t_min, t_max, x_tick_distance)

  # Setting the y_breaks where the number of ticks should be 5.
  y_breaks <- pretty(y_range, 5)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prediction based on linear model

  # Generate the new x values (on the original scale) for prediction
  x_new <- seq(t_min, t_max, length.out = 100)

  # Transformation of new x values, if necessary
  switch(xform[1],
         "no" = {},
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
  m_pred <- predict(model, newdata = d_new, interval = ivl,
                    level = 1 - alpha)

  # Back-transformation of predicted (response) values, if necessary
  switch(xform[2],
         "no" = {},
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
  # LSL (lower right), USL (upper right), POI model (low at poi.model)

  if (length(sl) == 2) {
    d_text <- data.frame(
      Time = c(rep(x_range[2], 2), poi_model),
      Response = c(sl, sl[1]),
      Label = c(print_val("LSL: ", sl[1], rvu, sl_sf[1]),
                print_val("USL: ", sl[2], rvu, sl_sf[2]),
                print_val("", poi_model, "", get_n_whole_part(poi_model) + 1)),
      Colour = c("black", "black", "forestgreen"),
      stringsAsFactors = FALSE)

    d_text$Response <- d_text$Response +
      rep(diff(y_breaks[1:2]), 3) * 1 / c(-10, 10, -2)
  } else {
    switch(ivl_side,
           "lower" = {
             d_text <- data.frame(
               Time = c(x_range[2], poi_model),
               Response = rep(sl, 2),
               Label =
                 c(print_val("LSL: ", sl, rvu, sl_sf),
                   print_val("", poi_model, "",
                             get_n_whole_part(poi_model) + 1)),
               Colour = c("black", "forestgreen"),
               stringsAsFactors = FALSE)

             d_text$Response <- d_text$Response +
               rep(diff(y_breaks[1:2]), 2) * 1 / c(-10, -2)
           },
           "upper" = {
             d_text <- data.frame(
               Time = c(x_range[2], poi_model),
               Response = rep(sl, 2),
               Label =
                 c(print_val("USL: ", sl, rvu, sl_sf),
                   print_val("", poi_model, "",
                             get_n_whole_part(poi_model) + 1)),
               Colour = c("black", "forestgreen"),
               stringsAsFactors = FALSE)

             d_text$Response <- d_text$Response +
               rep(diff(y_breaks[1:2]), 2) * 1 / c(10, 2)
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
             d_hlines <- data.frame(Response = sl,
                                    Item = c("LSL"),
                                    Colour = as.character(c("black")),
                                    Type = as.character(c("dotted")),
                                    stringsAsFactors = FALSE)
           },
           "upper" = {
             d_hlines <- data.frame(Response = sl,
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

  d_vlines <- data.frame(Month = c(poi_model),
                         Item = c("poi.model"),
                         Colour = c("forestgreen"),
                         Type = c("dotdash"),
                         stringsAsFactors = FALSE)

  if (xform[1] != "no") {
    colnames(d_vlines) <- c(old_time_vbl, "Item", "Colour", "Type")
  } else {
    colnames(d_vlines) <- c(time_vbl, "Item", "Colour", "Type")
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
               geom_ribbon(data = d_pred,
                           aes_string(ymin = "LL", ymax = "UL"),
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
               geom_line(data = d_pred,
                         aes_string(x = time_vbl, y = "LL", colour = batch_vbl),
                         linetype = "solid", size = 0.5) +
               geom_line(data = d_pred,
                         aes_string(x = time_vbl, y = "UL", colour = batch_vbl),
                         linetype = "solid", size = 0.5)
           },
           "ribbon" = {
             ggraph <- ggraph +
               geom_ribbon(data = d_pred,
                           aes_string(ymin = "LL", ymax = "UL",
                                      fill = batch_vbl), alpha = 0.25)
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
    geom_vline(xintercept = d_vlines[, time_vbl],
               colour = d_vlines$Colour, linetype = d_vlines$Type) +
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

  if (plot_option == "full") {
    ggraph <- ggraph +
      geom_text(data = d_text,
                aes_string(x = time_vbl, y = response_vbl),
                label = d_text$Label, hjust = "right", size = 4,
                lineheight = 0.8, colour = d_text$Colour)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Collecting the results

  invisible(list(Model = model,
                 Expiry = t_exp,
                 Graph = ggraph,
                 Prediction = d_pred,
                 text = d_text,
                 hlines = d_hlines,
                 vlines = d_vlines))
}

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><>
