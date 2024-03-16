#' Ordinary shelf life estimation (osle)
#'
#' Based on a linear regression model fitted to a stability data set the
#' function \code{expirest_osle()} estimates the shelf life, or retest period,
#' following the ICH Q1E guideline. The abbreviation \dQuote{osle} stands for
#' \dQuote{ordinary shelf life estimation}.
#'
#' @param data A data frame with the columns specified by \code{response_vbl},
#'   \code{time_vbl} and \code{batch_vbl}.
#' @param response_vbl A character string that specifies the response variable
#'   name that must be a column of \code{data}.
#' @param time_vbl A character string that specifies the time variable name
#'   that must be a column of \code{data}.
#' @param batch_vbl A character string that specifies the column in \code{data}
#'   with the grouping information (i.e. a factorial variable) for the
#'   differentiation of the observations of the various batches.
#' @param sl A numeric value or a numeric vector of length \code{2} that
#'   specifies the specification limit or limits. If a vector is provided it
#'   must be of the form \code{c(lower limit, upper limit)}.
#' @param sl_sf A positive integer or a vector of positive integers that
#'   specifies the number of \dQuote{significant figures} (sf) of \code{sl}.
#'   It must have the same length as \code{sl}.
#' @param srch_range A vector of length \code{2} that specifies the end-points
#'   of the (time) range that is supposed to contain the shelf life or retest
#'   period.
#' @param alpha A numeric value between 0 and 1 that specifies the significance
#'   level for the calculation of confidence or prediction intervals. The
#'   default is \code{0.05}.
#' @param alpha_pool A numeric value between 0 and 1 that specifies the type I
#'   error rate for the test of the poolability of the batches. The default
#'   is \code{0.25}, following ICH Q1E guideline. The value should not be
#'   changed unless supported by well-founded reasons.
#' @param xform A vector of two character strings that specifies the
#'   transformation of the response and the time variable. The default is
#'   \dQuote{no} transformation, i.e. \code{c("no", "no")}, where the first
#'   element specifies the transformation of the \eqn{x} variable and the
#'   second element the transformation of the \eqn{y} variable. Valid
#'   alternatives for \eqn{x} and/or \eqn{y} variable transformation are
#'   \code{"log"} (natural logarithm), \code{"sqrt"} (square root) and
#'   \code{"sq"} (square).
#' @param shift A vector of two values which will be added to the variables
#'   \eqn{x} and/or \eqn{y} before they are transformed as specified by the
#'   \code{xform} parameter, where the first element will be added to the
#'   \eqn{x} variable and the second element to the \eqn{y} variable. The
#'   purpose is to prevent an undefined state which could arise when variables
#'   with values of \eqn{\leq 0} are log or square root transformed. The
#'   default is \code{c(0, 0)}.
#' @param sf_option A character string that specifies if the limits (\code{rl}
#'   or \code{sl}) should be regarded as \dQuote{tight} or \dQuote{loose}, i.e.
#'   either \code{"tight"} or \code{"loose"}, respectively. The default is
#'   \code{"tight"}. The option \code{"tight"} means that the limits are
#'   rounded to the number of significant digits specified by the parameters
#'   \code{rl_sf} and \code{sl_sf}. If \code{sf_option = "loose"}, four on
#'   the order of the last significant digit + 1 is added if
#'   \code{ivl_side = "upper"} or five on the order of the last significant
#'   digit + 1 is subtracted if \code{ivl_side = "upper"}.
#' @param ivl A character string of either \code{"confidence"} or
#'   \code{"prediction"} that specifies the type of interval of interest.
#'   The default is \code{"confidence"}.
#' @param ivl_type A character string that specifies if a \dQuote{one sided}
#'   or a \dQuote{two sided} interval should be calculated, i.e. either
#'   \code{"one.sided"} or \code{"two.sided"}, respectively. The default is
#'   \code{"one.sided"}.
#' @param ivl_side A character string that specifies if the specification
#'   limit, given that the limit has only one side, is an \dQuote{upper} or a
#'   \dQuote{lower} bound, i.e. it is specified as either \code{"upper"} or
#'   \code{"lower"}, respectively. The default is \code{"lower"}. If the
#'   specification has two boundaries, then this parameter specifies the
#'   preferred side. If no side is preferred over the other, \code{"both"} can
#'   be used.
#' @param ... Additional named or unnamed arguments passed on to
#'   \code{uniroot()}.
#'
#' @details According to ICH Q1E guideline, \dQuote{\emph{an appropriate
#' approach to retest period or shelf life estimation is to analyse a
#' quantitative attribute by determining the earliest time at which the 95
#' percent confidence limit for the mean intersects the proposed acceptance
#' criterion}} (in this package, this point is called the \dQuote{point of
#' intersection} (POI)). Furthermore, it says that \dQuote{\emph{for an
#' attribute known to increase with time, the upper one-sided 95 percent
#' confidence limit should be compared to the acceptance criterion. For an
#' attribute that can either increase or decrease, or whose direction of change
#' is not known, two-sided 95 percent confidence limits should be calculated
#' and compared to the upper and lower acceptance criteria.}} The approach
#' can be used to estimate the retest period or shelf life for a single batch
#' or for multiple batches. According to the guideline, \dQuote{\emph{for a
#' drug substance or for a drug product available in a single strength and a
#' single container size and/or fill, the retest period or shelf life is
#' generally estimated based on the stability data from a minimum of three
#' batches.}}
#'
#' Before performing the retest period or shelf life estimation with results
#' from multiple batches, the most suitable model should be determined. It
#' should particularly be verified if data of all test batches are poolable or
#' not. Details on this are described in section \dQuote{Checking batch
#' poolability} below.
#'
#' @section Checking batch poolability:
#' According to ICH Q1E guideline, construction of the 95\% confidence interval
#' on the basis of the combined data of all test batches is allowed only if it
#' has been confirmed by aid of a statistical test whether the regression lines
#' from the different batches have a common slope and a common intercept. A
#' significance level of \code{alpha_pool = 0.25} should to be used for both
#' batch-related terms, and the test of the slopes has to precede the test of
#' the intercepts. From these tests, three possible models may be appropriate,
#' i.e.
#' \itemize{
#'  \item a \emph{common intercept / common slope} model (cics),
#'  \item a \emph{different intercept / common slope} model (dics) or
#'  \item a \emph{different intercept / different slope} model (dids).
#' }
#' The \emph{common intercept / different slope} model is not of practical
#' relevance because the corresponding model is missing an effect. If the
#' slopes are significantly different, there is no point comparing intercepts.
#' The dids model has individual intercepts and individual slopes, and the
#' calculation of confidence intervals is based on the corresponding individual
#' mean square errors. The \emph{different intercept / different slope} model
#' where the mean square error is pooled across batches is reported as
#' dids.pmse.
#'
#' These requirements can be checked by aid of an \dQuote{ANalysis of
#' COVAriance} (ANCOVA) including the batch variable as main effect and as
#' \eqn{batch \times time} interaction term. The full ANCOVA model
#' simultaneously tests all the effects, and non-significant effects can be
#' identified and removed for fitting of the final regression model that is
#' used for the estimation of the shelf life or retest period.
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
#' containing the following elements:
#' \item{Data}{Data frame of the original data including new columns with
#'   transformed variables, if applicable.}
#' \item{Parameters}{A list of the parameters with the elements \code{alpha},
#'   \code{alpha.pool}, \code{ivl}, \code{ivl.type} and \code{ivl.side}.}
#' \item{Variables}{A list of the variable names, i.e. the original names of
#'   \code{batch_vbl}, \code{time_vbl} and \code{response_vbl} and, if
#'   applicable, of the transformed variables.}
#' \item{Model.Type}{A list of two elements that specifies which model, based
#'   on the ANCOVA analysis, suits best. The first element (\code{type.spec})
#'   is a numeric vector of length 2 that specifies the best model accepted at
#'   the significance level specified by \code{alpha.pool}. The first number
#'   represents the decision on the intercept and the second on the slope,
#'   where \code{1} stands for \dQuote{common} and \code{2} stands for
#'   \dQuote{different}. The second element (\code{type.acronym}) is an acronym
#'   representing the first item.}
#' \item{Models}{A list of four elements named \code{cics}, \code{dics},
#'   \code{dids.pmse} and \code{dids}. The first three elements contain the
#'   \sQuote{\code{lm}} objects of the \dQuote{common intercept / common slope}
#'   (\code{cics}), \dQuote{different intercept / common slope} (\code{dics})
#'   and \dQuote{different intercept / different slope} (\code{dids}) models.
#'   The fourth element is a list of the \sQuote{\code{lm}} objects that is
#'   obtained from fitting a regression model to the data of each level of the
#'   categorical variable separately. The \code{cics}, \code{dics} and
#'   \code{dids.pmse} elements are \code{NA} if data of only a single batch
#'   is available.}
#' \item{AIC}{A numeric named vector of the Akaike Information Criterion (AIC)
#'   values of the \code{cics}, \code{dics} and \code{dids.pmse} models.}
#' \item{BIC}{A numeric named vector of the Bayesian Information Criterion (BIC)
#'   values of each of the \code{cics}, \code{dics} and \code{dids.pmse}
#'   models.}
#' \item{wc.icpt}{A numeric named vector of the worst case intercepts. The
#'   information about which limit the corresponding confidence interval
#'   crosses is stored in the attribute named \code{side}.}
#' \item{wc.batch}{A numeric named vector of the batches with the worst case
#'   intercepts. The information about which limit the corresponding confidence
#'   interval crosses is stored in the attribute named \code{side}.}
#' \item{Limits}{A list of all limits.}
#' \item{Intercepts}{A list of the intercepts of all models.}
#' \item{All.POI}{A list of two elements named \code{lower} and \code{upper}
#'   that contain either NA or lists of the POI values of all models.}
#' \item{POI}{A numeric named vector of the POI values of the worst case
#'   batches of each model. The information about which limit the corresponding
#'   confidence interval crosses is stored in the attribute named \code{side}.}
#'
#' @references
#' International Council for Harmonisation of Technical Requirements for
#' Registration of Pharmaceuticals for Human (ICH), Harmonised Tripartite
#' Guideline, Evaluation of Stability Data Q1E, step 4, February 2003
#' (CPMP/ICH/420/02).
#'
#' @seealso \code{\link{expirest_wisle}}, \code{\link[stats]{uniroot}},
#' \code{\link[stats]{lm}}, \code{\link[stats]{AIC}}, \code{\link[stats]{BIC}}.
#'
#' @example man/examples/examples_expirest_osle.R
#'
#' @importFrom stats setNames
#'
#' @export

expirest_osle <- function(data, response_vbl, time_vbl, batch_vbl, sl, sl_sf,
                          srch_range, alpha = 0.05, alpha_pool = 0.25,
                          xform = c("no", "no"), shift = c(0, 0),
                          sf_option = "tight", ivl = "confidence",
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
  if (!is.numeric(sl) || length(sl) > 2) {
    stop("The parameter sl must be a numeric or vector of length 1 or 2.")
  }
  if (length(sl) == 2) {
    if (sl[2] < sl[1]) {
      stop("The parameter sl must be of the form c(lower, upper).")
    }
  }
  if (!is.numeric(sl_sf) && all(!is.na(sl_sf))) {
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
  if (!is.numeric(srch_range) || length(srch_range) != 2) {
    stop("The parameter srch_range must be a vector of length 2.")
  }
  if (alpha <= 0 || alpha > 1) {
    stop("Please specify alpha as (0, 1].")
  }
  if (alpha_pool <= 0 || alpha_pool > 1) {
    stop("Please specify alpha_pool as (0, 1].")
  }
  if (length(xform) != 2) {
    stop("Please specify xform appropriately.")
  }
  if (!(xform[1] %in% c("no", "log", "sqrt", "sq")) ||
      !(xform[2] %in% c("no", "log", "sqrt", "sq"))) {
    stop("Please specify xform appropriately.")
  }
  if (!is.numeric(shift) || length(shift) != 2) {
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
  if (!(ivl_side %in% c("lower", "upper", "both"))) {
    stop("Please specify ivl_side either as \"lower\", \"upper\" or \"both\".")
  }
  if (ivl_side == "both" && length(sl) == 1) {
    stop("Since ivl_side = \"both\", a specification with two sides is ",
         "expected. Only one side has been specified, though, i.e. ",
         "sl = ", sl, ".\nPlease provide a specification with two sides.")
  }
  # Check that if ivl_side is "both" then ivl_type must be "two.sided"
  if (ivl_side == "both" && ivl_type != "two.sided") {
    warning("Since ivl_side is specified as \"both\" the parameter ivl_type ",
            "has been set to \"two.sided\".")
  }

  if (length(sl) == 2) {
    if (ivl_side == "lower" && sum(data[, response_vbl] < sl[1]) > 0) {
      warning("You specified ivl_side = \"lower\". But ",
              round(100 / nrow(data) * sum(data[, response_vbl] < sl[1]), 1),
              "% of the response values are < sl[1]. ",
              "Are you sure that you did not want to set ivl_side = \"upper\"?")
    }
    if (ivl_side == "upper" && sum(data[, response_vbl] > sl[2]) > 0) {
      warning("You specified ivl_side = \"upper\". But ",
              round(100 / nrow(data) * sum(data[, response_vbl] > sl[2]), 1),
              "% of the response values are > sl[2]. ",
              "Are you sure that you did not want to set ivl_side = \"lower\"?")
    }
  } else {
    if (ivl_side == "lower" && sum(data[, response_vbl] < sl) > 0) {
      warning("You specified ivl_side = \"lower\". But ",
              round(100 / nrow(data) * sum(data[, response_vbl] < sl), 1),
              "% of the response values are < sl. ",
              "Are you sure that you did not want to set ivl_side = \"upper\"?")
    }
    if (ivl_side == "upper" && sum(data[, response_vbl] > sl) > 0) {
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

  if (mrl != 0 && ivl_side == "both") {
    stop("For the assessment of release limits (rl), ivl_side must be ",
         "either \"lower\" or \"upper\".\n",
         "expirest_osle() was called with ivl_side = \"both\".")
  }

  # Although release limits are not relevant for the osle approach. However,
  # since expirest_wisle() calls expirest_osle(), this function must be able
  # to hand down the information and return the results appropriately.
  if (mrl == 0) {
    rl <- NA
  } else {
    rl <- mf[[mrl]]
  }
  if (mrlsf == 0) {
    rl_sf <- NA
  } else {
    rl_sf <- mf[[mrlsf]]
  }

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
    get_xformed_variables(data = data, response_vbl = response_vbl,
                          time_vbl = time_vbl, xform = xform, shift = shift)

  l_variables <-
    get_variable_list(response_vbl = response_vbl, time_vbl = time_vbl,
                      batch_vbl = batch_vbl, xform = xform)

  response_vbl <- l_variables[["response"]]
  time_vbl <- l_variables[["time"]]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Fitting of all possible models that are relevant

  l_models <- get_model_list(data = d_dat, response_vbl = response_vbl,
                             time_vbl = time_vbl, batch_vbl = batch_vbl)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determination and selection of limits

  l_lim <- set_limits(rl = rl, rl_sf = rl_sf, sl = sl, sl_sf = sl_sf,
                      sf_option = sf_option, xform = xform, shift = shift,
                      ivl_side = ivl_side)

  sl <- get_relevant_limits(limits_list = l_lim,
                            xform = xform, ivl_side = ivl_side)$sl

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determination of intercepts of all models

  l_icpt <- get_icpt_list(data = d_dat, response_vbl = response_vbl,
                          time_vbl = time_vbl, batch_vbl = batch_vbl,
                          model_list = l_models[["Models"]], xform = xform,
                          shift = shift)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determination of POI values of all models

  l_osle <- get_osle_poi_list(data = d_dat, batch_vbl = batch_vbl,
                              icpt_list = l_icpt,
                              model_list = l_models[["Models"]],
                              sl = sl, srch_range = srch_range, alpha = alpha,
                              xform = xform, shift = shift, ivl = ivl,
                              ivl_type = ivl_type, ivl_side = ivl_side, ...)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Back-transformation of POI values, if necessary

  t_poi <- l_osle[["poi"]]

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
  # Putting results into a list

  structure(list(Data = d_dat,
                 Parameters = list(alpha = alpha,
                                   alpha.pool = alpha_pool,
                                   ivl = ivl,
                                   ivl.type = ivl_type,
                                   ivl.side = ivl_side),
                 Variables = l_variables,
                 Model.Type =
                   check_ancova(data = d_dat, response_vbl = response_vbl,
                                time_vbl = time_vbl, batch_vbl = batch_vbl,
                                alpha = alpha_pool),
                 Models = l_models[["Models"]],
                 AIC = l_models[["AIC"]],
                 BIC = l_models[["BIC"]],
                 wc.icpt = l_osle[["wc.icpt"]],
                 wc.batch = l_osle[["which.wc.batch"]],
                 Limits = l_lim,
                 Intercepts = l_icpt,
                 All.POI = l_osle[["all.poi"]],
                 POI = t_poi),
            class = "expirest_osle")
}

#' Illustrating the ordinary shelf life estimate (osle)
#'
#' The function \code{plot_expirest_osle()} makes a graphical display of the
#' shelf life estimate done by the \code{\link{expirest_osle}()} function.
#'
#' @param model An \sQuote{\code{expirest_osle}} object, i.e. a list returned
#'   by the \code{\link{expirest_osle}()} function.
#' @param show_grouping `r lifecycle::badge("deprecated")`
#'   `show_grouping = \"yes\" or \"no\"` is no longer supported. Use the
#'   \code{mtbs} parameter instead which allows choosing a specific model,
#'   i.e. also the \emph{common intercept / common slope case} model which
#'   was the default model when \code{show_grouping} was \code{"no"}.
#' @param response_vbl_unit A character string that specifies the unit
#'   associated with the response variable. The default is \code{NULL}.
#' @param x_range A numeric vector of the form \code{c(min, max)} that
#'   specifies the range of the time variable to be plotted. The default is
#'   \code{NULL} and the \eqn{x} range is calculated automatically on the basis
#'   of the estimated shelf life.
#' @param y_range A numeric vector of the form \code{c(min, max)} that
#'   specifies the range of the response variable to be plotted. The default
#'   is \code{NULL} and the \eqn{y} range is calculated automatically on the
#'   basis of the time course of the response.
#' @param mtbs A character string that specifies the \dQuote{model to be shown},
#'   i.e. either \code{verified}, which is the default, or one of \code{cics},
#'   \code{dics}, \code{dids} or \code{dids.pmse}. The \code{verified} model
#'   is the model that was identified through the poolability check. It is
#'   thus also one of the possible optional models. The \code{dids} model
#'   represents the case where a separate model is fitted to the data of each
#'   individual batch while the \code{dids.pmse} model is the interaction
#'   model which includes the \eqn{batch} variable as main effect and in the
#'   interaction term with the \eqn{time} variable (\eqn{batch \times time}),
#'   i.e. a model where the mean square error is pooled across batches.
#' @param plot_option A character string of either \code{"full"} or
#'   \code{"lean"} that specifies if additional information should be put out
#'   on the plot (option \code{"full"}) or only basic information (option
#'   \code{"lean"}), i.e. the data points, the fitted regression line with
#'   the confidence interval, the specification limit(s) and the estimated
#'   shelf life limit(s). The default is \code{"full"}.
#' @param ci_app A character string of either \code{"line"} or \code{"ribbon"},
#'   that specifies the appearance of the confidence interval, i.e. if the
#'   limits should be plotted as lines (option \code{"line"}) or as a shaded
#'   ribbon (option \code{"ribbon"}). The default is \code{"line"}.
#'
#' @details The function \code{plot_expirest_osle()} uses the data and the
#' information about the linear model that was used for the estimation of
#' the shelf life by aid of the \code{\link{expirest_osle}()} function. It
#' plots a graph of the time course of a parameter, a linear regression line
#' fitted to the data and the associated confidence or prediction interval.
#' In addition, it shows features of the shelf life estimation.
#'
#' For plotting, the \code{\link[ggplot2]{ggplot}()} function from the
#' \sQuote{\code{ggplot2}} package is used. The various arguments can be
#' used to control the appearance of the plot. The \sQuote{\code{ggplot2}}
#' object of the generated plot is contained in the \code{Graph} element of
#' the list that is returned by \code{\link{plot_expirest_osle}()} and can be
#' used to modify the appearance of the graph.
#'
#' @return An object of class \sQuote{\code{plot_expirest_osle}} is returned
#' invisibly consisting of the following elements:
#' \item{Model}{The \sQuote{\code{expirest_osle}} object that was passed via
#'   the \code{model} argument.}
#' \item{Expiry}{A data frame of type \code{expiry}.}
#' \item{Graph}{A \sQuote{\code{ggplot2}} object for the graphical display.}
#' \item{Prediction}{A data frame of the predicted values.}
#' \item{text}{A data frame of the text elements on the plot.}
#' \item{hlines}{A data frame of the horizontal line elements on the plot.}
#' \item{vlines}{A data frame of the vertical line elements on the plot.}
#'
#' @seealso \code{\link{expirest_osle}}, \code{\link{expirest_wisle}},
#' \code{\link{plot_expirest_wisle}}.
#'
#' @example man/examples/examples_plot_expirest_osle.R
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
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
#' @importFrom ggplot2 coord_cartesian
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 unit
#' @importFrom lifecycle badge
#' @importFrom lifecycle deprecate_warn
#'
#' @export

plot_expirest_osle <- function(model, show_grouping = "yes",
                               response_vbl_unit = NULL, x_range = NULL,
                               y_range = NULL, mtbs = "verified",
                               plot_option = "full", ci_app = "line") {
  if (!inherits(model, "expirest_osle")) {
    stop("The model must be an object of class expirest_osle.")
  }
  if (show_grouping == "no") {
    lifecycle::deprecate_warn(
      when = "0.1.7",
      what = "plot_expirest_osle(show_grouping)",
      with = "plot_expirest_osle(mtbs)",
      details =
        c("If you do not want to see the grouping, use mtbs = \"cics\". ",
          "If you have set show_grouping = \"no\", mtbs is set to \"cics\".",
          "If you have set show_grouping = \"yes\", the settings in mtbs
          apply."))
  }
  if (!is.null(x_range)) {
    if (!is.numeric(x_range) || length(x_range) != 2) {
      stop("The parameter x_range must be a vector of length 2.")
    }
  }
  if (!is.null(y_range)) {
    if (!is.numeric(y_range) || length(y_range) != 2) {
      stop("The parameter y_range must be a vector of length 2.")
    }
  }
  if (!(mtbs %in% c("verified", "cics", "dics", "dids", "dids.pmse"))) {
    stop("Please specify mtbs either as \"verified\", \"cics\", \"dics\", ",
         "\"dids\" or \"dids.pmse\".")
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

  d_dat <- expob[["Data"]]
  xform <- expob[["Limits"]][["xform"]]

  # Make visible binding for global variable
  LL <- UL <- NULL

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extraction of models and of the model type
  # If show_grouping = "no", the model_type is "cics"

  if (show_grouping == "yes") {
    model_name <- ifelse(mtbs == "verified",
                   expob[["Model.Type"]][["type.acronym"]],
                   mtbs)
  } else {
    model_name <- "cics"
    mtbs <- "cics"
  }

  # Set model_name to dids if it is n.a.
  model_name <- ifelse(model_name == "n.a.", "dids", model_name)

  # Checking if estimation of POI.Model or Shelf.Life was successful
  t_exp <- expob[["POI"]]

  if (any(is.na(t_exp))) {
    stop("Expiry determination was not successful.")
  }

  # POI with the upper or lower confidence or prediction interval of the
  # most appropriate model
  poi_model <- t_exp[model_name]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Setting x_range and ticks
  # For the setting of the y_range, a prediction must be done first

  if (!is.null(x_range)) {
    x_min <- min(x_range)
    x_max <- max(x_range)
  } else {
    if (xform[1] == "no") {
      x_min <- pretty(d_dat[, expob[["Variables"]][["time"]]], n = 1)[1]
      x_max <- pretty(poi_model, n = 1)[2]
    } else {
      x_min <- pretty(d_dat[, expob[["Variables"]][["time.orig"]]], n = 1)[1]
      x_max <- pretty(poi_model, n = 1)[2]
    }
  }

  x_range <- c(x_min, x_max)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prediction based on linear model

  d_pred <- get_predictions(model = expob, model_name = model_name,
                            x_range = x_range)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Setting y_range and ticks

  if (!is.null(y_range)) {
    y_min <- min(y_range)
    y_max <- max(y_range)
  } else {
    tmp <- pretty(c(d_pred$LL, d_pred$UL), n = 1)
    y_min <- tmp[1]
    y_max <- tmp[length(tmp)]
  }

  y_range <- c(y_min, y_max)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Generation of ancillary data frames for plotting

  # <-><-><->
  # d_text - display of text elements
  # The rows in data frame d_text have the following meaning and position
  # (position in brackets):
  # LSL (lower right), USL (upper right), POI model (low at poi.model)

  d_text <- get_text_annotation(model = expob, rvu = rvu, x_range = x_range,
                                y_range = y_range, mtbs = mtbs,
                                plot_option = plot_option)

  # <-><-><->
  # d_hlines - display of horizontal lines

  d_hlines <- get_hlines(model = expob)

  # <-><-><->
  # d_vlines - display of vertical lines

  d_vlines <- get_vlines(model = expob, mtbs = mtbs)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Generation of ggplot object

  # Setting variable names
  time_vbl <- expob[["Variables"]][["time"]]
  response_vbl <- expob[["Variables"]][["response"]]
  batch_vbl <- expob[["Variables"]][["batch"]]

  if (sum(xform %in% "no") == 0) {
    time_vbl <- expob[["Variables"]][["time.orig"]]
    response_vbl <- expob[["Variables"]][["response.orig"]]
  }
  if (sum(xform %in% "no") == 1) {
    if (xform[1] != "no") {
      time_vbl <- expob[["Variables"]][["time.orig"]]
    }
    if (xform[2] != "no") {
      response_vbl <- expob[["Variables"]][["response.orig"]]
    }
  }

  if (model_name == "cics") {
    ggraph <-
      ggplot(d_dat,
             aes(x = .data[[time_vbl]], y = .data[[response_vbl]])) +
      geom_point(size = 2, shape = 1) +
      geom_line(data = d_pred,
                aes(x = .data[[time_vbl]], y = .data[[response_vbl]]),
                colour = "royalblue", linetype = "solid")

    switch(ci_app,
           "line" = {
             ggraph <- ggraph +
               geom_line(data = d_pred, aes(x = .data[[time_vbl]], y = LL),
                         colour = "royalblue", linetype = "solid",
                         linewidth = 0.5) +
               geom_line(data = d_pred, aes(x = .data[[time_vbl]], y = UL),
                         colour = "royalblue", linetype = "solid",
                         linewidth = 0.5)
           },
           "ribbon" = {
             ggraph <- ggraph +
               geom_ribbon(data = d_pred, aes(ymin = LL, ymax = UL),
                           fill = "royalblue", alpha = 0.25)
           })

    ggraph <- ggraph + theme(legend.position = "none")
  } else {
    ggraph <-
      ggplot(d_dat,
             aes(x = .data[[time_vbl]], y = .data[[response_vbl]])) +
      geom_point(aes(colour = .data[[batch_vbl]],
                     shape = .data[[batch_vbl]]), size = 2) +
      geom_line(data = d_pred,
                aes(x = .data[[time_vbl]], y = .data[[response_vbl]],
                    colour = .data[[batch_vbl]]),
                linetype = "solid")

    switch(ci_app,
           "line" = {
             ggraph <- ggraph +
               geom_line(data = d_pred,
                         aes(x = .data[[time_vbl]], y = LL,
                             colour = .data[[batch_vbl]]),
                         linetype = "solid", linewidth = 0.5) +
               geom_line(data = d_pred,
                         aes(x = .data[[time_vbl]], y = UL,
                             colour = .data[[batch_vbl]]),
                         linetype = "solid", linewidth = 0.5)
           },
           "ribbon" = {
             ggraph <- ggraph +
               geom_ribbon(data = d_pred,
                           aes(ymin = LL, ymax = UL,
                               fill = .data[[batch_vbl]]), alpha = 0.25)
           })

    ggraph <- ggraph + theme(legend.position.inside = c(0.04, 0.96),
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
    coord_cartesian(xlim = x_range, ylim = y_range) +
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
                aes(x = .data[[time_vbl]], y = .data[[response_vbl]]),
                label = d_text$Label, hjust = "right", size = 4,
                lineheight = 0.8, colour = d_text$Colour)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Collecting the results

  invisible(structure(list(Model = expob[["Models"]][[model_name]],
                           Expiry = t_exp,
                           Graph = ggraph,
                           Prediction = d_pred,
                           text = d_text,
                           hlines = d_hlines,
                           vlines = d_vlines),
                      class = "plot_expirest_osle"))
}
