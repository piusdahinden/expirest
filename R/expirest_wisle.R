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
#' @param rl A numeric value or a numeric vector that specifies the release
#'   specification limit(s) for which the corresponding expiry should be
#'   estimated.
#' @param rl_sf A positive integer or a vector of positive integers that
#'   specifies the number of \dQuote{significant figures} (sf) of \code{rl}.
#'   It must have the same length as \code{rl}.
#' @param ivl_side A character string that specifies if the \dQuote{upper} or
#'   the \dQuote{lower} limit is the relevant limit, i.e. either \code{"upper"}
#'   or \code{"lower"}, respectively. The default is \code{"lower"}. Since this
#'   parameter additionally specifies the relationship of \code{rl} with
#'   \code{sl}, i.e. which of the two sides of \code{sl} the \code{rl} is
#'   compared to, only either either \code{"upper"} or \code{"lower"} is
#'   possible. In this respect, the usage of \code{ivl_side} differs from its
#'   usage in the \code{expirest_osle()} function where \code{ivl_side} in
#'   addition can be \code{"both"}.
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
#' \item{wc.icpt}{A data frame of the worst case intercepts of each of the
#'   four fitted models.}
#' \item{wc.batch}{A list of numeric value(s) of the worst case batch(es) per
#'   model type.}
#' \item{Limits}{A list of all limits.}
#' \item{POI}{A data frame of the intercepts, the differences between release
#'   and shelf life limits, the WCSLs, the expiry and release specification
#'   limits, the shelf lives and POI values.}
#'
#' Structure of the \code{POI} data frame:
#' \item{Intercept.cics}{The intercept of the worst case batch of the cics
#'   model.}
#' \item{Intercept.dics}{The intercept of the worst case batch of the dics
#'   model.}
#' \item{Intercept.dids.pmse}{The intercept of the worst case batch of the dids
#'   model with pooled mean square error (pmse).}
#' \item{Intercept.dids}{The intercept of the worst case batch of the dids
#'   model obtained by fitting individual models to the data of each batch.}
#' \item{Delta.cics}{Absolute difference between the release and and the shelf
#'   life specification of the cics model.}
#' \item{Delta.dics}{Absolute difference between the release and and the shelf
#'   life specification of the dics model.}
#' \item{Delta.dids.pmse}{Absolute difference between the release and and the
#'   shelf life specification of the dids model with pooled mean square error
#'   (pmse).}
#' \item{Delta.dids}{Absolute difference between the release and and the shelf
#'   life specification of the dids model obtained by fitting individual
#'   models to the data of each batch.}
#' \item{WCSL.cics}{WCSL of the cics model.}
#' \item{WCSL.dics}{WCSL of the dics model.}
#' \item{WCSL.dids.pmse}{WCSL of the dids model with pooled mean square error
#'   (pmse).}
#' \item{WCSL.dids}{WCSL of the dids model obtained by fitting individual
#'   models to the data of each batch.}
#' \item{Exp.Spec}{The (expiry) specification, i.e. the specification which is
#'   relevant for the determination of the expiry.}
#' \item{Rel.Spec}{The calculated release specification.}
#' \item{Shelf.Life.cics}{The estiamted shelf life of the cics model.}
#' \item{Shelf.Life.dics}{The estiamted shelf life of the dics model.}
#' \item{Shelf.Life.dids.pmse}{The estimated shelf life of the dids model with
#'   pooled mean square error (pmse).}
#' \item{Shelf.Life.dids}{The estimated shelf life of the dids model obtained
#'   by fitting individual models to the data of each batch.}
#' \item{POI.Model.cics}{The POI of the cics model.}
#' \item{POI.Model.dics}{The POI of the dics model.}
#' \item{POI.Model.dids.pmse}{The POI of the dids model with pooled mean
#'   square error (pmse).}
#' \item{POI.Model.dids}{The POI of the dids model obtained by fitting
#'   individual models to the data of each batch.}
#'
#' @references
#' Therapeutic Goods Administration (TGA) of the Department of Health of the
#' Australian Government, Australian Regulatory Guidelines for Prescription
#' Medicines (ARGPM), Stability testing for prescription medicines,
#' Version 1.1, March 2017
#'
#' International Council for Harmonisation of Technical Requirements for
#' Registration of Pharmaceuticals for Human (ICH), Harmonised Tripartite
#' Guideline, Evaluation of Stability Data Q1E, step 4, February 2003
#' (CPMP/ICH/420/02).
#'
#' @seealso \code{\link{expirest_osle}}, \code{\link[stats]{uniroot}},
#' \code{\link[stats]{lm}}, \code{\link[stats]{AIC}}, \code{\link[stats]{BIC}}.
#'
#' @example man/examples/examples_expirest_wisle.R
#'
#' @importFrom stats setNames
#'
#' @export

expirest_wisle <- function(data, response_vbl, time_vbl, batch_vbl, rl, rl_sf,
                           sl, sl_sf, srch_range, alpha = 0.05,
                           alpha_pool = 0.25, xform = c("no", "no"),
                           shift = c(0, 0), sf_option = "loose",
                           ivl = "confidence", ivl_type = "one.sided",
                           ivl_side = "lower", ...) {
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
  if (!is.numeric(rl_sf) && all(!is.na(rl_sf))) {
    stop("The parameter rl_sf must be a positive integer of the same length ",
         "as rl, or NA.")
  }
  if (sum(rl_sf < 0) > 0) {
    stop("The parameter rl_sf must be a positive integer of the same length ",
         "as rl, or NA.")
  }
  if (length(rl_sf) != length(rl)) {
    stop("The parameter rl_sf must be a positive integer of the same length ",
         "as rl, or NA.")
  }
  if (!isTRUE(all.equal(rl_sf, as.integer(rl_sf)))) {
    stop("The parameter rl_sf must be a positive integer of the same length ",
         "as rl, or NA.")
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
    if (ivl_side == "lower" && !all(rl > sl)) {
      stop("If ivl_side is \"lower\" rl must be > sl.")
    }
    if (ivl_side == "upper" && !all(rl < sl)) {
      stop("If ivl_side is \"upper\" rl must be < sl.")
    }
  } else {
    if (ivl_side == "lower" && !all(rl > sl[1])) {
      stop("If ivl_side is \"lower\" rl must be > sl.")
    }
    if (ivl_side == "upper" && !all(rl < sl[2])) {
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
                         rl = rl, rl_sf = rl_sf, ...)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Linearisation of data by variable transformation
  # Transformations:
  #   log: natural logarithm of the variable
  #   sqrt: square root of the variable variable
  #   sq: square of the variable
  #
  # Note: The log and sqrt transformations include adding the value defined by
  #       the shift parameter before performing the transformation.

  d_dat <- r_ret[["Data"]]

  if (sum(xform %in% "no") == 0) {
    time_vbl <- r_ret[["Variables"]][["time"]]
    response_vbl <- r_ret[["Variables"]][["response"]]
  }
  if (sum(xform %in% "no") == 1) {
    if (xform[1] != "no") {
      time_vbl <- r_ret[["Variables"]][["time"]]
    }
    if (xform[2] != "no") {
      response_vbl <- r_ret[["Variables"]][["response"]]
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determination of limits

  rel_lim <- get_relevant_limits(limits_list = r_ret[["Limits"]],
                                 xform = xform, ivl_side = ivl_side)

  sl <- rel_lim[["sl"]]
  rl <- rel_lim[["rl"]]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculation of POI values for all models (according to ARGPM)

  l_wisle <-
    get_wisle_poi_list(icpt_list = r_ret[["Intercepts"]],
                       model_list = r_ret[["Models"]], rl = rl, sl = sl,
                       srch_range = srch_range, alpha = alpha,  xform = xform,
                       shift = shift, ivl = ivl, ivl_type = ivl_type,
                       ivl_side = ivl_side, ...)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compilation of summary data frame

  l_ws <-
    compile_wisle_summary(data = d_dat, batch_vbl = batch_vbl, rl = rl,
                          poi_list = l_wisle[["all.poi"]],
                          icpt_list = r_ret[["Intercepts"]],
                          wcsl_list = l_wisle[["all.wcsl"]],
                          wcb_list = l_wisle[["which.wc.batch"]],
                          limits_list = rel_lim, poi_ich = r_ret[["POI"]],
                          xform = xform, shift = shift)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Putting results into a list

  structure(list(Data = r_ret[["Data"]],
                 Parameters = r_ret[["Parameters"]],
                 Variables = r_ret[["Variables"]],
                 Model.Type = r_ret[["Model.Type"]],
                 Models = r_ret[["Models"]],
                 AIC = r_ret[["AIC"]],
                 BIC = r_ret[["BIC"]],
                 wc.icpt = l_ws[["wc.icpt"]],
                 wc.batch = l_wisle[["which.wc.batch"]],
                 Limits = r_ret[["Limits"]],
                 POI = l_ws[["POI"]]),
            class = "expirest_wisle")
}

#' Illustrating the what-if (approach for) shelf life estimate (wisle)
#'
#' The function \code{plot_expirest_wisle()} makes a graphical display of the
#' shelf life estimate done by the \code{\link{expirest_wisle}()} function.
#'
#' @param model An \sQuote{\code{expirest_wisle}} object, i.e. a list returned
#'   by the \code{\link{expirest_wisle}()} function.
#' @param rl_index A positive integer that specifies which of the release limit
#'   values that have been handed over to \code{\link{expirest_wisle}()} should
#'   be displayed. The default value is \code{1}.
#' @param scenario A character string that specifies if the plot should be
#'   extended (with respect to the \eqn{x} axis) up to the \dQuote{standard
#'   scenario} (\code{"standard"}) or up to the \dQuote{worst case scenario}
#'   (\code{"worst"}). The default is \code{"standard"}.
#' @param plot_option A character string of either \code{"full"},
#'   \code{"lean1"}, \code{"lean2"}, \code{"basic1"} and \code{"basic2"}
#'   that specifies if additional information should be shown in the plot
#'   (option \code{"full"}) or only basic information (options \code{"lean"}
#'   and \code{"basic"}). Full means the data points, the fitted regression
#'   line with the confidence or prediction interval, the specification
#'   limit(s) and the estimated shelf life. The default is \code{"full"}.
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
#' @seealso \code{\link{expirest_wisle}}, \code{\link{expirest_osle}},
#' \code{\link{plot_expirest_osle}}.
#'
#' @example man/examples/examples_plot_expirest_wisle.R
#'
#' @importFrom stats lm
#' @importFrom stats as.formula
#' @importFrom stats coef
#' @importFrom stats predict
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

plot_expirest_wisle <- function(model, rl_index = 1, show_grouping = "yes",
                                response_vbl_unit = NULL, x_range = NULL,
                                y_range = NULL, scenario = "standard",
                                mtbs = "verified", plot_option = "full",
                                ci_app = "line") {
  if (!inherits(model, "expirest_wisle")) {
    stop("The model must be an object of class expirest_wisle.")
  }
  if (!is.numeric(rl_index) || length(rl_index) > 1) {
    stop("The parameter rl_index must be a positive integer of length 1.")
  }
  if (rl_index != as.integer(rl_index)) {
    stop("The parameter rl_index must be a positive integer of length 1.")
  }
  if (rl_index < 1 || rl_index > nrow(model[["POI"]])) {
    stop("The parameter rl_index must be between 1 and the number of rl ",
         "values.")
  }
  if (show_grouping == "no") {
    lifecycle::deprecate_warn(
      when = "0.1.7",
      what = "plot_expirest_wisle(show_grouping)",
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
  if (!(scenario %in% c("standard", "worst"))) {
    stop("Please specify scenario either as \"standard\" or \"worst\".")
  }
  if (!(mtbs %in% c("verified", "cics", "dics", "dids", "dids.pmse"))) {
    stop("Please specify mtbs either as \"verified\", \"cics\", \"dics\", ",
         "\"dids\" or \"dids.pmse\".")
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

  # Make visible binding for global variable
  LL <- UL <- NULL

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extraction of models and of the model type
  # If show_grouping = "no", the model_type is "cics"

  l_models <- expob[["Models"]]

  if (show_grouping == "yes") {
    model_name <- ifelse(mtbs == "verified",
                         expob[["Model.Type"]][["type.acronym"]],
                         mtbs)
  } else {
    model_name <- "cics"
  }

  # Most appropriate model based on the ANCOVA analysis, or as specified
  # via the mtbs parameter
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
  # linear regression model representing the worst case scenario (woca) case
  poi_model <- t_exp[rl_index, poi_model_name]
  poi_woca <- t_exp[rl_index, sl_model_name]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Setting x_range and ticks

  if (!is.null(x_range)) {
    x_min <- min(x_range)
  } else {
    if (xform[1] == "no") {
      x_min <- pretty(d_dat[, time_vbl], n = 1)[1]
    } else {
      x_min <- pretty(d_dat[, old_time_vbl], n = 1)[1]
    }
  }

  if (!is.null(x_range)) {
    x_max <- max(x_range)
  } else {
    switch(scenario,
           "standard" = {
             x_max <- pretty(poi_model, n = 1)[2]
           },
           "worst" = {
             x_max <- pretty(poi_woca, n = 1)[2]
           })
  }

  x_range <- c(x_min, x_max)

  if (plot_option == "full") {
    x_range[1] <- x_range[1] - x_range[2] / 5
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prediction based on linear model

  # Generate the new x values (on the original scale) for prediction
  x_new <- seq(x_min, x_max, length.out = 100)

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
    l_pred <- lapply(t_batches, function(x) {
      predict(l_models$dids[[x]],
              newdata = d_new[d_new[, batch_vbl] == x, ],
              interval = ivl, level = 1 - alpha)
    })
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
  # LSL (lower right), USL (upper right),
  # WCSL (left, at RL), Intercept (left, at intercept),
  # POI worst case (low at poi.woca), POI model (low at poi.model)
  # RL (lower right or upper right)

  d_text <-
    get_text_annotation(rvu = rvu, x_range = x_range, y_range = y_range,
                        sl = sl, sl_sf = sl_sf, poi_model = poi_model,
                        ivl_side = ivl_side, poi_woca = poi_woca,
                        wisle_est = t_exp, wc_icpt = wc_icpt, rl_sf = rl_sf,
                        rl_index = rl_index, wcsl_model_name = wcsl_model_name,
                        plot_option = plot_option)

  # <-><-><->
  # d_hlines - display of horizontal lines

  d_hlines <- get_hlines(sl, ivl_side)

  # <-><-><->
  # d_vlines - display of vertical lines

  d_vlines <- data.frame(Month = c(poi_woca, poi_model),
                         Item = c("poi.woca", "poi.model"),
                         Colour = c("forestgreen", "grey50"),
                         Type = c("dashed", "dotdash"),
                         stringsAsFactors = FALSE)

  # <-><-><->
  # d_seg - display of segments explaining the TGA method
  # The columns in data frame d_seg have the following meaning and position
  # (position in brackets):
  # Maximal allowed difference over time from intercept (horizontal),
  # Release Limit (horizontal),
  # Maximal allowed difference over time from SL (vertical),
  # Maximal allowed difference over time from intercept (vertical)
  # The elements of d_seg are not displayed if plot_option is "basic".

  d_seg <-
    get_segments(sl = sl, ivl_side = ivl_side, wisle_est = t_exp,
                 rl = rl, rl_index = rl_index, sl_model_name = sl_model_name,
                 poi_woca = poi_woca, wc_icpt = wc_icpt,
                 x_range = x_range, wcsl_model_name = wcsl_model_name)

  # <-><-><->
  # d_arr - display of arrow explaining the TGA method
  # The elements of d_arr are not displayed if plot_option is not "full".

  d_arr <- get_arrow(sl = sl, ivl_side = ivl_side, wisle_est = t_exp, rl = rl,
                     rl_index = rl_index, sl_model_name = sl_model_name,
                     wc_icpt = wc_icpt, x_range = x_range,
                     wcsl_model_name = wcsl_model_name)

  # <-><-><->
  # Renaming of columns for plotting on the original scale

  if (sum(xform %in% "no") == 2) {
    colnames(d_text) <- c(time_vbl, response_vbl, "Label", "Colour")
    colnames(d_hlines) <- c(response_vbl, "Item", "Colour", "Type")
    colnames(d_vlines) <- c(time_vbl, "Item", "Colour", "Type")
    colnames(d_seg) <- c(paste(time_vbl, 1:2, sep = "."),
                         paste(response_vbl, 1:2, sep = "."),
                         "Item", "Colour", "Type", "Size")
    colnames(d_arr) <- c(paste(time_vbl, 1:2, sep = "."),
                         paste(response_vbl, 1:2, sep = "."),
                         "Item", "Colour", "Line.Type", "Arrow.Type",
                         "Size", "Curvature", "Angle", "Length")
  }
  if (sum(xform %in% "no") == 0) {
    colnames(d_text) <- c(old_time_vbl, old_response_vbl, "Label", "Colour")
    colnames(d_hlines) <- c(old_response_vbl, "Item", "Colour", "Type")
    colnames(d_vlines) <- c(old_time_vbl, "Item", "Colour", "Type")
    colnames(d_seg) <- c(paste(old_time_vbl, 1:2, sep = "."),
                         paste(old_response_vbl, 1:2, sep = "."),
                         "Item", "Colour", "Type", "Size")
    colnames(d_arr) <- c(paste(old_time_vbl, 1:2, sep = "."),
                         paste(old_response_vbl, 1:2, sep = "."),
                         "Item", "Colour", "Line.Type", "Arrow.Type",
                         "Size", "Curvature", "Angle", "Length")
  }
  if (sum(xform %in% "no") == 1) {
    if (xform[1] != "no") {
      colnames(d_text) <- c(old_time_vbl, response_vbl, "Label", "Colour")
      colnames(d_vlines) <- c(old_time_vbl, "Item", "Colour", "Type")
      colnames(d_seg) <- c(paste(old_time_vbl, 1:2, sep = "."),
                           paste(response_vbl, 1:2, sep = "."),
                           "Item", "Colour", "Type", "Size")
      colnames(d_arr) <- c(paste(old_time_vbl, 1:2, sep = "."),
                           paste(response_vbl, 1:2, sep = "."),
                           "Item", "Colour", "Line.Type", "Arrow.Type",
                           "Size", "Curvature", "Angle", "Length")
    } else {
      colnames(d_vlines) <- c(time_vbl, "Item", "Colour", "Type")
    }
    if (xform[2] != "no") {
      colnames(d_text) <- c(time_vbl, old_response_vbl, "Label", "Colour")
      colnames(d_hlines) <- c(old_response_vbl, "Item", "Colour", "Type")
      colnames(d_seg) <- c(paste(time_vbl, 1:2, sep = "."),
                           paste(old_response_vbl, 1:2, sep = "."),
                           "Item", "Colour", "Type", "Size")
      colnames(d_arr) <- c(paste(time_vbl, 1:2, sep = "."),
                           paste(old_response_vbl, 1:2, sep = "."),
                           "Item", "Colour", "Line.Type", "Arrow.Type",
                           "Size", "Curvature", "Angle", "Length")
    } else {
      colnames(d_hlines) <- c(response_vbl, "Item", "Colour", "Type")
    }
  }

  # <-><-><->
  # Determination of items to be shown depending on plot_option

  # If plot_option is "full": plot the complete information.
  # If plot_option is "lean1" or "lean2":
  # - Plot LRL or URL, USL and / or LSL, worst case and standard scenario.
  # -  Do not plot vertical grey segments.
  # If plot_option is "basic1" or "basic2":
  # - Plot USL and / or LSL.
  # - Do not show segments (no need for show_seg at all)

  switch(plot_option,
         "full" = {
           show_text <- rep(TRUE, nrow(d_text))
           show_seg <- rep(TRUE, nrow(d_seg))
         },
         "lean1" = {
           show_text <- d_text$Colour %in% c("black", "forestgreen", "grey50")
           show_seg <- d_seg$Colour != "grey50"
         },
         "lean2" = {
           show_text <- d_text$Colour %in% c("black", "forestgreen", "grey50")
           show_seg <- d_seg$Colour != "grey50"
         },
         "basic1" = {
           show_text <- d_text$Colour %in% c("black")
         },
         "basic2" = {
           show_text <- rep(FALSE, nrow(d_text))
         })

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

  if (model_name == "cics") {
    ggraph <- ggplot(d_dat,
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
    ggraph <- ggplot(d_dat, aes(x = .data[[time_vbl]],
                                y = .data[[response_vbl]])) +
      geom_point(size = 2, aes(colour = .data[[batch_vbl]],
                               shape = .data[[batch_vbl]])) +
      geom_line(data = d_pred,
                aes(x = .data[[time_vbl]], y = .data[[response_vbl]],
                    colour = .data[[batch_vbl]]), linetype = "solid")

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
    coord_cartesian(xlim = x_range, ylim = y_range) +
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
                aes(x = .data[[time_vbl]], y = .data[[response_vbl]]),
                label = d_text[show_text, "Label"], hjust = "right",
                size = 4, lineheight = 0.8,
                colour = d_text[show_text, "Colour"])
  }

  if (plot_option %in% c("lean1", "lean2", "full")) {
    ggraph <- ggraph +
      geom_vline(xintercept = d_vlines[, time_vbl],
                 colour = d_vlines$Colour, linetype = d_vlines$Type) +
      geom_segment(data = d_seg[show_seg, ],
                   aes(x = .data[[paste(time_vbl, 1, sep = ".")]],
                       y = .data[[paste(response_vbl, 1, sep = ".")]],
                       xend = .data[[paste(time_vbl, 2, sep = ".")]],
                       yend = .data[[paste(response_vbl, 2, sep = ".")]]),
                   colour = d_seg$Colour[show_seg],
                   linetype = d_seg$Type[show_seg],
                   linewidth = d_seg$Size[show_seg])
  }

  if (plot_option == "full") {
    ggraph <- ggraph +
      geom_curve(data = d_arr,
                 aes(x = .data[[paste(time_vbl, 1, sep = ".")]],
                     y = .data[[paste(response_vbl, 1, sep = ".")]],
                     xend = .data[[paste(time_vbl, 2, sep = ".")]],
                     yend = .data[[paste(response_vbl, 2, sep = ".")]]),
                 colour = d_arr$Colour, linetype = d_arr$Line.Type,
                 linewidth = d_arr$Size, curvature = d_arr$Curvature,
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
