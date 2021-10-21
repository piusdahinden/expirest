#' Confidence or prediction interval limit
#'
#' The function \code{get_intvl_limit()} calculates the upper or lower
#' confidence or prediction interval limit(s) at a given value of \eqn{x}.
#'
#' @param x_new A numeric value of \eqn{x} at which the upper or lower
#'   confidence or prediction interval limit(s) should be calculated.
#' @param model A linear model object of type \sQuote{\code{lm}}.
#' @inheritParams expirest_osle
#'
#' @details The function \code{get_intvl_limit()} calculates the upper or lower
#' confidence or prediction interval limit(s) for the linear regression model
#' provided by \code{model}.
#'
#' @return A numeric value or, if the model contained a categorical variable,
#' a numeric vector of the predicted upper or lower confidence or prediction
#' interval limit(s) at a given value of \eqn{x}.
#'
#' @seealso \code{\link[stats]{predict.lm}}.
#'
#' @keywords internal

get_intvl_limit <- function(x_new, model, alpha = 0.05, ivl = "confidence",
                         ivl_type = "one.sided", ivl_side = "lower") {
  if (!is.numeric(x_new)) {
    stop("x_new must be a numeric value.")
  }
  if (class(model) != "lm") {
    stop("Please provide a model of type \"lm\".")
  }
  if (alpha <= 0 | alpha > 1) {
    stop("Please specify alpha as (0, 1].")
  }
  if (!(ivl %in% c("confidence", "prediction", "fitted.line"))) {
    stop("Please specify ivl either as \"confidence\" or \"prediction\" or ",
         "\"fitted.line\".")
  }
  if (!(ivl_type %in% c("one.sided", "two.sided"))) {
    stop("Please specify ivl_type either as \"one.sided\" or \"two.sided\".")
  }
  if (!(ivl_side %in% c("lower", "upper"))) {
    stop("Please specify ivl_side either as \"lower\" or \"upper\".")
  }

  # If x_new is NA, terminate with NA
  if (is.na(x_new)) {
    return(NA)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Note: since the predict.lm() function from the 'stats' package always
  # calculates two-sided limits the value of alpha must be doubled in case that
  # the ivl_type is "one-sided".

  if (ivl_type == "one.sided") {
    alpha <- alpha * 2
  }

  variable_names <- names(attr(model$terms, which = "dataClasses"))
  is_factor <- sum(attr(model$terms, which = "dataClasses") %in% "factor")

  if (is_factor == 0 & length(variable_names) == 2) {
    l_newdata <- list(x_new)
    names(l_newdata) <- variable_names[2]

    if (ivl == "fitted.line") {
      res <- do.call("predict",
                     list(object = model, newdata = l_newdata,
                          level = 1 - alpha, interval = "confidence"))[, "fit"]
    } else {
      switch(ivl_side,
             "lower" = {
               res <- do.call("predict",
                              list(object = model, newdata = l_newdata,
                                   level = 1 - alpha,
                                   interval = ivl))[, "lwr"]
             },
             "upper" = {
               res <- do.call("predict",
                              list(object = model, newdata = l_newdata,
                                   level = 1 - alpha,
                                   interval = ivl))[, "upr"]
             })
    }
  } else {
    grouping_variables <-
      variable_names[attr(model$terms, which = "dataClasses") %in% "factor"]

    x_length <- vapply(grouping_variables,
                       function(x)
                         nlevels(model$model[, grouping_variables]),
                       numeric(1))
    x_length <- prod(x_length)

    l_newdata <-
      vapply(grouping_variables,
             function(x)
               list(rep(levels(model$model[, grouping_variables]),
                        each = x_length /
                          nlevels(model$model[, grouping_variables]) *
                          length(x_new))),
             list(1))
    l_newdata[[length(l_newdata) + 1]] <- rep(x_new, x_length)
    names(l_newdata) <- c(grouping_variables,
                          variable_names[!(variable_names %in%
                                             grouping_variables)][-1])

    if (ivl == "fitted.line") {
      res <- do.call("predict",
                     list(object = model, newdata = l_newdata,
                          level = 1 - alpha, interval = "confidence"))[, "fit"]
    } else {
      switch(ivl_side,
             "lower" = {
               res <- do.call("predict",
                              list(object = model, newdata = l_newdata,
                                   level = 1 - alpha,
                                   interval = ivl))[, "lwr"]
             },
             "upper" = {
               res <- do.call("predict",
                              list(object = model, newdata = l_newdata,
                                   level = 1 - alpha,
                                   interval = ivl))[, "upr"]
             })
    }
  }

  return(res)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Determine distance of lines
#'
#' The function \code{get_distance()} calculates the distance between two
#' lines at a given value of \eqn{x}.
#'
#' @param x_new A numeric value of \eqn{x} for which the distance between two
#'   lines is sought, e.g. the distance of upper or lower confidence/prediction
#'   interval limits from the upper or lower specification or expiry limits,
#'   respectively.
#' @param model A linear model object of type \sQuote{\code{lm}}.
#' @param sl A numeric variable specifying the \dQuote{specification limit} (SL)
#'   or, for determinations according to ARGPM guidance \dQuote{Stability
#'   testing for prescription medicines}, the \dQuote{expiry limit} (EL). The
#'   EL is defined as the intercept \eqn{\pm} the difference between the
#'   specification limit and the release limit (RL). If it is the upper limit
#'   which is the relevant limit, it is added (\code{+}) to the intercept,
#'   otherwise it is subtracted (\code{-}) from the intercept.
#' @inheritParams expirest_osle
#'
#' @details The function \code{find_get_distance()} estimates the distance
#' between the upper or lower confidence or prediction interval and the upper
#' or lower acceptance criterion (i.e. the specification or the expiry limit)
#' at a certain value of \code{x_new}. The confidence or prediction interval
#' is calculated for the linear regression model provided by \code{model}.
#' Recommendations on how to estimate shelf life or expiry can be found in the
#' corresponding section below.
#'
#' @section How to estimate shelf life or expiry:
#' ICH Q1E recommends that \dQuote{\emph{For an attribute known to decrease with
#' time, the lower one-sided 95 percent confidence limit should be compared
#' to the acceptance criterion. For an attribute known to increase with time,
#' the upper one-sided 95 percent confidence limit should be compared to the
#' acceptance criterion. For an attribute that can either increase or decrease,
#' or whose direction of change is not known, two-sided 95 percent confidence
#' limits should be calculated and compared to the upper and lower acceptance
#' criteria.}} Since attributes often either decrease or increase, the default
#' for \code{ivl_type} is \code{one.sided}.
#'
#' According to the ARGPM guidance \dQuote{Stability testing for prescription
#' medicines}, the shelf life or expiry limit is estimated as the point where
#' the upper or lower limit of the 95\% confidence interval of the linear model
#' fitted to the data intersects the worst case scenario limit. The worst case
#' scenario limit is obtained by adding/subtracting the absolute difference of
#' specification limit and release limit to/from the common intercept of the
#' test batches or the intercept of the worst performing batch.
#'
#' @return A numeric value representing the distance of the respective lines
#' is returned.
#'
#' @inherit expirest_wisle references
#'
#' @seealso \code{\link{find_poi}}, \code{\link{expirest_osle}},
#' \code{\link{expirest_wisle}}.
#'
#' @keywords internal

get_distance <- function(x_new, model, sl, alpha = 0.05, ivl = "confidence",
                          ivl_type = "one.sided", ivl_side = "lower") {
  if (!is.numeric(x_new)) {
    stop("x_new must be a numeric value.")
  }
  if (class(model) != "lm") {
    stop("Please provide a model of type \"lm\".")
  }
  if (!is.numeric(sl) | length(sl) > 1) {
    stop("sl must be a numeric value of length 1.")
  }
  if (alpha <= 0 | alpha > 1) {
    stop("Please specify alpha as (0, 1].")
  }
  if (!(ivl %in% c("confidence", "prediction", "fitted.line"))) {
    stop("Please specify ivl either as \"confidence\" or \"prediction\" or ",
         "\"fitted.line\".")
  }
  if (!(ivl_type %in% c("one.sided", "two.sided"))) {
    stop("Please specify ivl_type either as \"one.sided\" or \"two.sided\".")
  }
  if (!(ivl_side %in% c("lower", "upper"))) {
    stop("Please specify ivl_side either as \"lower\" or \"upper\".")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  pred_lim <-
    get_intvl_limit(x_new = x_new, model = model, alpha = alpha,
                    ivl = ivl, ivl_type = ivl_type, ivl_side = ivl_side)

  switch(ivl_side,
         "lower" = {
           res <- sl - min(pred_lim, na.rm = TRUE)
         },
         "upper" = {
           res <- sl - max(pred_lim, na.rm = TRUE)
         })

  return(res)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Point of intersection
#'
#' The function \code{find_poi()} determines the point where the distance
#' between two lines is minimal, e.g., the distance between a specification or
#' expiry limit and a confidence or prediction interval. The estimation is done
#' by aid of \code{\link[stats]{uniroot}()} from the \sQuote{\code{stats}}
#' package.
#'
#' @param srch_range A vector of length \code{2} specifying the end-points of
#'   the (time) range within which the minimal distance is expected to be found.
#' @param model A linear model object of type \sQuote{\code{lm}}.
#' @param sl A numeric variable specifying the \dQuote{specification limit}
#'   (SL). Another kind of acceptance criterion may be regarded as SL.
#' @param alpha A numeric value specifying the significance level of the
#'   confidence or prediction interval that is calculated for the provided
#'   linear model. The default value is \code{0.05}.
#' @param ... Additional named or unnamed arguments passed on to
#'   \code{\link[stats]{uniroot}()}.
#' @inheritParams expirest_osle
#'
#' @details The function \code{find_poi()} (find the \dQuote{point of
#' intersection}) estimates the value of \eqn{x} (e.g., the time) where the
#' difference between the upper or lower confidence or prediction interval and
#' the upper or lower acceptance criterion (e.g., the specification or the
#' expiry limit) is minimal. Confidence or prediction intervals are calculated
#' for the \code{model} provided. The POI is determined by aid of the
#' \code{\link[stats]{uniroot}()} function from the \sQuote{\code{stats}}
#' package. The distance between the two lines of interest is calculated using
#' the function \code{\link{get_distance}()}, and it is this distance which
#' \code{uniroot()} tries to minimise. Recommendations on how to estimate shelf
#' life or expiry can be found in the corresponding section below.
#'
#' #@inheritSection get_distance How to estimate shelf life or expiry
#'
#' @return A numeric value representing the value of \eqn{x} where the distance
#' between the two lines of interest is minimal is returned.
#'
#' @inherit expirest_wisle references
#'
#' @seealso \code{\link{get_distance}}, \code{\link[stats]{uniroot}},
#' \code{\link{expirest_osle}}, \code{\link{expirest_wisle}}.
#'
#' @importFrom stats uniroot
#'
#' @keywords internal

find_poi <- function(srch_range, model, sl, alpha = 0.05, ivl = "confidence",
                     ivl_type = "one.sided", ivl_side = "lower", ...) {
  if (!is.numeric(srch_range) | length(srch_range) != 2) {
    stop("The parameter srch_range must be a vector of length 2.")
  }
  if (class(model) != "lm") {
    stop("Please provide a model of type \"lm\".")
  }
  if (!is.numeric(sl) | length(sl) > 1) {
    stop("sl must be a numeric value of length 1.")
  }
  if (alpha <= 0 | alpha > 1) {
    stop("Please specify alpha as (0, 1].")
  }
  if (!(ivl %in% c("confidence", "prediction", "fitted.line"))) {
    stop("Please specify ivl either as \"confidence\" or \"prediction\" or ",
         "\"fitted.line\".")
  }
  if (!(ivl_type %in% c("one.sided", "two.sided"))) {
    stop("Please specify ivl_type either as \"one.sided\" or \"two.sided\".")
  }
  if (!(ivl_side %in% c("lower", "upper"))) {
    stop("Please specify ivl_side either as \"lower\" or \"upper\".")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  tmp <- try_get_model(
    uniroot(f = get_distance, interval = srch_range, model = model,
            sl = sl, alpha = alpha, ivl = ivl, ivl_type = ivl_type,
            ivl_side = ivl_side, ...)[["root"]]
  )

  if (is.null(tmp[["Error"]])) {
    res <- tmp[["Model"]]
  } else {
    stop("Error in uniroot! utility.R.488: ", tmp[["Error"]])
  }

  return(res)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Transformation of variables
#'
#' The function \code{get_xformed_variables()} transforms the variables as
#' needed.
#'
#' @inheritParams expirest_osle
#'
#' @details The function \code{get_xformed_variables()} transforms the variables
#' (\code{response_vbl} and/or \code{time_vbl}) as specified by the parameters
#' \code{xform} and \code{shift}.
#'
#' @return The provided data frame with (a) new column(s) of the transformed
#' variable(s).
#'
#' @keywords internal

get_xformed_variables <- function(data, response_vbl, time_vbl,
                                  xform = c("no", "no"), shift = c(0, 0)) {
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
  if (length(xform) != 2) {
    stop("Please specify xform appropriately.")
  }
  if (!(xform[1] %in% c("no", "log", "sqrt", "sq")) |
      !(xform[2] %in% c("no", "log", "sqrt", "sq"))) {
    stop("Please specify xform appropriately.")
  }
  if (xform[1] == "log" & shift[1] != 1) {
    stop("For log xform of x select a shift of 1.")
  }
  if (xform[1] == "sqrt" & shift[1] != 0) {
    stop("For sqrt xform of x select a shift of 0.")
  }
  if (xform[1] == "sq" & shift[1] != 0) {
    stop("For sq xform of x select a shift of 0.")
  }
  if (!is.numeric(shift) | length(shift) != 2) {
    stop("The parameter shift must be a numeric vector of length 2.")
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

  d_dat <- data

  if (xform[1] != "no") {
    switch(xform[1],
           "log" = {
             trfmd_time_vbl <- paste("log", time_vbl, sep = ".")
             d_dat <- cbind(d_dat, log(d_dat[, time_vbl] + shift[1]))
             colnames(d_dat)[ncol(d_dat)] <- trfmd_time_vbl
           },
           "sqrt" = {
             trfmd_time_vbl <- paste("sqrt", time_vbl, sep = ".")
             d_dat <- cbind(d_dat, sqrt(d_dat[, time_vbl] + shift[1]))
             colnames(d_dat)[ncol(d_dat)] <- trfmd_time_vbl
           },
           "sq" = {
             trfmd_time_vbl <- paste("sq", time_vbl, sep = ".")
             d_dat <- cbind(d_dat, (d_dat[, time_vbl] + shift[1])^2)
             colnames(d_dat)[ncol(d_dat)] <- trfmd_time_vbl
           })
  }

  if (xform[2] != "no") {
    switch(xform[2],
           "log" = {
             trfmd_response_vbl <- paste("log", response_vbl, sep = ".")
             d_dat <- cbind(d_dat, log(d_dat[, response_vbl] + shift[2]))
             colnames(d_dat)[ncol(d_dat)] <- trfmd_response_vbl
           },
           "sqrt" = {
             trfmd_response_vbl <- paste("sqrt", response_vbl, sep = ".")
             d_dat <- cbind(d_dat, sqrt(d_dat[, response_vbl] + shift[2]))
             colnames(d_dat)[ncol(d_dat)] <- trfmd_response_vbl
           },
           "sq" = {
             trfmd_response_vbl <- paste("sq", response_vbl, sep = ".")
             d_dat <- cbind(d_dat, (d_dat[, response_vbl] + shift[2])^2)
             colnames(d_dat)[ncol(d_dat)] <- trfmd_response_vbl
           })
  }

  return(d_dat)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Listing of variable names
#'
#' The function \code{get_variable_list()} makes a list of the (original) and,
#' if applicable, the transformed variable names.
#'
#' @inheritParams expirest_osle
#'
#' @details The function \code{get_variable_list()} makes a list of the
#' variable names. If data have been transformed, the list comprises the
#' original variable name(s) (with suffix .orig in the corresponding list
#' element names) and the transformed variable name(s).
#'
#' @return A list with the variable names. If the data have been transformed,
#' the list element names of the original variables have the suffix
#' \code{".orig"}.
#'
#' @keywords internal

get_variable_list <- function(response_vbl, time_vbl, batch_vbl,
                              xform = c("no", "no")) {
  if (!is.character(response_vbl)) {
    stop("The parameter response_vbl must be a string.")
  }
  if (!is.character(time_vbl)) {
    stop("The parameter time_vbl must be a string.")
  }
  if (!is.character(batch_vbl)) {
    stop("The parameter batch_vbl must be a string.")
  }
  if (length(xform) != 2) {
    stop("Please specify xform appropriately.")
  }
  if (!(xform[1] %in% c("no", "log", "sqrt", "sq")) |
      !(xform[2] %in% c("no", "log", "sqrt", "sq"))) {
    stop("Please specify xform appropriately.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  l_variables <- as.list(c(batch_vbl, response_vbl, time_vbl))
  names(l_variables) <- c("batch", "response", "time")

  if (xform[1] != "no") {
    switch(xform[1],
           "log" = {
             time_vbl <- paste("log", time_vbl, sep = ".")
           },
           "sqrt" = {
             time_vbl <- paste("sqrt", time_vbl, sep = ".")
           },
           "sq" = {
             time_vbl <- paste("sq", time_vbl, sep = ".")
           })

    l_variables[[length(l_variables) + 1]] <- time_vbl
    names(l_variables)[3] <- "time.orig"
    names(l_variables)[length(l_variables)] <- "time"
  }
  if (xform[2] != "no") {
    switch(xform[2],
           "log" = {
             response_vbl <- paste("log", response_vbl, sep = ".")
           },
           "sqrt" = {
             response_vbl <- paste("sqrt", response_vbl, sep = ".")
           },
           "sq" = {
             response_vbl <- paste("sq", response_vbl, sep = ".")
           })

    l_variables[[length(l_variables) + 1]] <- response_vbl
    names(l_variables)[2] <- "response.orig"
    names(l_variables)[length(l_variables)] <- "response"
  }

  return(l_variables)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Adjustment of limits
#'
#' The function \code{set_limits()} adjusts the limits according to the number
#' of relevant decimal places and according to the transformation requirement.
#'
#' @inheritParams expirest_osle
#'
#' @details The function \code{set_limits()} adjusts the limits according to
#' \code{rl_sf} and \code{sl_sf} and, if necessary, transforms the limits
#' (\code{rl} and \code{sl}) as specified by the parameters \code{xform} and
#' \code{shift}.
#'
#' @return A list with the following elements is returned:
#' \item{sf.option}{A character string specifying the option concerning the
#'   significant figures.}
#' \item{xform}{A vector of two character strings specifying the transformation
#'   of the response and the time variable.}
#' \item{shift}{A vector of two values to be added to the values of the
#'   transformed \eqn{x} and/or \eqn{y} variables (specified via the
#'   \code{xform} parameter).}
#' \item{rl.orig}{An optional element containing a numeric value or a numeric
#'   vector specifying the release limit(s) on the original scale.}
#' \item{rl.sf}{An optional element containing a numeric value or a numeric
#'   vector specifying the significant figures of the release limit(s).}
#' \item{rl}{An optional element containing a numeric value or a numeric vector
#'   of the adjusted (as specified by the \code{sf.option} parameter) release
#'   limit(s).}
#' \item{rl.trfmd}{An optional element containing a numeric value or a numeric
#'   vector of the adjusted and transformed, if applicable (as specified by the
#'   the \code{sf.option} parameter and the second element of the \code{xform}
#'   parameter, respectively), release limit(s), otherwise the same as
#'   \code{rl}.}
#' \item{sl.orig}{A numeric value or a numeric vector of length \code{2}
#'   specifying the specification limit(s) on the original scale.}
#' \item{sl.sf}{A numeric value or a numeric vector of length \code{2}
#'   specifying the significant figures of the specification limit(s).}
#' \item{sl}{A numeric value or a numeric vector of length \code{2} of the
#'   adjusted (as specified by the \code{sf.option} parameter) specification
#'   limit(s).}
#' \item{sl.trfmd}{A numeric value or a numeric vector of length \code{2} of
#'   the adjusted and transformed, if applicable (as specified by the the
#'   \code{sf.option} parameter and the second element of the \code{xform}
#'   parameter, respectively) specification limit(s), otherwise the same as
#'   \code{sl}.}
#'
#' @keywords internal

set_limits <- function(rl, rl_sf, sl, sl_sf, sf_option = "loose",
                       xform = c("no", "no"), shift = c(0, 0),
                       ivl_side = "lower") {
  if (!is.numeric(rl) & all(!is.na(rl))) {
    stop("The parameter rl must be a numeric value or NA.")
  }
  if (!is.numeric(rl_sf) & all(!is.na(rl_sf))) {
    stop("The parameter rl_sf must be a positive integer of length rl, or NA.")
  }
  if (sum(rl_sf < 0) > 0 & all(!is.na(rl_sf))) {
    stop("The parameter rl_sf must be a positive integer of length rl, or NA.")
  }
  if (length(rl_sf) != length(rl)) {
    stop("The parameter rl_sf must be a positive integer of length rl, or NA.")
  }
  if (!isTRUE(all.equal(rl_sf, as.integer(rl_sf))) & all(!is.na(rl_sf))) {
    stop("The parameter rl_sf must be a positive integer of length rl, or NA.")
  }
  if (!is.numeric(sl) | length(sl) > 2) {
    stop("The parameter sl must be a numeric value or vector of length 1 or 2.")
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
  if (!(sf_option %in% c("tight", "loose"))) {
    stop("Please specify sf_option either as \"tight\" or \"loose\".")
  }
  if (length(xform) != 2) {
    stop("Please specify xform appropriately.")
  }
  if (!(xform[1] %in% c("no", "log", "sqrt", "sq")) |
      !(xform[2] %in% c("no", "log", "sqrt", "sq"))) {
    stop("Please specify xform appropriately.")
  }
  if (xform[1] == "log" & shift[1] != 1) {
    stop("For log xform of x select a shift of 1.")
  }
  if (xform[1] == "sqrt" & shift[1] != 0) {
    stop("For sqrt xform of x select a shift of 0.")
  }
  if (xform[1] == "sq" & shift[1] != 0) {
    stop("For sq xform of x select a shift of 0.")
  }
  if (!is.numeric(shift) | length(shift) != 2) {
    stop("The parameter shift must be a numeric vector of length 2.")
  }
  if (!(ivl_side %in% c("lower", "upper"))) {
    stop("Please specify ivl_side either as \"lower\" or \"upper\".")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Save the original values of rl and sl.

  rl_orig <- rl
  sl_orig <- sl

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If necessary, determine the adjusted (based on rl_sf and sl_sf) values of
  # rl and sl.

  if (sf_option == "loose"){
    # Specification limit(s)
    sl_factor <- 10^(get_n_whole_part(sl) - 1)
    sl_std <- signif(sl / sl_factor, sl_sf)

    if (length(sl) == 2) {
      sl[1] <- (sl_std[1] - 5 / 10^sl_sf[1]) * sl_factor[1]
      sl[2] <- (sl_std[2] + 4 / 10^sl_sf[2]) * sl_factor[2]
    } else {
      switch(ivl_side,
             "lower" = {
               sl <- (sl_std - 5 / 10^sl_sf) * sl_factor
             },
             "upper" = {
               sl <- (sl_std + 4 / 10^sl_sf) * sl_factor
             })
    }

    # Release limit(s)
    rl_factor <- 10^(get_n_whole_part(rl) - 1)
    rl_std <- signif(rl / rl_factor, rl_sf)

    switch(ivl_side,
           "lower" = {
             rl <- (rl_std - 5 / 10^rl_sf) * rl_factor
           },
           "upper" = {
             rl <- (rl_std + 4 / 10^rl_sf) * rl_factor
           })
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Based on the transformation of the response variable the rl and sl limits
  # also have to be transformed.

  if (xform[2] != "no") {
    switch(xform[2],
           "log" = {
             rl_trfmd <- log(rl + shift[2])
             sl_trfmd <- log(sl + shift[2])
           },
           "sqrt" = {
             rl_trfmd <- sqrt(rl + shift[2])
             sl_trfmd <- sqrt(sl + shift[2])
           },
           "sq" = {
             rl_trfmd <- (rl + shift[2])^2
             sl_trfmd <- (sl + shift[2])^2
           })
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Collect and return the data

  if (all(!is.na(rl))) {
    l_res <- list(sf.option = sf_option,
                  xform = xform,
                  shift = shift,
                  rl.orig = rl_orig,
                  rl.sf = rl_sf,
                  rl = rl,
                  sl.orig = sl_orig,
                  sl.sf = sl_sf,
                  sl = sl)
  } else {
    l_res <- list(sf.option = sf_option,
                  xform = xform,
                  shift = shift,
                  sl.orig = sl_orig,
                  sl.sf = sl_sf,
                  sl = sl)
  }

  if (xform[2] != "no") {
    if (all(!is.na(rl))) {
      l_res[[length(l_res) + 1]] <- rl_trfmd
      names(l_res)[length(l_res)] <- "rl.trfmd"
    }

    l_res[[length(l_res) + 1]] <- sl_trfmd
    names(l_res)[length(l_res)] <- "sl.trfmd"
  }

  return(l_res)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Determination of the \dQuote{worst case scenario} (wcs) limit
#'
#' The function \code{get_wcs_limit()} calculates \dQuote{worst case scenario}
#' (wcs) limit following the ARGPM Guidance \dQuote{Stability testing for
#' prescription medicines}.
#'
#' @param rl A numeric value specifying the release specification limit(s),
#'   on the same scale as \code{sl} and \code{intercept}.
#' @param sl A numeric value specifying the specification limit, on the same
#'   scale as \code{rl} and \code{intercept}.
#' @param intercept A numeric value representing the intercept of a linear
#'   regression model fitted to sample data, on the same scale as \code{rl}
#'   and \code{sl}.
#' @inheritParams expirest_osle
#'
#' @details The function \code{get_wcs_limit()} determines the \dQuote{worst
#' case scenario} (wcs) limit as is proposed by the Australian Regulatory
#' Guidelines for Prescription Medicines (ARGPM) guidance \dQuote{Stability
#' testing for prescription medicines}. According to this guideline, the shelf
#' life or expiry limit is estimated as the point where the upper or lower
#' limit of the 95\% confidence interval of the linear model fitted to the
#' data intersects the wcs limit. The wcs limit is obtained by
#' adding/subtracting the absolute difference of specification limit and
#' release limit to/from the common intercept of the test batches or the
#' intercept of the worst performing batch.
#'
#' If data have been linearised by transformation, all elements, i.e. \code{rl},
#' \code{sl} and \code{intercept} must be on the same, i.e. transformed, scale.
#' The results are returned on the transformed scale and on the original scale.
#'
#' @return A list with the following elements is returned:
#' \item{xform}{A vector of two character strings specifying the transformation
#'   of the response and the time variable.}
#' \item{shift}{A vector of two values which has been added to the values of
#'   the transformed \eqn{x} and/or \eqn{y} variables (specified via the
#'   \code{xform} parameter).}
#' \item{delta.lim}{A numeric value or a numeric vector of the absolute
#'   difference(s) between \code{rl} and {sl}, if \code{xform[2] != "no"} on
#'   the transformed scale.}
#' \item{delta.lim.orig}{A numeric value or a numeric vector of the absolute
#'   difference(s) between \code{rl} and {sl} on the original scale.}
#' \item{wcs.lim}{A numeric value or a numeric vector of the worst case
#'   scenario (wcs) limit(s), if \code{xform[2] != "no"} on the transformed
#'   scale.}
#' \item{wcs.lim.orig}{A numeric value or a numeric vector of the worst case
#'   scenario (wcs) limit(s) on the original scale.}
#'
#' @references
#' Therapeutic Goods Administration (TGA) of the Department of Health of the
#' Australian Government, Australian Regulatory Guidelines for Prescription
#' Medicines (ARGPM), Stability testing for prescription medicines,
#' Version 1.1, March 2017 \cr
#' \url{https://www.tga.gov.au/stability-testing-prescription-medicines}
#'
#' @seealso \code{\link{expirest_wisle}}, \code{\link[stats]{lm}}.
#'
#' @keywords internal

get_wcs_limit <- function(rl, sl, intercept, xform = c("no", "no"),
                          shift = c(0, 0), ivl_side = "lower") {
  if (!is.numeric(rl) | length(rl) > 1) {
    stop("The parameter rl must be a numeric value of length 1.")
  }
  if (!is.numeric(sl) | length(sl) > 1) {
    stop("The parameter sl must be a numeric value of length 1.")
  }
  if (!is.numeric(intercept)) {
    stop("The parameter intercept must be a numeric value of length 1.")
  }
  if (length(xform) != 2) {
    stop("Please specify xform appropriately.")
  }
  if (!(xform[1] %in% c("no", "log", "sqrt", "sq")) |
      !(xform[2] %in% c("no", "log", "sqrt", "sq"))) {
    stop("Please specify xform appropriately.")
  }
  if (xform[1] == "log" & shift[1] != 1) {
    stop("For log xform of x select a shift of 1.")
  }
  if (xform[1] == "sqrt" & shift[1] != 0) {
    stop("For sqrt xform of x select a shift of 0.")
  }
  if (xform[1] == "sq" & shift[1] != 0) {
    stop("For sq xform of x select a shift of 0.")
  }
  if (!is.numeric(shift) | length(shift) != 2) {
    stop("The parameter shift must be a numeric vector of length 2.")
  }
  if (!(ivl_side %in% c("lower", "upper"))) {
    stop("Please specify ivl_side either as \"lower\" or \"upper\".")
  }

  if (ivl_side == "lower" & rl < sl) {
    stop("If ivl_side is \"lower\" rl must be > sl.")
  }
  if (ivl_side == "upper" & rl > sl) {
    stop("If ivl_side is \"upper\" rl must be < sl.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determination of worst case scenario (wcs) limit(s)

  switch(ivl_side,
         "lower" = {
           delta_lim <- rl - sl
           wcs_lim <- intercept - delta_lim
         },
         "upper" = {
           delta_lim <- sl - rl
           wcs_lim <- intercept + delta_lim
         })

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Based on the transformation of the response variable delta_lim and
  # wcs_lim have to be back-transformed to the original scale.

  if (xform[2] != "no") {
    switch(xform[2],
           "log" = {
             delta_lim_orig <- exp(delta_lim) - shift[2]
             wcs_lim_orig <- exp(wcs_lim) - shift[2]
           },
           "sqrt" = {
             delta_lim_orig <- delta_lim^2 - shift[2]
             wcs_lim_orig <- wcs_lim^2 - shift[2]
           },
           "sq" = {
             delta_lim_orig <- sqrt(delta_lim) - shift[2]

             if (wcs_lim < 0) {
               wcs_lim_orig <- NA
             } else {
               wcs_lim_orig <- sqrt(wcs_lim) - shift[2]
             }
           })
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Collect and return the data

  if (xform[2] == "no") {
    l_res <- list(xform = xform,
                  shift = shift,
                  delta.lim = delta_lim,
                  delta.lim.orig = delta_lim,
                  wcs.lim = wcs_lim,
                  wcs.lim.orig = wcs_lim)
  } else {
    l_res <- list(xform = xform,
                  shift = shift,
                  delta.lim = delta_lim,
                  delta.lim.orig = delta_lim_orig,
                  wcs.lim = wcs_lim,
                  wcs.lim.orig = wcs_lim_orig)
  }

  return(l_res)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Result of ANCOVA model check
#'
#' The function \code{check_ancova()} fits an ANalysis of COVAriance (ANCOVA)
#' model to figure out which kind of linear regression model suits the
#' (historical) data best.
#'
#' @param alpha A numeric value specifying the significance level for the
#'   decision which model is appropriate, i.e. if the assumption of
#'   \emph{common slope} or \emph{common intercept} is appropriate or not.
#'   The default is \code{0.05}.
#' @inheritParams expirest_osle
#'
#' @details The function \code{check_ancova()} fits an ANCOVA (ANalyis of
#' COVAriance) model to the data contained in the provided data frame. Based
#' on \code{alpha}, it checks if the intercepts and/or slopes between the
#' groups differ significantly or not.
#'
#' @return A numeric vector of the form \code{c(ci, cs)} is returned, specifying
#' if a common intercept is appropriate (\code{ci = 1}) or not (\code{ci = 0})
#' and if a common slope is appropriate (\code{cs = 1}) or not (\code{cs = 0}).
#'
#' @seealso \code{\link[stats]{aov}}.
#'
#' @importFrom stats summary.aov
#'
#' @keywords internal

check_ancova <- function(data, response_vbl, time_vbl, batch_vbl,
                         alpha = 0.05) {
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
  if (alpha <= 0 | alpha > 1) {
    stop("Please specify alpha as (0, 1].")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  t_formula <-
    paste(response_vbl, "~", paste(batch_vbl, time_vbl, sep  = " * "))
  lm_ancova <-
    do.call("aov", list(as.formula(t_formula), data = as.name("data")))
  slm_ancova <- summary(lm_ancova)[[1]]

  p_batch <- slm_ancova[grepl(batch_vbl, rownames(slm_ancova)) &
                          !grepl(time_vbl, rownames(slm_ancova)), "Pr(>F)"]
  p_time <- slm_ancova[!grepl(batch_vbl, rownames(slm_ancova)) &
                         grepl(time_vbl, rownames(slm_ancova)), "Pr(>F)"]
  p_interaction <- slm_ancova[grepl(batch_vbl, rownames(slm_ancova)) &
                                grepl(time_vbl, rownames(slm_ancova)), "Pr(>F)"]

  # Store the outcome of the test in two logical parameters, i.e.
  # common_icpt: yes or no (common intercept model)
  # common_slp:  yes or no (common slope model)

  ifelse (p_batch > alpha, common_icpt <- 1L, common_icpt <- 0L)
  ifelse (p_interaction > alpha, common_slp <- 1L, common_slp <- 0L)

  t_res <- c(common_icpt, common_slp)
  names(t_res) <- c("common.icpt", "common.slp")

  return(t_res)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Getting intercept(s) of a linear model
#'
#' The function \code{get_icpt()} determines the intercept(s) of the provided
#' model.
#'
#' @param model A linear model object of type \sQuote{\code{lm}}.
#' @param response_vbl A character string specifying the response variable name
#'   that must be a column of the data frame that was used for model fitting.
#' @param time_vbl A character string specifying the time variable name that
#'   must be a column of data frame that was used for model fitting.
#' @param batch_vbl A character string specifying the column of the data frame
#'   that was used for model fitting with the grouping information (i.e. a
#'   factorial variable) for the differentiation of the observations from the
#'   different batches.
#' @inheritParams expirest_osle
#'
#' @details The function \code{get_icpt()} determines the intercept(s) of the
#' model that has been handed over via the \code{model} parameter.
#'
#' @return A list with a single element containing the numeric value or a
#' numeric vector of the intercept(s) or, if the data have been transformed,
#' a list with an additional element that contains the numeric value or numeric
#' vector on the original scale.
#'
#' @importFrom stats coef
#' @importFrom stats formula
#'
#' @keywords internal

get_icpt <- function(model, response_vbl, time_vbl, batch_vbl,
                     xform = c("no", "no"), shift = c(0, 0)) {
  if (class(model) != "lm") {
    stop("Please provide a model of type \"lm\".")
  }
  if (!is.character(response_vbl)) {
    stop("The parameter response_vbl must be a string.")
  }
  if (length(grep(response_vbl, as.character(formula(model$terms)))) == 0) {
    stop("The parameter response_vbl was not found in the provided model.")
  }
  if (!is.character(time_vbl)) {
    stop("The parameter time_vbl must be a string.")
  }
  if (length(grep(time_vbl, as.character(formula(model$terms)))) == 0) {
    stop("The parameter time_vbl was not found in the provided model.")
  }
  if (!is.character(batch_vbl)) {
    stop("The parameter batch_vbl must be a string.")
  }
  if (length(attr(model$terms, "order")) > 1) {
    if (length(grep(batch_vbl, as.character(formula(model$terms)))) == 0) {
      stop("The parameter batch_vbl was not found in the provided model.")
    }
  }
  if (length(xform) != 2) {
    stop("Please specify xform appropriately.")
  }
  if (!(xform[1] %in% c("no", "log", "sqrt", "sq")) |
      !(xform[2] %in% c("no", "log", "sqrt", "sq"))) {
    stop("Please specify xform appropriately.")
  }
  if (xform[1] == "log" & shift[1] != 1) {
    stop("For log xform of x select a shift of 1.")
  }
  if (xform[1] == "sqrt" & shift[1] != 0) {
    stop("For sqrt xform of x select a shift of 0.")
  }
  if (xform[1] == "sq" & shift[1] != 0) {
    stop("For sq xform of x select a shift of 0.")
  }
  if (!is.numeric(shift) | length(shift) != 2) {
    stop("The parameter shift must be a numeric vector of length 2.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determination of intercept(s)

  if (length(coef(model)) == 2) {
    intercept <- coef(model)["(Intercept)"]
  } else {
    t_coef <- coef(model)
    t_batch_levels <- levels(model[["model"]][, batch_vbl])

    t_batch_id <- grep(batch_vbl, names(t_coef))
    t_time_id <- grep(time_vbl, names(t_coef))

    # Calculate the intercepts
    intercept <- c(t_coef["(Intercept)"], t_coef["(Intercept)"] +
                     t_coef[t_batch_id[t_batch_id < t_time_id[1]]])

    # Rename the vector
    t_names <- names(intercept)
    t_names <- sub(batch_vbl, "", t_names)
    t_names[which(!(t_names %in% t_batch_levels))] <-
      t_batch_levels[which(!(t_names %in% t_batch_levels))]
    names(intercept) <- t_names
  }

  # ---------
  # Depending on the transformation of the response variable the intercept(s)
  # has (have) to be back-transformed to the original scale.

  if (xform[2] != "no") {
    switch(xform[2],
           "log" = {
             icpt_orig <- exp(intercept) - shift[2]
           },
           "sqrt" = {
             icpt_orig <- intercept^2 - shift[2]
           },
           "sq" = {
             if (sum(intercept < 0) > 0) {
               icpt_orig <- NA
             } else {
               icpt_orig <- sqrt(intercept) - shift[2]
             }
           })
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return of the intercept(s)

  l_res <- list(icpt = intercept)

  if (xform[2] != "no") {
    l_res[[length(l_res) + 1]] <- icpt_orig
    names(l_res)[length(l_res)] <- "icpt.orig"
  }

  return(l_res)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Print value(s)
#'
#' The function \code{print_val()} generates a character string for the purpose
#' to print a value on a plot (together with associated information).
#'
#' @param val_name A character string specifying the text preceding the value
#'   of the parameter to be displayed.
#' @param val_value A numeric value specifying the value of the parameter to
#'   be displayed.
#' @param val_unit A character string specifying the text following the value
#'   of the parameter to be displayed.
#' @param val_sf A positive integer specifying the number of significant
#'   figures for the display of the limit.
#' @param prefix A character string at the beginning of the whole text. The
#'   default is an empty string, i.e. \code{""}.
#' @param suffix A character string at the end of the whole text. The default
#'   is an empty string, i.e. \code{""}.
#'
#' @details The function \code{print_val()} generates a character string that
#' is based on the provided information. The string is used as label of a
#' corresponding graph element. For the number formatting, the
#' \code{\link[base]{sprintf}()} function from the \sQuote{\code{base}} package
#' is used. For concatenation of the various elements, the
#' \code{\link[base]{paste}()} function from the \sQuote{\code{base}} package
#' is used.
#'
#' @return A single character string of the form \dQuote{val_name: val_value
#' (with the number of specified decimal places) val_unit}.
#'
#' @seealso \code{\link[base]{formatC}}, \code{\link[base]{paste}}.
#'
#' @keywords internal

print_val <- function(val_name, val_value, val_unit, val_sf,
                      prefix = "", suffix = "") {
  if (!is.character(val_name)) {
    stop("The parameter val_name must be a string.")
  }
  if (length(val_value) > 1) {
    stop("The parameter val_value must be a numeric value of length 1.")
  }
  if (!is.numeric(val_value) & !is.na(val_value)) {
    stop("The parameter val_value must be a numeric value of length 1.")
  }
  if (!is.character(val_unit)) {
    stop("The parameter val_unit must be a string.")
  }
  if (!is.numeric(val_sf) | length(val_sf) > 1) {
    stop("The parameter val_sf must be a positive integer of length 1.")
  }
  if (val_sf != as.integer(val_sf)) {
    stop("The parameter val_sf must be a positive integer of length 1.")
  }
  if (val_sf < 0) {
    stop("The parameter val_sf must be a positive integer of length 1.")
  }
  if (!is.character(prefix)) {
    stop("The parameter prefix must be a string.")
  }
  if (!is.character(suffix)) {
    stop("The parameter suffix must be a string.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Formatting of information

  paste(prefix, val_name,
        formatC(signif(val_value, val_sf), digits = val_sf, format = "fg",
                flag = "#"),
        val_unit, suffix, sep = "")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Get number of digits of whole part (of a decimal number)
#'
#' The function \code{get_n_whole_part()} counts the number of digits of the
#' whole number portion of a decimal number.
#'
#' @param x A decimal number (or an integer) or a vector of decimal numbers (or
#'   of integers).
#'
#' @details The function \code{get_n_whole_part()} counts the number of digits
#' of the whole number portion of a decimal number.
#'
#' @return An integer representing the number of digits of the whole number
#' portion of the decimal number that was handed over.
#'
#' @keywords internal

get_n_whole_part <- function(x) {
  if (!is.numeric(x) & all(!is.na(x))) {
    stop("The parameter x must be a numeric value or NA.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determine n

  check <- function(xx) {
    if (is.na(xx)) {
      NA
    } else {
      if (xx <= 1 & xx >= -1) {
        1
      } else {
        floor(log10(abs(xx))) + 1
      }
    }
  }

  vapply(x, function(xx) check(xx), numeric(1))
}

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><>
