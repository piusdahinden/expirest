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
#' @return A numeric value or, if \code{model} contains a categorical variable,
#' a numeric vector of the predicted upper or lower confidence or prediction
#' interval limit(s) at a given value of \eqn{x}.
#'
#' @seealso \code{\link[stats]{predict.lm}}.
#'
#' @keywords internal

get_intvl_limit <- function(x_new, model, alpha = 0.05, ivl = "confidence",
                         ivl_type = "one.sided", ivl_side = "lower") {
  if (!is.numeric(x_new) && !is.na(x_new)) {
    stop("x_new must be a numeric value.")
  }
  if (!inherits(model, "lm")) {
    stop("Please provide a model of type \"lm\".")
  }
  if (alpha <= 0 || alpha > 1) {
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

  if (is_factor == 0 && length(variable_names) == 2) {
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

    n_levels <- vapply(grouping_variables,
                       function(x) {
                         nlevels(model$model[, grouping_variables])
                       },
                       numeric(1))
    n_levels <- prod(n_levels)

    l_newdata <-
      vapply(grouping_variables,
             function(x) {
               list(rep(levels(model$model[, grouping_variables]),
                        each = n_levels /
                          nlevels(model$model[, grouping_variables]) *
                          length(x_new)))
             },
             list(1))
    l_newdata[[length(l_newdata) + 1]] <- rep(x_new, n_levels)
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
    names(res) <- l_newdata[[grouping_variables]]
  }

  return(res)
}

#' Determine distance of lines
#'
#' The function \code{get_distance()} calculates the distance between two
#' lines at a given value of \eqn{x}.
#'
#' @param x_new A numeric value of \eqn{x} for which the distance between two
#'   lines is sought, e.g. the distance of upper or lower confidence/prediction
#'   interval limits from the upper or lower specification or expiry limits,
#'   respectively.
#' @param sl A numeric variable that specifies the \dQuote{specification limit}
#'   (SL) or, for determinations according to ARGPM guidance \dQuote{Stability
#'   testing for prescription medicines}, the \dQuote{expiry limit} (EL). The
#'   EL is defined as the intercept \eqn{\pm} the difference between the
#'   specification limit and the release limit (RL). If it is the upper limit
#'   which is the relevant limit, it is added (\code{+}) to the intercept,
#'   otherwise it is subtracted (\code{-}) from the intercept.
#' @param mode A character string of either \code{"minimal"} or \code{"all"},
#'   that specifies if only the minimal distance of a factor regression model
#'   is returned or if the distances of all lines belonging to the different
#'   factor levels is returned. The default is \code{"minimal"}.
#' @inheritParams get_intvl_limit
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
#' @seealso \code{\link{get_intvl_limit}}, \code{\link{find_poi}},
#' \code{\link{expirest_osle}}, \code{\link{expirest_wisle}},
#' \code{\link[stats]{lm}}.
#'
#' @keywords internal

get_distance <- function(x_new, model, sl, mode = "minimal", alpha = 0.05,
                         ivl = "confidence", ivl_type = "one.sided",
                         ivl_side = "lower") {
  if (!is.numeric(x_new)) {
    stop("x_new must be a numeric value.")
  }
  if (!inherits(model, "lm")) {
    stop("Please provide a model of type \"lm\".")
  }
  if (!is.numeric(sl) || length(sl) > 1) {
    stop("sl must be a numeric value of length 1.")
  }
  if (!(mode %in% c("minimal", "all"))) {
    stop("Please specify mode either as \"minimal\" or \"all\".")
  }
  if (alpha <= 0 || alpha > 1) {
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

  if (mode == "minimal") {
    switch(ivl_side,
           "lower" = {
             res <- sl - min(pred_lim, na.rm = TRUE)
           },
           "upper" = {
             res <- sl - max(pred_lim, na.rm = TRUE)
           })
  } else {
    res <- sl - pred_lim
  }

  return(res)
}

#' Find the point of intersection (POI)
#'
#' The function \code{find_poi()} determines the point where the distance
#' between two lines is minimal, e.g. the distance between a specification or
#' expiry limit and a confidence or prediction interval, or in other words
#' the point of intersection (POI). The estimation is done by aid of
#' \code{\link[stats]{uniroot}()} from the \sQuote{\code{stats}} package.
#'
#' @param srch_range A vector of length \code{2} that specifies the endpoints
#'   of the (time) range within which the minimum distance is expected.
#' @param sl A numeric variable that specifies the \dQuote{specification limit}
#'   (SL). Another kind of acceptance criterion may be regarded as SL.
#' @param ... Additional named or unnamed arguments passed on to
#'   \code{\link[stats]{uniroot}()}.
#' @inheritParams expirest_osle
#' @inheritParams get_distance
#'
#' @details The function \code{find_poi()} (find the \dQuote{point of
#' intersection}) estimates the value of \eqn{x} (e.g. the time) where the
#' difference between the upper or lower confidence or prediction interval and
#' the upper or lower acceptance criterion (e.g. the specification or the
#' expiry limit) is minimal, or in other words where the corresponding lines
#' intersect each other. Confidence or prediction intervals are calculated
#' for the \code{model} provided. The POI is determined by aid of the
#' \code{\link[stats]{uniroot}()} function from the \sQuote{\code{stats}}
#' package. The distance between the two lines of interest is calculated using
#' the function \code{\link{get_distance}()}, and it is this distance which
#' \code{uniroot()} tries to minimise. Recommendations on how to estimate shelf
#' life or expiry can be found in the corresponding section below.
#'
#' @inheritSection get_distance How to estimate shelf life or expiry
#'
#' @return A numeric value representing the value of \eqn{x} where the distance
#' between the two lines of interest is minimal is returned.
#'
#' @inherit expirest_wisle references
#'
#' @seealso \code{\link{get_distance}}, \code{\link{get_poi_list}},
#' \code{\link{expirest_osle}}, \code{\link{expirest_wisle}},
#' \code{\link[stats]{uniroot}}.
#'
#' @importFrom stats uniroot
#' @importFrom stats setNames
#'
#' @keywords internal

find_poi <- function(srch_range, model, sl, mode = "minimal", alpha = 0.05,
                     ivl = "confidence", ivl_type = "one.sided",
                     ivl_side = "lower", ...) {
  if (!is.numeric(srch_range) || length(srch_range) != 2) {
    stop("The parameter srch_range must be a vector of length 2.")
  }
  if (!inherits(model, "lm")) {
    stop("Please provide a model of type \"lm\".")
  }
  if (!is.numeric(sl) || length(sl) > 1) {
    stop("sl must be a numeric value of length 1.")
  }
  if (!(mode %in% c("minimal", "all"))) {
    stop("Please specify mode either as \"minimal\" or \"all\".")
  }
  if (alpha <= 0 || alpha > 1) {
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

  if (mode == "minimal") {
    tmp <- try_get_model(
      uniroot(f = get_distance, interval = srch_range, model = model,
              sl = sl, mode = mode, alpha = alpha, ivl = ivl,
              ivl_type = ivl_type, ivl_side = ivl_side, ...)[["root"]]
    )

    if (is.null(tmp[["Error"]])) {
      res <- tmp[["Model"]]
    } else {
      stop("Error in uniroot (find_poi)!: ", tmp[["Error"]])
    }
  } else {
    t_dist <-
      get_distance(x_new = 0, model = model, sl = sl, mode = mode,
                   alpha = alpha, ivl = ivl, ivl_type = ivl_type,
                   ivl_side = ivl_side)
    res <- setNames(rep(NA, length(t_dist)), names(t_dist))

    for(i in seq_along(t_dist)) {
      tmp <- try_get_model(
        uniroot(f = function(x, ii, ...) get_distance(x_new = x, ...)[ii],
                interval = srch_range, ii = i, model = model, sl = sl,
                mode = "all", alpha = alpha, ivl = ivl, ivl_type = ivl_type,
                ivl_side = ivl_side, ...)[["root"]]
      )

      if (is.null(tmp[["Error"]])) {
        res[i] <- tmp[["Model"]]
      }
    }
  }

  return(res)
}

#' List of points of intersection
#'
#' The function \code{get_poi_list()} prepares a list of points of intersection
#' (POI) for multiple regression models using the \code{find_poi()} function.
#'
#' @param data The data frame that was used for fitting the models of parameter
#'   \code{model_list}.
#' @param batch_vbl A character string that specifies the column in \code{data}
#'   with the grouping information (i.e. a categorical variable) for the
#'   differentiation of the observations of the different batches.
#' @param model_list A list of regression models of different type. Usually,
#'   it is a list of four elements named \code{cics}, \code{dics},
#'   \code{dids.pmse} and \code{dids}, where the first three elements contain
#'   \sQuote{\code{lm}} objects of the \dQuote{common intercept / common slope}
#'   (\code{cics}), \dQuote{different intercept / common slope} (\code{dics})
#'   and \dQuote{different intercept / different slope} (\code{dids.pmse}) type.
#'   The fourth element with the label \code{dids} is usually a list of the
#'   \sQuote{\code{lm}} objects that is obtained from fitting a regression
#'   model to the data of each level of the categorical variable separately.
#'   The \code{dids.pmse} model differs from the \code{dids} model in that it
#'   is a model with the categorical variable as a fixed main effect and with
#'   an interaction term of the categorical variable with the time variable,
#'   i.e. a model where the mean square error is pooled across batches (thus
#'   the \dQuote{pmse} suffix meaning \dQuote{pooled mean square error}). The
#'   \code{cics}, \code{dics} and \code{dids.pmse} elements are \code{NA} if
#'   data of only a single batch is available.
#' @param sl A numeric variable that specifies the \dQuote{specification limit}
#'   (SL). Another kind of acceptance criterion may be regarded as SL.
#' @inheritParams find_poi
#'
#' @details The function \code{get_poi_list()} applies the \code{find_poi()}
#' function (find the \dQuote{point of intersection}) on all the models that
#' are provided.
#'
#' @return A list of four elements named \code{cics}, \code{dics},
#' \code{dids.pmse} and \code{dids} is returned. Each of them contains a named
#' vector of the POI values estimated for each batch and named accordingly.
#'
#' @seealso \code{\link{get_model_list}}, \code{\link{find_poi}},
#' \code{\link{get_distance}}, \code{\link{get_osle_poi_list}},
#' \code{\link{expirest_osle}}, \code{\link{expirest_wisle}},
#' \code{\link[stats]{uniroot}}.
#'
#' @keywords internal

get_poi_list <- function(data, batch_vbl, model_list, sl, srch_range,
                         mode = "minimal", alpha = 0.05, ivl = "confidence",
                         ivl_type = "one.sided", ivl_side = "lower", ...) {
  if (!is.data.frame(data)) {
    stop("The data must be provided as data frame.")
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
  if (!is.list(model_list)) {
    stop("The parameter model_list must be a list.")
  }
  if (sum(names(model_list) %in% c("cics", "dics", "dids.pmse", "dids")) != 4) {
    stop("The parameter model_list must have four elements named \"cics\", ",
         "\"dics\", \"dids\" and \"dids.pmse\".")
  }
  if (!is.numeric(sl) || length(sl) > 1) {
    stop("The parameter sl must be a numeric value of length 1.")
  }
  if (!is.numeric(srch_range) || length(srch_range) != 2) {
    stop("The parameter srch_range must be a vector of length 2.")
  }
  if (!(mode %in% c("minimal", "all"))) {
    stop("Please specify mode either as \"minimal\" or \"all\".")
  }
  if (alpha <= 0 || alpha > 1) {
    stop("Please specify alpha as (0, 1].")
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

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Preparation of data

  d_dat <- droplevels(data)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determination of points of intersection (POIs)
  l_poi <- lapply(model_list, function(x) NA)

  l_poi[["dids"]] <-
    vapply(model_list[["dids"]],
           function(x) {
             tmp <- try_get_model(
               find_poi(srch_range = srch_range, model = x, sl = sl,
                        mode = "minimal", alpha = alpha, ivl_type = ivl_type,
                        ivl_side = ivl_side, ivl = ivl)
             )

             ifelse(is.null(tmp[["Error"]]), tmp[["Model"]], NA)
           },
           numeric(1))

  if (nlevels(d_dat[, batch_vbl]) > 1) {
    for (variety in names(l_poi)[names(l_poi) != "dids"]) {
      if (variety == "cics") {
        tmp <- try_get_model(
          find_poi(srch_range = srch_range, model = model_list[[variety]],
                   sl = sl, mode = "minimal", alpha = alpha,
                   ivl_type = ivl_type, ivl_side = ivl_side, ivl = ivl))
      }
      if (variety %in% c("dics", "dids.pmse")) {
        tmp <- try_get_model(
          find_poi(srch_range = srch_range, model = model_list[[variety]],
                   sl = sl, mode = "all", alpha = alpha,
                   ivl_type = ivl_type, ivl_side = ivl_side, ivl = ivl))
      }
      if (is.null(tmp[["Error"]])) {
        l_poi[[variety]] <- tmp[["Model"]]
      }
    }
  }

  return(l_poi)
}

#' Getting the intercept(s) of a linear model
#'
#' The function \code{get_icpt()} determines the intercept(s) of the provided
#' model.
#'
#' @param response_vbl A character string that specifies the response variable
#'   name that must be a column of the data frame that was used for model
#'   fitting.
#' @param time_vbl A character string that specifies the time variable name
#'   that must be a column of data frame that was used for model fitting.
#' @param batch_vbl A character string that specifies the column of the data
#'   frame that was used for model fitting with the grouping information (i.e.
#'   a categorical variable) for the differentiation of the observations from
#'   the different batches.
#' @inheritParams get_intvl_limit
#' @inheritParams expirest_osle
#'
#' @details The function \code{get_icpt()} determines the intercept(s) of the
#' model that has been handed over via the \code{model} parameter.
#'
#' @return A list with a single element containing the numeric value or a
#' numeric vector of the intercept(s) or, if the data have been transformed,
#' a list with an additional element that contains the numeric value or
#' numeric vector on the original scale is returned.
#'
#' @seealso \code{\link{get_icpt_list}}, \code{\link{expirest_osle}},
#' \code{\link{expirest_wisle}}, \code{\link[stats]{lm}}.
#'
#' @importFrom stats coef
#' @importFrom stats formula
#'
#' @keywords internal

get_icpt <- function(model, response_vbl, time_vbl, batch_vbl,
                     xform = c("no", "no"), shift = c(0, 0)) {
  if (!inherits(model, "lm")) {
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
  if (!(xform[1] %in% c("no", "log", "sqrt", "sq")) ||
      !(xform[2] %in% c("no", "log", "sqrt", "sq"))) {
    stop("Please specify xform appropriately.")
  }
  if (!is.numeric(shift) || length(shift) != 2) {
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
             icpt_orig <- rep(NA, length(intercept))
             names(icpt_orig) <- names(intercept)

             ok <- unname(which(intercept >= 0))
             if (length(ok) > 0) {
               icpt_orig[ok] <- sqrt(intercept[ok]) - shift[2]
             }
           })
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return of the intercept(s)

  if (xform[2] == "no") {
    l_res <- list(icpt = intercept)
  } else {
    l_res <- list(icpt = intercept,
                  icpt.orig = icpt_orig)
  }

  return(l_res)
}

#' List of intercepts
#'
#' The function \code{get_icpt_list()} prepares a list of the intercepts
#' of the regression models fitted to the data.
#'
#' @param data The data frame that was used for fitting the models of parameter
#'   \code{model_list}.
#' @inheritParams expirest_osle
#' @inheritParams get_poi_list
#'
#' @details The function \code{get_icpt_list()} extracts the intercepts of
#' the various regression models (fitted by aid of the \code{lm()} function)
#' that are passed in via the \code{model_list} parameter.
#'
#' @return A list of four elements named \code{cics}, \code{dics},
#' \code{dids.pmse} and \code{dids} is returned. Each of them contains a list
#' element named \code{icpt} with a vector of the intercepts. If the data
#' have been transformed, each of the primary list elements contains a further
#' list element called \code{icpt.orig} with a numeric vector of the intercepts
#' on the original scale.
#'
#' @seealso \code{\link{get_model_list}}, \code{\link{get_icpt}},
#' \code{\link{expirest_osle}}, \code{\link{expirest_wisle}}.
#'
#' @keywords internal

get_icpt_list <- function(data, response_vbl, time_vbl, batch_vbl, model_list,
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
  if (!is.character(batch_vbl)) {
    stop("The parameter batch_vbl must be a string.")
  }
  if (!(batch_vbl %in% colnames(data))) {
    stop("The batch_vbl was not found in the provided data frame.")
  }
  if (!is.factor(data[, batch_vbl])) {
    stop("The column in data specified by batch_vbl must be a factor.")
  }
  if (!is.list(model_list)) {
    stop("The parameter model_list must be a list.")
  }
  if (sum(names(model_list) %in% c("cics", "dics", "dids.pmse", "dids")) != 4) {
    stop("The parameter model_list must have four elements named \"cics\", ",
         "\"dics\", \"dids\" and \"dids.pmse\".")
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

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Preparation of data

  d_dat <- droplevels(data)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determination of the intercepts

  if (nlevels(d_dat[, batch_vbl]) > 1) {
    l_icpt <- vapply(model_list[c("cics", "dics", "dids.pmse")], function(x) {
      list(get_icpt(model = x, response_vbl = response_vbl,
                    time_vbl = time_vbl, batch_vbl = batch_vbl,
                    xform = xform, shift = shift))
    },
    list(1))
  } else {
    l_icpt <- list(cics = NA, dics = NA, dids.pmse = NA)
  }

  tmp <- vapply(model_list[["dids"]], function(x) {
    list(get_icpt(model = x, response_vbl = response_vbl,
                  time_vbl = time_vbl, batch_vbl = batch_vbl,
                  xform = xform, shift = shift))
  },
  list(1))
  tmp <- unlist(tmp)
  names(tmp) <- sub("\\.\\(Intercept\\)", "", names(tmp))

  if (xform[2] == "no") {
    names(tmp) <- sub("\\.icpt", "", names(tmp))
    l_icpt <- c(l_icpt, list(dids = list(icpt = tmp)))
  } else {
    t_i_orig <- grep("orig", names(tmp))
    names(tmp) <- sub("\\.icpt", "", names(tmp))
    names(tmp) <- sub("\\.orig", "", names(tmp))

    l_icpt <- c(l_icpt, list(dids = list(icpt = tmp[-t_i_orig],
                                         icpt.orig = tmp[t_i_orig])))
  }

  return(l_icpt)
}

#' Determination of the \dQuote{worst case scenario} (wcs) limit
#'
#' The function \code{get_wcs_limit()} calculates \dQuote{worst case scenario}
#' (wcs) limit following the ARGPM Guidance \dQuote{Stability testing for
#' prescription medicines}.
#'
#' @param rl A numeric value that specifies the release specification limit(s),
#'   on the same scale as \code{sl} and \code{intercept}.
#' @param sl A numeric value that specifies the specification limit, on the same
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
#' \item{xform}{A vector of two character strings that specifies the
#'   transformation of the response and the time variable.}
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
#' Version 1.1, March 2017
#'
#' @seealso \code{\link{extract_from_ll_wcsl}},
#' \code{\link{get_wisle_poi_list}}, \code{\link{expirest_wisle}}.
#'
#' @keywords internal

get_wcs_limit <- function(rl, sl, intercept, xform = c("no", "no"),
                          shift = c(0, 0), ivl_side = "lower") {
  if (!is.numeric(rl) || length(rl) > 1) {
    stop("The parameter rl must be a numeric value of length 1.")
  }
  if (!is.numeric(sl) || length(sl) > 1) {
    stop("The parameter sl must be a numeric value of length 1.")
  }
  if (!is.numeric(intercept)) {
    stop("The parameter intercept must be a numeric value of length 1.")
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
  if (!(ivl_side %in% c("lower", "upper"))) {
    stop("Please specify ivl_side either as \"lower\" or \"upper\".")
  }

  if (ivl_side == "lower" && any(rl < sl)) {
    stop("If ivl_side is \"lower\" rl must be > sl.")
  }
  if (ivl_side == "upper" && any(rl > sl)) {
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

#' Get intercepts of the worst case batches
#'
#' The function \code{get_wc_icpt()} prepares a vector of the intercepts
#' of the worst case batches of all the regression models fitted to the data.
#'
#' @param icpt_list A list of four elements named \code{cics}, \code{dics},
#'   \code{dids.pmse} and \code{dids} with the intercepts of each linear
#'   regression model and batch. The \code{cics}, \code{dics} and
#'   \code{dids.pmse} elements are \code{NA} if data of only a single batch
#'   is available.
#' @param poi_list A list of four elements named \code{cics}, \code{dics},
#'   \code{dids.pmse} and \code{dids} with the points of intersection (POI)
#'   of each linear regression model and batch. The \code{cics}, \code{dics}
#'   and \code{dids.pmse} elements are \code{NA} if data of only a single
#'   batch is available.
#' @param wc_batch A numeric vector of the indices of the worst case batches
#'   of each model type, i.e. a vector of four elements named \code{cics},
#'   \code{dics}, \code{dids.pmse} and \code{dids}. The \code{cics} element
#'   is \code{NA} because in the \dQuote{common intercept / common slope} model
#'   the data from different batches is pooled.
#' @inheritParams expirest_osle
#'
#' @details The function \code{get_wc_icpt()} extracts the worst case batches
#' from the list of intercepts (\code{icpt_list}), given that the estimation
#' of the corresponding POIs (\code{poi_list}) was successful.
#'
#' @return A named vector of the intercepts of the worst case batches is
#' returned.
#'
#' @seealso \code{\link{get_icpt_list}}, \code{\link{get_poi_list}},
#' \code{\link{get_osle_poi_list}}, \code{\link{get_wisle_poi_list}},
#' \code{\link{expirest_osle}}, \code{\link{expirest_wisle}}.
#'
#' @importFrom stats setNames
#'
#' @keywords internal

get_wc_icpt <- function(data, batch_vbl, icpt_list, poi_list, wc_batch, xform) {
  if (!is.data.frame(data)) {
    stop("The data must be provided as data frame.")
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
  if (!is.list(icpt_list)) {
    stop("The parameter icpt_list must be a list.")
  }
  if (sum(names(icpt_list) %in% c("cics", "dics", "dids.pmse", "dids")) != 4) {
    stop("The parameter icpt_list must have four elements named \"cics\", ",
         "\"dics\", \"dids\" and \"dids.pmse\".")
  }
  if (!is.list(poi_list)) {
    stop("The parameter poi_list must be a list.")
  }
  if (sum(names(poi_list) %in% c("cics", "dics", "dids.pmse", "dids")) != 4) {
    stop("The parameter poi_list must have four elements named \"cics\", ",
         "\"dics\", \"dids\" and \"dids.pmse\".")
  }
  if (!is.numeric(wc_batch)) {
    stop("The parameter wc_batch must be a numeric vector.")
  }
  if (sum(names(wc_batch) %in% c("cics", "dics", "dids.pmse", "dids")) != 4) {
    stop("The parameter wc_batch must have four elements named \"cics\", ",
         "\"dics\", \"dids\" and \"dids.pmse\".")
  }
  if (length(xform) != 2) {
    stop("Please specify xform appropriately.")
  }
  if (!(xform[1] %in% c("no", "log", "sqrt", "sq")) ||
      !(xform[2] %in% c("no", "log", "sqrt", "sq"))) {
    stop("Please specify xform appropriately.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Preparation of data

  d_dat <- droplevels(data)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (nlevels(d_dat[, batch_vbl]) > 1) {
    wc_icpt <-
      vapply(names(icpt_list), function(nn) {
        if (nn == "cics") {
          if (xform[2] != "no") {
            unname(icpt_list[[nn]][["icpt.orig"]])
          } else {
            unname(icpt_list[[nn]][["icpt"]])
          }
        } else {
          if (xform[2] != "no") {
            icpt_list[[nn]][["icpt.orig"]][wc_batch[nn]]
          } else {
            icpt_list[[nn]][["icpt"]][wc_batch[nn]]
          }
        }
      },
      numeric(1))
  } else {
    wc_icpt <-
      setNames(rep(NA, length(wc_batch)), names(wc_batch))

    if (!any(is.na(poi_list[["dids"]]))) {
      if (xform[2] != "no") {
        wc_icpt["dids"] <-
          icpt_list[["dids"]][["icpt.orig"]][wc_batch["dids"]]
      } else {
        wc_icpt["dids"] <-
          icpt_list[["dids"]][["icpt"]][wc_batch["dids"]]
      }
    }
  }

  return(wc_icpt)
}

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
#' @seealso \code{\link{expirest_osle}}, \code{\link{expirest_wisle}}.
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
  if (!(xform[1] %in% c("no", "log", "sqrt", "sq")) ||
      !(xform[2] %in% c("no", "log", "sqrt", "sq"))) {
    stop("Please specify xform appropriately.")
  }
  if (!is.numeric(shift) || length(shift) != 2) {
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

  d_dat <- droplevels(data)

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
#' @seealso \code{\link{expirest_osle}}, \code{\link{expirest_wisle}}.
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
  if (!(xform[1] %in% c("no", "log", "sqrt", "sq")) ||
      !(xform[2] %in% c("no", "log", "sqrt", "sq"))) {
    stop("Please specify xform appropriately.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (xform[1] != "no") {
    switch(xform[1],
           "log" = {
             new_time_vbl <- paste("log", time_vbl, sep = ".")
           },
           "sqrt" = {
             new_time_vbl <- paste("sqrt", time_vbl, sep = ".")
           },
           "sq" = {
             new_time_vbl <- paste("sq", time_vbl, sep = ".")
           })
  }
  if (xform[2] != "no") {
    switch(xform[2],
           "log" = {
             new_response_vbl <- paste("log", response_vbl, sep = ".")
           },
           "sqrt" = {
             new_response_vbl <- paste("sqrt", response_vbl, sep = ".")
           },
           "sq" = {
             new_response_vbl <- paste("sq", response_vbl, sep = ".")
           })
  }

  if (xform[1] == "no" && xform[2] == "no") {
    l_variables <- list(batch = batch_vbl,
                        response = response_vbl,
                        time = time_vbl)
  }

  if (xform[1] != "no" && xform[2] != "no") {
    l_variables <- list(batch = batch_vbl,
                        response.orig = response_vbl,
                        response = new_response_vbl,
                        time.orig = time_vbl,
                        time = new_time_vbl)
  } else {
    if (xform[1] != "no") {
      l_variables <- list(batch = batch_vbl,
                          response = response_vbl,
                          time.orig = time_vbl,
                          time = new_time_vbl)
    }
    if (xform[2] != "no") {
      l_variables <- list(batch = batch_vbl,
                          response.orig = response_vbl,
                          response = new_response_vbl,
                          time = time_vbl)
    }
  }

  return(l_variables)
}

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
#' \item{sf.option}{A character string that specifies the option concerning the
#'   significant figures.}
#' \item{xform}{A vector of two character strings that specifies the
#'   transformation of the response and the time variable.}
#' \item{shift}{A vector of two values to be added to the values of the
#'   transformed \eqn{x} and/or \eqn{y} variables (specified via the
#'   \code{xform} parameter).}
#' \item{rl.orig}{A numeric value or a numeric vector of the release limit(s)
#'   on the original scale. If \code{NA} was passed in, \code{NA} is returned.}
#' \item{rl.sf}{A numeric value or a numeric vector that specifies the
#'   significant figures of the release limit(s). If \code{NA} was  passed in,
#'   \code{NA} is returned.}
#' \item{rl}{A numeric value or a numeric vector of the adjusted (as specified
#'   by the \code{sf.option} parameter) release limit(s). If \code{NA} was
#'   passed in, \code{NA} is returned.}
#' \item{rl.trfmd}{A numeric value or a numeric vector of the adjusted and
#'   transformed, if applicable (as specified by the the \code{sf.option}
#'   parameter and the second element of the \code{xform} parameter,
#'   respectively), release limit(s), otherwise the same as \code{rl}.}
#' \item{sl.orig}{A numeric value or a numeric vector of length \code{2} of the
#'   specification limit(s) on the original scale.}
#' \item{sl.sf}{A numeric value or a numeric vector of length \code{2}
#'   that specifies the significant figures of the specification limit(s).}
#' \item{sl}{A numeric value or a numeric vector of length \code{2} of the
#'   adjusted (as specified by the \code{sf.option} parameter) specification
#'   limit(s).}
#' \item{sl.trfmd}{A numeric value or a numeric vector of length \code{2} of
#'   the adjusted and transformed, if applicable (as specified by the the
#'   \code{sf.option} parameter and the second element of the \code{xform}
#'   parameter, respectively) specification limit(s), otherwise the same as
#'   \code{sl}.}
#'
#' @seealso \code{\link{expirest_osle}}, \code{\link{expirest_wisle}}.
#'
#' @keywords internal

set_limits <- function(rl, rl_sf, sl, sl_sf, sf_option = "loose",
                       xform = c("no", "no"), shift = c(0, 0),
                       ivl_side = "lower") {
  if (!is.numeric(rl) && all(!is.na(rl))) {
    stop("The parameter rl must be a numeric value or NA.")
  }
  if (!is.numeric(rl_sf) && all(!is.na(rl_sf))) {
    stop("The parameter rl_sf must be a positive integer of length rl, or NA.")
  }
  if (sum(rl_sf < 0) > 0 && all(!is.na(rl_sf))) {
    stop("The parameter rl_sf must be a positive integer of length rl, or NA.")
  }
  if (length(rl_sf) != length(rl)) {
    stop("The parameter rl_sf must be a positive integer of length rl, or NA.")
  }
  if (!isTRUE(all.equal(rl_sf, as.integer(rl_sf))) && all(!is.na(rl_sf))) {
    stop("The parameter rl_sf must be a positive integer of length rl, or NA.")
  }
  if (!is.numeric(sl) || length(sl) > 2) {
    stop("The parameter sl must be a numeric value or vector of length 1 or 2.")
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
  if (!(sf_option %in% c("tight", "loose"))) {
    stop("Please specify sf_option either as \"tight\" or \"loose\".")
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
  if (!(ivl_side %in% c("lower", "upper", "both"))) {
    stop("Please specify ivl_side either as \"lower\", \"upper\" or \"both\".")
  }
  if (ivl_side == "both" && length(sl) == 1) {
    stop("Since ivl_side = \"both\", a specification with two sides is ",
         "expected. Only one side has been specified, though, i.e. ",
         "sl = ", sl, ".\nPlease provide a specification with two sides.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Save the original values of rl and sl.

  rl_orig <- rl
  sl_orig <- sl

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If necessary, determine the adjusted (based on rl_sf and sl_sf) values of
  # rl and sl.

  if (sf_option == "loose") {
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

    if (ivl_side != "both") {
      switch(ivl_side,
             "lower" = {
               rl <- (rl_std - 5 / 10^rl_sf) * rl_factor
             },
             "upper" = {
               rl <- (rl_std + 4 / 10^rl_sf) * rl_factor
             })
    }
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

  if (xform[2] != "no") {
    l_res <- list(sf.option = sf_option,
                  xform = xform,
                  shift = shift,
                  rl.orig = rl_orig,
                  rl.sf = rl_sf,
                  rl = rl,
                  rl.trfmd = rl_trfmd,
                  sl.orig = sl_orig,
                  sl.sf = sl_sf,
                  sl = sl,
                  sl.trfmd = sl_trfmd)
  } else {
    l_res <- list(sf.option = sf_option,
                  xform = xform,
                  shift = shift,
                  rl.orig = rl_orig,
                  rl.sf = rl_sf,
                  rl = rl,
                  rl.trfmd = rl,
                  sl.orig = sl_orig,
                  sl.sf = sl_sf,
                  sl = sl,
                  sl.trfmd = sl)
  }

  return(l_res)
}

#' Get relevant limits
#'
#' The function \code{get_relevant_limits()} expects a list returned by the
#' function \code{set_limits} and returns a list of only the relevant limits,
#' i.e. those that are relevant with respect to transformation.
#'
#' @param limits_list A list returned by the \code{set_limits()} function.
#' @inheritParams expirest_osle
#'
#' @details The function \code{get_relevant_limits()} makes a subset of the
#' list returned by the \code{set_limits()} function.
#'
#' @return A list with the following elements is returned:
#' \item{sl.orig}{A numeric value or a numeric vector of length \code{2} of the
#'   specification limit(s) on the original scale.}
#' \item{sl}{A numeric value or a numeric vector of length \code{2} of adjusted
#'   specification limit(s).}
#' \item{rl.orig}{A numeric value or a numeric vector that specifies the release
#'   limit(s) on the original scale. If \code{NA} was  passed in, \code{NA} is
#'   returned.}
#' \item{rl}{A numeric value or a numeric vector of adjusted release limit(s).
#'   If \code{NA} was passed in, \code{NA} is returned.}
#' \item{sl.bt}{A numeric value or a numeric vector of length \code{2} of
#'   adjusted specification limit(s) before transformation. If no
#'   transformation has been performed it is \code{NA}.}
#' \item{rl.bt}{A numeric value or a numeric vector of adjusted release
#'   limit(s) before transformation. If no transformation has been performed
#'   it is \code{NA}.}
#'
#' @seealso \code{\link{set_limits}}
#'
#' @keywords internal

get_relevant_limits <- function(limits_list, xform = c("no", "no"),
                                ivl_side = "lower") {
  if (!is.list(limits_list)) {
    stop("The parameter limits_list must be a list.")
  }
  if (sum(names(limits_list) %in%
          c("sf.option", "xform", "shift", "sl.orig", "sl.sf", "sl",
            "sl.trfmd")) != 7) {
    stop("The limits_list must have at least the elements \"sf.option\",
         \"xform\", \"shift\", \"sl.orig\", \"sl.sf\", \"sl\" and ",
         "\"sl.trfmd\".")
  }
  if (length(xform) != 2) {
    stop("Please specify xform appropriately.")
  }
  if (!(xform[1] %in% c("no", "log", "sqrt", "sq")) ||
      !(xform[2] %in% c("no", "log", "sqrt", "sq"))) {
    stop("Please specify xform appropriately.")
  }
  if (!(ivl_side %in% c("lower", "upper", "both"))) {
    stop("Please specify ivl_side either as \"lower\", \"upper\" or \"both\".")
  }
  if (ivl_side == "both" && length(limits_list[["sl"]]) == 1) {
    stop("Since ivl_side = \"both\", a specification with two sides is ",
         "expected. Only one side has been specified, though, i.e. ",
         "sl = ", limits_list[["sl"]], ".\nPlease provide a specification ",
         "with two sides.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extract the relevant limits

  # ---------
  # Specification limits
  if (length(limits_list[["sl"]]) == 2 && ivl_side != "both") {
    switch(ivl_side,
           "lower" = {
             sl_orig <- limits_list[["sl.orig"]][1]

             if (xform[2] == "no") {
               sl <- limits_list[["sl"]][1]
               sl_bt <- NA
             } else {
               sl <- limits_list[["sl.trfmd"]][1]
               sl_bt <- limits_list[["sl"]][1]
             }
           },
           "upper" = {
             sl_orig <- limits_list[["sl.orig"]][2]

             if (xform[2] == "no") {
               sl <- limits_list[["sl"]][2]
               sl_bt <- NA
             } else {
               sl <- limits_list[["sl.trfmd"]][2]
               sl_bt <- limits_list[["sl"]][2]
             }
           })
  } else {
    sl_orig <- limits_list[["sl.orig"]]

    if (xform[2] == "no") {
      sl <- limits_list[["sl"]]
      sl_bt <- NA
    } else {
      sl <- limits_list[["sl.trfmd"]]
      sl_bt <- limits_list[["sl"]]
    }
  }

  # ---------
  # Release limits
  rl_orig <- limits_list[["rl.orig"]]

  if (xform[2] == "no") {
    rl <- limits_list[["rl"]]
    rl_bt <- NA
  } else {
    rl <- limits_list[["rl.trfmd"]]
    rl_bt <- limits_list[["rl"]]
  }

  # ---------
  # Compile and return results

  return(list(sl.orig = sl_orig,
              sl = sl,
              rl.orig = rl_orig,
              rl = rl,
              sl.bt = sl_bt,
              rl.bt = rl_bt))
}

#' Result of ANCOVA model check
#'
#' The function \code{check_ancova()} fits an ANalysis of COVAriance (ANCOVA)
#' model to figure out which kind of linear regression model suits the
#' (historical) data best.
#'
#' @param alpha A numeric value that specifies the significance level for the
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
#' @return A list of two elements is returned that specifies which model, based
#'   on the ANCOVA analysis, suits best. The first element (\code{type.spec})
#'   is a numeric vector of length 2 that specifies the best model accepted at
#'   the significance level specified by \code{alpha}. It has the form
#'   \code{c(ci, cs)}, where \code{ci} specifies if a common intercept is
#'   appropriate (\code{ci = 1}) or not (\code{ci = 0}) and \code{cs} specifies
#'   if a common slope is appropriate (\code{cs = 1}) or not (\code{cs = 0}).
#'   The second element (\code{type.acronym}) is an acronym representing the
#'   first item. In case of a linear model including only a single batch,
#'   all elements are \code{NA}.
#'
#' @seealso \code{\link{expirest_osle}}, \code{\link{expirest_wisle}},
#' \code{\link[stats]{aov}}.
#'
#' @importFrom stats summary.aov
#' @importFrom stats setNames
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
  if (alpha <= 0 || alpha > 1) {
    stop("Please specify alpha as (0, 1].")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Remove unused factor levels
  data <- droplevels(data)

  if (nlevels(data[, batch_vbl]) > 1) {
    t_formula <-
      paste(response_vbl, "~", paste(batch_vbl, time_vbl, sep  = " * "))
    lm_ancova <-
      do.call("aov", list(as.formula(t_formula), data = as.name("data")))
    slm_ancova <- summary(lm_ancova)[[1]]

    p_batch <-
      slm_ancova[grepl(batch_vbl, rownames(slm_ancova)) &
                   !grepl(time_vbl, rownames(slm_ancova)), "Pr(>F)"]
    p_interaction <-
      slm_ancova[grepl(batch_vbl, rownames(slm_ancova)) &
                   grepl(time_vbl, rownames(slm_ancova)), "Pr(>F)"]

    # Store the outcome of the test in two logical parameters, i.e.
    # common_icpt: yes or no (common intercept model)
    # common_slp:  yes or no (common slope model)

    ifelse(p_batch > alpha, common_icpt <- 1L, common_icpt <- 0L)
    ifelse(p_interaction > alpha, common_slp <- 1L, common_slp <- 0L)

    l_model_type <-
      list(type.spec = setNames(c(common_icpt, common_slp),
                                c("common.icpt", "common.slp")),
           type.acronym =
             paste0(c("di", "ci")[common_icpt + 1],
                    c("ds", "cs")[common_slp + 1]))
  } else {
    l_model_type <-
      list(type.spec = setNames(c(NA, NA), c("common.icpt", "common.slp")),
           type.acronym = "n.a.")
  }

  return(l_model_type)
}

#' Linear model fitting
#'
#' The function \code{get_model_list()} fits four types of linear regression
#' models that are often used for the assessment of stability data, e.g. for
#' the estimation of the shelf life or retest period.
#'
#' @inheritParams expirest_osle
#'
#' @details The function \code{get_model_list()} expects a data frame with
#' a response variable, a time variable and a categorical variable which
#' usually has factor levels of multiple batches of a drug product that was
#' assessed over a certain period of time with respect to the time-dependent
#' behavior of characteristic parameters. Using these results, the function
#' fits
#' \itemize{
#'  \item a \emph{common intercept / common slope} model (cics),
#'  \item a \emph{different intercept / common slope} model (dics) or
#'  \item a \emph{different intercept / different slope} model with pooled
#'    mean square error (dids.pmse) and
#'  \item a \emph{different intercept / different slope} model (dids) in which
#'    individual models are fitted to each level of the categorical variable.
#' }
#'
#' If the categorical variable has only a single factor level, then the first
#' three models are \code{NA} and only a single regression model is fitted.
#'
#' @return A list of three elements is returned,
#' containing the following elements:
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
#'
#' @seealso \code{\link{expirest_osle}}, \code{\link{expirest_wisle}},
#' \code{\link[stats]{lm}}, \code{\link[stats]{AIC}}, \code{\link[stats]{BIC}}.
#'
#' @importFrom stats lm
#' @importFrom stats as.formula
#' @importFrom stats coef
#' @importFrom stats AIC
#' @importFrom stats BIC
#' @importFrom stats setNames
#'
#' @keywords internal

get_model_list <- function(data, response_vbl, time_vbl, batch_vbl) {
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

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Remove unused factor levels
  d_dat <- droplevels(data)
  l_models <- setNames(vector(mode = "list", length = 4),
                       c("cics", "dics", "dids.pmse", "dids"))

  if (nlevels(d_dat[, batch_vbl]) > 1) {
    # ---------
    # Common Intercept / Common Slope
    t_formula <- paste(response_vbl, "~", time_vbl)
    l_models[["cics"]] <-
      do.call("lm", list(as.formula(t_formula), data = as.name("d_dat")))

    # ---------
    # Different Intercept / Common Slope
    t_formula <-
      paste(response_vbl, "~", paste(batch_vbl, time_vbl, sep = " + "))
    l_models[["dics"]] <-
      do.call("lm", list(as.formula(t_formula), data = as.name("d_dat")))

    # ---------
    # Different Intercept / Different Slope (pooled mean square error)
    t_formula <-
      paste(response_vbl, "~", paste(batch_vbl, time_vbl, sep = " * "))
    l_models[["dids.pmse"]] <-
      do.call("lm", list(as.formula(t_formula), data = as.name("d_dat")))

    # ---------
    # Different Intercept / Different Slope (individual models)
    t_formula <- paste(response_vbl, "~", time_vbl)
    tmp <- lapply(levels(d_dat[, batch_vbl]),
                  function(batch) {
                    t_dat <- d_dat[d_dat[, batch_vbl] == batch, ]
                    do.call("lm", list(as.formula(t_formula),
                                       data = as.name("t_dat")))
                  })
    names(tmp) <- levels(d_dat[, batch_vbl])
    l_models[["dids"]] <- tmp

    # ---------
    # Determination of the Akaike Information Criterion (AIC) and Bayesian
    # Information Criterion (BIC) of each of the relevant models

    t_AIC <- vapply(l_models[c("cics", "dics", "dids.pmse")], AIC, numeric(1))
    t_BIC <- vapply(l_models[c("cics", "dics", "dids.pmse")], BIC, numeric(1))
  } else {
    t_formula <- paste(response_vbl, "~", time_vbl)

    l_models[names(l_models) != "dids"] <- NA
    tmp <- lapply(levels(d_dat[, batch_vbl]),
                  function(batch) {
                    t_dat <- d_dat[d_dat[, batch_vbl] == batch, ]
                    do.call("lm", list(as.formula(t_formula),
                                       data = as.name("t_dat")))
                  })
    names(tmp) <- levels(d_dat[, batch_vbl])
    l_models[["dids"]] <- tmp

    t_AIC <- t_BIC <- setNames(rep(NA, 3), c("cics", "dics", "dids.pmse"))
  }

  return(list(Models = l_models,
              AIC = t_AIC,
              BIC = t_BIC))
}

#' Extract information from \dQuote{worst case scenario} (wcs) limit lists list
#'
#' The function \code{extract_from_ll_wcsl()} extracts specific elements from
#' a list of lists returned by the \code{\link{get_wcs_limit}()} function.
#'
#' @param ll A list of lists returned by the \code{\link{get_wcs_limit}()}
#'   function. The list must have four elements named \code{"cics"},
#'   \code{"dics"}, \code{dids.pmse} and \code{"dids"}. Each of these elements
#'   has a sub-list of the same length as the number of intercepts. And each
#'   of these elements has a sub-sub-list of the same length as the number of
#'   release limits (\code{rl}).
#' @param element A character string that specifies the element to be extracted,
#'   i.e. either one of  \code{"delta.lim"}, \code{"delta.lim.orig"},
#'   \code{"wcs.lim"} or \code{"wcs.lim.orig"}.
#'
#' @details Information in a bulk list of lists that has been obtained by
#' aid of the function \code{\link{get_wcs_limit}()} for a set of release
#' limit values (\code{rl}) and intercepts.
#'
#' @return A list of the same length as \code{ll_wcsl} is returned. The
#' individual elements of the list are matrices of the values specified by
#' \code{element} that have been extracted from \code{ll}.
#'
#' @seealso \code{\link{get_wcs_limit}}.
#'
#' @keywords internal

extract_from_ll_wcsl <- function(ll, element) {
  if (sum(names(ll) %in% c("cics", "dics", "dids.pmse", "dids")) != 4) {
    stop("The list ll must have four elements named \"cics\", \"dics\" ",
         "\"dids.pmse\" and \"dids\".")
  }
  if (!(element %in%
        c("delta.lim", "delta.lim.orig", "wcs.lim", "wcs.lim.orig"))) {
    stop("Please specify element either as \"delta.lim\", \"delta.lim.orig\", ",
         "\"wcs.lim\" or \"wcs.lim.orig\".")
  }
  if (get_n_list_levels(ll) != 4) {
    stop("The parameter ll must be a list of lists returned by ",
         "get_wcs_limit() at level three.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determination of worst case scenario (wcs) limit(s)

  t_expected_names <-
    c("delta.lim", "delta.lim.orig", "wcs.lim", "wcs.lim.orig")
  t_model_names <- names(ll)

  l_res <- lapply(t_model_names, function(mm) {
    if (mm == "cics") {
      if (get_n_list_levels(ll[[mm]]) == 0) {
        NA
      } else {
        if (any(vapply(ll[[mm]], function(x) {
          sum(t_expected_names %in% names(x[[1]])) != 4
        },
        logical(1)))) {
          stop("The element was not found in one of the sub-elements of ",
               "model ", mm, ". Please provide a list returned ",
               "from get_wcs_limit().")
        } else {
          matrix(vapply(seq_along(ll[[mm]][[1]]), function(j) {
            ll[[mm]][[1]][[j]][[element]]
          },
          numeric(1)),
          nrow = length(ll[[mm]][[1]]), ncol = length(ll[[mm]]),
          dimnames = list(NULL, names(ll[[mm]])))
        }
      }
    } else {
      if (get_n_list_levels(ll[[mm]]) == 0) {
        NA
      } else {
        if (any(vapply(ll[[mm]], function(x) {
          sum(t_expected_names %in% names(x[[1]])) != 4
        },
        logical(1)))) {
          stop("The element was not found in one of the sub-elements of ",
               "model ", mm, ". Please provide a list returned ",
               "from get_wcs_limit().")
        } else {
          matrix(vapply(seq_along(ll[[mm]]), function(bb) {
            vapply(seq_along(ll[[mm]][[1]]), function(j) {
              ll[[mm]][[bb]][[j]][[element]]
            },
            numeric(1))
          }, numeric(length(ll[[mm]][[1]]))),
          nrow = length(ll[[mm]][[1]]), ncol = length(ll[[mm]]),
          dimnames = list(NULL, names(ll[[mm]])))
        }
      }
    }
  })

  names(l_res) <- t_model_names

  return(l_res)
}

#' Extract worst case x value
#'
#' The function \code{extract_wc_x()} extracts the worst case \eqn{x} value
#' from a list of matrices of possible \eqn{x} values based on a list of
#' vectors of indices which specify the worst case elements.
#'
#' @param l1 A list of matrices of \eqn{x} values or a list of lists of one
#'   vectors of \eqn{x} values. The list must have four elements named
#'   \code{"cics"}, \code{"dics"}, \code{"dids.pmse"} and \code{"dids"}.
#' @param l2 A list of vectors of indices. As \code{l1}, the list must have
#'   four elements named \code{"cics"}, \code{"dics"}, \code{"dids.pmse"} and
#'   \code{"dids"}. The length of the vectors of \code{l2} must be equal to
#'   the number of rows of the matrices in \code{l1} or the length of the
#'   vectors in \code{l1}.
#'
#' @details Information from a list of matrices or a list of lists of vectors
#' is extracted by aid of a list of vectors of indices which specify which
#' elements per row of each matrix or which elements of the vectors have to
#' be returned.
#'
#' @return A matrix of the worst case values with the number of rows
#' corresponding to the length of the vectors in \code{l2} and the number of
#' columns corresponding to the length of \code{l1} or \code{l2} is returned.
#'
#' @keywords internal

extract_wc_x <- function(l1, l2) {
  if (!is.list(l1)) {
    stop("The parameter l1 must be a list.")
  }
  if (!is.list(l2)) {
    stop("The parameter l2 must be a list.")
  }
  if (sum(names(l1) %in% c("cics", "dics", "dids.pmse", "dids")) != 4) {
    stop("The list l1 must have four elements named \"cics\", \"dics\" ",
         "\"dids.pmse\" and \"dids\".")
  }
  if (sum(names(l2) %in% c("cics", "dics", "dids.pmse", "dids")) != 4) {
    stop("The list l2 must have four elements named \"cics\", \"dics\" ",
         "\"dids.pmse\" and \"dids\".")
  }
  if (get_n_list_levels(l1) == 1) {
    if (sum(vapply(l1, function(x) {
      !is.matrix(x)
    },
    logical(1))) > 0) {
      stop("The elements of l1 must be matrices or lists of vectors.")
    }
  } else {
    if (sum(vapply(l1, function(x) {
      !is.numeric(x[[1]]) & !is.logical(x[[1]])
    },
    logical(1))) > 0) {
      stop("The elements of l1 must be matrices or lists of vectors.")
    }
  }
  if (sum(vapply(l2, function(x) {
    !is.numeric(x) & !is.logical(x)
  },
  logical(1))) > 0) {
    stop("The elements of l2 must be numeric vectors or vectors of NA.")
  }
  if (sum(vapply(l1, function(x) is.matrix(x), logical(1))) == 4) {
    if (!isTRUE(all.equal(vapply(l1, function(x) nrow(x), numeric(1)),
                          vapply(l2, function(x) length(x), numeric(1))))) {
      stop("The number of rows of the matrices in l1 must be equal ",
           "to the length of the vectors in l2.")
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  m_res <- matrix(NA, nrow = length(l2[[1]]), ncol = length(l1))
  colnames(m_res) <- names(l1)

  for (i in seq_along(l1)) {
    if (names(l1)[i] == "cics") {
      if (is.matrix(l1[["cics"]])) {
        m_res[, "cics"] <- l1[["cics"]]
      }
      if (is.list(l1[["cics"]])) {
        if (is.vector(l1[["cics"]][[length(l1[["cics"]])]]) &&
            length(l1[["cics"]][[length(l1[["cics"]])]]) == 1) {
          m_res[, "cics"] <- rep(unname(l1[[i]][[1]]), length(l2[[1]]))
        }
      }
    } else {
      if (is.matrix(l1[[i]])) {
        m_res[, i] <- vapply(seq_along(l2[[1]]), function(j) {
          ifelse(!is.na(l2[[i]][j]),
                 l1[[i]][j, l2[[i]][j]],
                 NA)
        }, numeric(1))
      }
      if (is.list(l1[[i]])) {
        m_res[, i] <- vapply(seq_along(l2[[1]]), function(j) {
          ifelse(!is.na(l2[[i]][j]),
                 l1[[i]][[1]][l2[[i]][j]],
                 NA)
        },
        numeric(1))
      }
    }
  }

  return(m_res)
}

#' Compile information on worst case batches for ordinary shelf life estimation
#'
#' The function \code{get_osle_poi_list()} prepares a list of points of
#' intersection (POI) for multiple regression models using the
#' \code{find_poi()} function.
#'
#' @inheritParams expirest_osle
#' @inheritParams get_wc_icpt
#' @inheritParams get_poi_list
#'
#' @details The function \code{get_osle_poi_list()} applies the
#' \code{find_poi()} function (find the \dQuote{point of intersection}) on
#' all the models and for each release limit (\code{rl}) provided. With respect
#' to the latter it differs from the \code{\link{get_poi_list}()} function.
#'
#' @return A list with the following elements is returned:
#' \item{all.poi}{A list of the POI values, i.e. a list with one or two list
#'   elements for the side (i.e. \code{lower} or \code{upper}) of the
#'   corresponding specification limit, each containing a list returned by the
#'   \code{\link{get_poi_list}()} function, containing of four elements named
#'   \code{cics}, \code{dics}, \code{dids.pmse} and \code{dids}. Each of them
#'   contains a named vector of the POI values estimated for each batch and
#'   named accordingly.}
#' \item{poi}{A named vector of the worst case POI values of each model, i.e.
#'   named \code{cics}, \code{dics}, \code{dids.pmse} and \code{dids}. In
#'   addition, the vector has an attribute called \code{side} that specifies
#'   the side of the specification limit which is crossed by the confidence or
#'   prediction interval at the corresponding POI value.}
#' \item{wc.icpt}{A named vector of the intercepts of the worst case batches of
#'   each model, i.e. named \code{cics}, \code{dics}, \code{dids.pmse} and
#'   \code{dids}. In addition, the vector has an attribute called \code{side}
#'   that specifies the side of the specification limit which is crossed by the
#'   confidence or prediction interval at the POI value of the corresponding
#'   worst case batch.}
#' \item{which.wc.batch}{A named vector of the indices of the worst case
#'   batches of each model, i.e. named \code{cics}, \code{dics},
#'   \code{dids.pmse} and \code{dids}. In addition, the vector has an attribute
#'   called \code{side} that specifies the side of the specification limit
#'   which is crossed by the confidence or prediction interval at the POI value
#'   of the corresponding worst case batch.}
#'
#' @seealso \code{\link{get_icpt_list}}, \code{\link{get_model_list}},
#' \code{\link{get_poi_list}}, \code{\link{get_wc_icpt}},
#' \code{\link{expirest_osle}}, \code{\link{expirest_wisle}}.
#'
#' @keywords internal

get_osle_poi_list <- function(data, batch_vbl, icpt_list, model_list, sl,
                              srch_range, alpha = 0.05, xform = c("no", "no"),
                              shift = c(0, 0), ivl = "confidence",
                              ivl_type = "one.sided", ivl_side = "lower", ...) {
  if (!is.data.frame(data)) {
    stop("The data must be provided as data frame.")
  }
  if (!is.character(batch_vbl)) {
    stop("The parameter batch_vbl must be a string.")
  }
  if (!(batch_vbl %in% colnames(data))) {
    stop("The batch_vbl was not found in the provided data frame.")
  }
  if (!is.list(icpt_list) ||
      sum(names(icpt_list) %in% c("cics", "dics", "dids.pmse", "dids")) != 4) {
    stop("The icpt_list must be a list with four elements named \"cics\", ",
         "\"dics\", \"dids\" and \"dids.pmse\".")
  }
  if (!is.list(model_list) ||
      sum(names(model_list) %in% c("cics", "dics", "dids.pmse", "dids")) != 4) {
    stop("The model_list must be a list with four elements named \"cics\", ",
         "\"dics\", \"dids\" and \"dids.pmse\".")
  }
  if (!is.numeric(sl) || length(sl) > 2) {
    stop("The parameter sl must be a numeric or vector of length 1 or 2.")
  }
  if (length(sl) == 2) {
    if (sl[2] < sl[1]) {
      stop("The parameter sl must be of the form c(lower, upper).")
    }
  }
  if (!is.numeric(srch_range) || length(srch_range) != 2) {
    stop("The parameter srch_range must be a vector of length 2.")
  }
  if (alpha <= 0 || alpha > 1) {
    stop("Please specify alpha as (0, 1].")
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
  if (!(ivl %in% c("confidence", "prediction"))) {
    stop("Please specify ivl either as \"confidence\" or \"prediction\".")
  }
  if (!(ivl_type %in% c("one.sided", "two.sided"))) {
    stop("Please specify ivl_type either as \"one.sided\" or \"two.sided\".")
  }
  if (!(ivl_side %in% c("lower", "upper", "both"))) {
    stop("Please specify ivl_side either as \"lower\", \"upper\" or \"both\".")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Preparation of data

  d_dat <- droplevels(data)
  l_icpt <- icpt_list
  l_models <- model_list
  t_sides <- c("lower", "upper")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determination of POI values of all models

  if (ivl_side == "both" && length(sl) == 2) {
    l_poi <-
      lapply(seq_along(t_sides), function(i) {
        get_poi_list(data = d_dat, batch_vbl = batch_vbl, model_list = l_models,
                     srch_range = srch_range, sl = sl[i], mode = "minimal",
                     alpha = alpha, ivl = ivl, ivl_type = ivl_type,
                     ivl_side = t_sides[i])
      })
    names(l_poi) <- t_sides

    # Extraction of all worst case POI values
    d_poi <-
      rbind(lower = vapply(l_poi$lower, function(x) {
        ifelse(all(is.na(x)), NA, min(x, na.rm = TRUE))
      },
      numeric(1)),
      upper = vapply(l_poi$upper, function(x) {
        ifelse(all(is.na(x)), NA, min(x, na.rm = TRUE))
      },
      numeric(1)))
    d_poi[is.infinite(d_poi)] <- NA

    # Determination of the side of the worst case POI value
    t_poi_side <- t_sides[(d_poi["upper", ] < d_poi["lower", ]) + 1L]
    names(t_poi_side) <- names(l_models)

    # Summary vector of the worst case POI values
    t_poi <- vapply(seq_along(colnames(d_poi)), function(i) {
      ifelse(is.na(t_poi_side[i]), NA, d_poi[t_poi_side[i], i])
    },
    numeric(1))
    names(t_poi) <- colnames(d_poi)
    attr(t_poi, "side") <- t_poi_side
  } else {
    l_poi <- setNames(list(NA), ivl_side)
    l_poi[[ivl_side]] <-
      get_poi_list(data = d_dat, batch_vbl = batch_vbl, model_list = l_models,
                   srch_range = srch_range, sl = sl, mode = "minimal",
                   alpha = alpha, ivl = ivl, ivl_type = ivl_type,
                   ivl_side = ivl_side)

    # "Determination" of the side of the worst case POI value
    t_poi_side <- rep(ivl_side, length(l_models))
    names(t_poi_side) <- names(l_models)

    # Summary vector of the worst case POI values
    t_poi <- vapply(l_poi[[ivl_side]], function(x) {
      ifelse(all(is.na(x)), NA, min(x, na.rm = TRUE))
    },
    numeric(1))
    t_poi[is.infinite(t_poi)] <- NA
    attr(t_poi, "side") <- t_poi_side
  }

  if (nlevels(d_dat[, batch_vbl]) > 1) {
    if (sum(is.na(t_poi)) != 0) {
      if (min(srch_range) == 0) {
        warning("Not for all model types POI values obtained. ",
                "Possibly, changing srch_range could solve the issue ",
                "(a lower limit > 0 might be a solution).")
      } else {
        warning("Not for all model types POI values obtained. ",
                "Possibly, changing srch_range could solve the issue. ")
      }
    }
  } else {
    if (is.na(t_poi["dids"])) {
      if (min(srch_range) == 0) {
        warning("No POI value was obtained. ",
                "Possibly, changing srch_range could solve the issue ",
                "(a lower limit > 0 might be a solution).")
      } else {
        warning("No POI value was obtained. ",
                "Possibly, changing srch_range could solve the issue. ")
      }
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determination of worst case batch (wc_batch) and its intercept (wc_icpt)

  # In case of cics model: wc_icpt is the common intercept of all batches
  #   and none of the batches is the worst case batch and thus NA.

  # Worst case batch (of each model)
  if (ivl_side == "both" && length(sl) == 2) {
    l_wc_batch <-
      lapply(l_poi, function(ll) {
        vapply(ll, function(x) {
          ifelse(!length(which.min(x)), NA, which.min(x))
        },
        numeric(1))
      })

    l_wc_batch$lower["cics"] <- NA
    l_wc_batch$upper["cics"] <- NA

    # Summary vector of the worst case batches
    wc_batch <- vapply(seq_along(names(l_models)), function(i) {
      ifelse(is.na(t_poi_side[i]), NA, l_wc_batch[[t_poi_side[i]]][i])
    },
    numeric(1))
    attr(wc_batch, "side") <- t_poi_side
  } else {
    wc_batch <- vapply(l_poi[[ivl_side]], function(x) {
      ifelse(!length(which.min(x)), NA, which.min(x))
    },
    numeric(1))

    wc_batch["cics"] <- NA
    attr(wc_batch, "side") <- t_poi_side
  }

  # Intercept of the worst case batch (of each model)
  if (ivl_side == "both" && length(sl) == 2) {
    l_wc_icpt <-
      lapply(t_sides, function(side) {
        get_wc_icpt(data = d_dat, batch_vbl = batch_vbl,
                    icpt_list = l_icpt, poi_list = l_poi[[side]],
                    wc_batch = l_wc_batch[[side]], xform = xform)
      })
    names(l_wc_icpt) <- t_sides

    # Summary vector of the intercepts of the worst case batches
    wc_icpt <- vapply(seq_along(names(l_models)), function(i) {
      ifelse(is.na(t_poi_side[i]), NA, l_wc_icpt[[t_poi_side[i]]][i])
    },
    numeric(1))
    attr(wc_icpt, "side") <- t_poi_side
  } else {
    wc_icpt <- get_wc_icpt(data = d_dat, batch_vbl = batch_vbl,
                           icpt_list = l_icpt, poi_list = l_poi[[ivl_side]],
                           wc_batch = wc_batch, xform = xform)
    attr(wc_icpt, "side") <- t_poi_side
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compile and return results

  return(list(all.poi = l_poi,
              poi = t_poi,
              wc.icpt = wc_icpt,
              which.wc.batch = wc_batch))
}

#' Compile information on worst case batches for what-if shelf life estimation
#'
#' The function \code{get_wisle_poi_list()} prepares a list of points of
#' intersection (POI) for multiple regression models and release limits
#' using the \code{find_poi()} function.
#'
#' @inheritParams expirest_wisle
#' @inheritParams get_wc_icpt
#' @inheritParams get_poi_list
#'
#' @details The function \code{get_wisle_poi_list()} applies the
#' \code{find_poi()} function (find the \dQuote{point of intersection}) on
#' all the models and for each release limit (\code{rl}) provided. With respect
#' to the latter it differs from the \code{\link{get_poi_list}()} function.
#'
#' @return A list with the following elements is returned:
#' \item{all.wcsl}{A list of the worst case scenario (wcs) limits with a list
#'   of four elements for each linear model named \code{cics}, \code{dics},
#'   \code{dids.pmse} and \code{dids}. Each of these elements contains a list
#'   element for each batch (intercept) containing itself a list element for
#'   each release limit. The wcs limit is obtained by adding/subtracting the
#'   absolute difference of specification limit and release limit to/from the
#'   common intercept of the test batches or the intercept of the worst
#'   performing batch.}
#' \item{all.poi}{A list of the POI values, i.e. a list with a list of four
#'   elements for each linear model named \code{cics}, \code{dics},
#'   \code{dids.pmse} and \code{dids}. Each of these elements contains a
#'   matrix with columns for each intercept and rows for each release limit.}
#' \item{all.prl}{A list of the confidence or prediction interval limits that
#'   are associated with a POI value, i.e. a list with a list of four elements
#'   for each linear model named \code{cics}, \code{dics}, \code{dids.pmse}
#'   and \code{dids}. Each of these elements contains an array with a level for
#'   each batch, containing matrices with columns for each batch and rows for
#'   each release limit, where the matrices contain the estimated interval
#'   limits at each POI value per batch.}
#' \item{which.min.dist}{A list of four elements for each linear model named
#'   \code{cics}, \code{dics}, \code{dids.pmse} and \code{dids}. Each of these
#'   list elements contains a matrix of the indices of the batches with the
#'   minimal intercept in the \code{all.prl} list. The matrices have a column
#'   for each batch and a row for each release limit.}
#' \item{which.min.poi}{A list of four elements for each linear model named
#'   \code{cics}, \code{dics}, \code{dids.pmse} and \code{dids}. Each of these
#'   list elements contains a numeric vector with the minimal POI value of
#'   associated with each release limit.}
#' \item{which.wc.batch}{A list of four elements for each linear model named
#'   \code{cics}, \code{dics}, \code{dids.pmse} and \code{dids}. Each of these
#'   list elements contains a numeric vector with the indices of the worst
#'   case batches associated with each release limit.}
#'
#' @seealso \code{\link{get_icpt_list}}, \code{\link{get_model_list}},
#' \code{\link{get_wcs_limit}}, \code{\link{find_poi}},
#' \code{\link{get_intvl_limit}}, \code{\link{expirest_osle}},
#' \code{\link{expirest_wisle}}.
#'
#' @keywords internal

get_wisle_poi_list <- function(icpt_list, model_list, rl, sl, srch_range,
                               alpha = 0.05, xform = c("no", "no"),
                               shift = c(0, 0), ivl = "confidence",
                               ivl_type = "one.sided",
                               ivl_side = "lower", ...) {
  if (!is.list(icpt_list) ||
      sum(names(icpt_list) %in% c("cics", "dics", "dids.pmse", "dids")) != 4) {
    stop("The icpt_list must be a list with four elements named \"cics\", ",
         "\"dics\", \"dids.pmse\" and \"dids\".")
  }
  if (!is.list(model_list) ||
      sum(names(model_list) %in% c("cics", "dics", "dids.pmse", "dids")) != 4) {
    stop("The model_list must be a list with four elements named \"cics\", ",
         "\"dics\", \"dids\" and \"dids.pmse\".")
  }
  if (!is.numeric(rl)) {
    stop("The parameter rl must be a numeric.")
  }
  if (!is.numeric(sl) || length(sl) > 1) {
    stop("The parameter sl must be a numeric value of length 1.")
  }
  if (!is.numeric(srch_range) || length(srch_range) != 2) {
    stop("The parameter srch_range must be a vector of length 2.")
  }
  if (alpha <= 0 || alpha > 1) {
    stop("Please specify alpha as (0, 1].")
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
  if (!(ivl %in% c("confidence", "prediction"))) {
    stop("Please specify ivl either as \"confidence\" or \"prediction\".")
  }
  if (!(ivl_type %in% c("one.sided", "two.sided"))) {
    stop("Please specify ivl_type either as \"one.sided\" or \"two.sided\".")
  }
  if (!(ivl_side %in% c("lower", "upper"))) {
    stop("Please specify ivl_side either as \"lower\" or \"upper\".")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Preparation of data

  l_icpt <- icpt_list
  l_models <- model_list

  # Preliminary definition of the lists that will be required below
  l_poi <- l_prl <- l_wc_batch <- setNames(rep(list(NA), 4), names(l_models))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determination of worst case scenario (wcs) limits for all intercepts of
  # all models (on the transformed scale, if data have been transformed)

  # List of all wcs_limit lists
  ll_wcsl <- lapply(seq_along(l_icpt),
                    function(i) {
                      if (get_n_list_levels(l_icpt[[i]]) != 0) {
                        lapply(l_icpt[[i]]$icpt, function(xx) {
                          lapply(rl, function(j) {
                            get_wcs_limit(rl = j, sl = sl, intercept = xx,
                                          xform = xform, shift = shift,
                                          ivl_side = ivl_side)
                          })
                        })
                      } else {
                        NA
                      }
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

  for (variety in names(l_wcsl)) {
    if (get_n_list_levels(l_icpt[[variety]]) != 0) {
      # Initialise empty arrays
      m_poi <- matrix(NA,
                      nrow = length(rl),
                      ncol = length(l_icpt[[variety]][["icpt"]]))
      colnames(m_poi) <- names(l_icpt[[variety]][["icpt"]])

      a_prl <- array(NA,
                     dim = c(length(rl), length(l_icpt[[variety]][["icpt"]]),
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
                       model = l_models[["dids"]][[k]],
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
                    model = l_models[["dids"]][[kk]], alpha = alpha,
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
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determination of the batches with the confidence or prediction interval
  # limits that are closest to the respective specification limit for each
  # model and each POI

  switch(ivl_side,
         "lower" = {
           l_min_dist <- lapply(l_prl, FUN = function(x) {
             if (is.logical(x)) {
               NA
             } else {
               apply(x, c(1, 2), FUN = function(y) {
                 ifelse(length(which.min(y)) != 0, which.min(abs(y)), NA)
               })
             }
           })
         },
         "upper" = {
           l_min_dist <- lapply(l_prl, FUN = function(x) {
             if (is.logical(x)) {
               NA
             } else {
               apply(x, c(1, 2), FUN = function(y) {
                 ifelse(length(which.max(y)) != 0, which.max(abs(y)), NA)
               })
             }
           })
         })

  # Determination of the smallest POI value for each model and each rl value
  l_min_poi <- lapply(l_poi, FUN = function(x) {
    if (is.logical(x)) {
      NA
    } else {
      apply(x, 1, function(y) {
        ifelse(length(which.min(y)) != 0, which.min(y), NA)
      })
    }
  })

  # Determination of the worst case batches for each model and each rl value:
  #   The worst case batches are the ones with the confidence or prediction
  #   interval limits that are closest to the respective specification limit
  #   where the POI values are smallest.
  # In case of cics model: wc_icpt is the common intercept of all batches
  #   and none of the batches is the worst case batch.

  for (i in seq_along(l_min_dist)) {
    if (!is.logical(l_min_dist[[i]])) {
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
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compile and return results

  return(list(all.wcsl = ll_wcsl,
              all.poi = l_poi,
              all.prl = l_prl,
              which.min.dist = l_min_dist,
              which.min.poi = l_min_poi,
              which.wc.batch = l_wc_batch))
}

#' Compile \dQuote{what-if shelf life estimation} (wisle) assessment results
#'
#' The function \code{compile_wisle_summary()} extracts results from various
#' lists that are generated during the wisle estimation and compiles a
#' summary data frame.
#'
#' @param wcsl_list A list of four elements named \code{cics}, \code{dics},
#'   \code{dids.pmse} and \code{dids} with the worst case scenario limits
#'   of each batch and linear regression model. The \code{cics}, \code{dics}
#'   and \code{dids.pmse} elements are \code{NA} if data of only a single
#'   batch is available.
#' @param wcb_list A list of four elements named \code{cics}, \code{dics},
#'   \code{dids.pmse} and \code{dids} with the indices of the worst case
#'   batches. The \code{cics}, \code{dics} and \code{dids.pmse} elements are
#'   \code{NA} if data of only a single batch is available.
#' @param poi_ich A numeric named vector of the POI values of the worst case
#'   batches of each model.
#' @inheritParams expirest_osle
#' @inheritParams get_wc_icpt
#' @inheritParams get_relevant_limits
#'
#' @details Information stored in multiple lists that are generated during the
#' \dQuote{what-if shelf life estimation} is extracted and compiled in a single
#' data frame.
#'
#' @return A list with two element is returned, containing the following
#' elements:
#' \item{wc.icpt}{A data frame of the worst case intercepts of each of the
#'   four fitted models.}
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
#' @seealso \code{\link{extract_wc_x}}, \code{\link{extract_from_ll_wcsl}},
#' \code{\link{expirest_wisle}}.
#'
#' @keywords internal

compile_wisle_summary <- function(data, batch_vbl, rl, poi_list, icpt_list,
                                  wcsl_list, wcb_list, limits_list, poi_ich,
                                  xform = c("no", "no"), shift = c(0, 0)) {
  # This function replaces the following section in expirest_wisle()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Collection of data and compilation of summary data frame

  if (!is.data.frame(data)) {
    stop("The data must be provided as data frame.")
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
  if (!is.list(poi_list) ||
      sum(names(poi_list) %in% c("cics", "dics", "dids.pmse", "dids")) != 4) {
    stop("The poi_list must be a list with four elements named \"cics\", ",
         "\"dics\", \"dids.pmse\" and \"dids\".")
  }
  if (!is.list(icpt_list) ||
      sum(names(icpt_list) %in% c("cics", "dics", "dids.pmse", "dids")) != 4) {
    stop("The icpt_list must be a list with four elements named \"cics\", ",
         "\"dics\", \"dids.pmse\" and \"dids\".")
  }
  if (!is.list(wcsl_list) ||
      sum(names(wcsl_list) %in% c("cics", "dics", "dids.pmse", "dids")) != 4) {
    stop("The wcsl_list must be a list with four elements named \"cics\", ",
         "\"dics\", \"dids.pmse\" and \"dids\".")
  }
  if (!is.list(wcb_list) ||
      sum(names(wcb_list) %in% c("cics", "dics", "dids.pmse", "dids")) != 4) {
    stop("The wcb_list must be a list with four elements named \"cics\", ",
         "\"dics\", \"dids.pmse\" and \"dids\".")
  }
  if (!is.list(limits_list) ||
      sum(names(limits_list) %in% c("sl.orig", "sl", "rl.orig", "rl")) != 4) {
    stop("The limits_list must be a list with at least the four elements ",
         "named \"sl.orig\", \"sl\", \"rl.orig\" and \"rl\".")
  }
  if (!is.numeric(poi_ich) || length(poi_ich) != 4) {
    stop("The parameter poi_ich must be a vector of length 4.")
  }
  if (!all((names(poi_ich) %in% c("cics", "dics", "dids", "dids.pmse")))) {
    stop("The parameter poi_ich must be a vector with the names \"cics\", ",
         "\"dics\", \"dids\" or \"dids.pmse\".")
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

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Preparation of data

  d_dat <- droplevels(data)
  l_poi <- poi_list
  l_icpt <- icpt_list
  ll_wcsl <- wcsl_list
  l_wc_batch <- wcb_list

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Matrix of the worst case POI values for each model and each rl value

  if (nlevels(d_dat[, batch_vbl]) > 1) {
    m_poi <- extract_wc_x(l1 = l_poi, l2 = l_wc_batch)
  } else {
    m_poi <- matrix(NA, nrow = length(rl), ncol = length(l_wc_batch))
    colnames(m_poi) <- names(l_wc_batch)

    m_poi[, "dids"] <- as.numeric(l_poi[["dids"]])
  }

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
  # Worst case intercepts (wc_icpt_argpm) (on the original scale)

  if (nlevels(d_dat[, batch_vbl]) > 1) {
    if (xform[2] == "no") {
      wc_icpt_argpm <- extract_wc_x(l1 = l_icpt, l2 = l_wc_batch)
    } else {
      l_icpt_sub <- lapply(l_icpt, function(x) list(x$icpt.orig))

      wc_icpt_argpm <- extract_wc_x(l1 = l_icpt_sub, l2 = l_wc_batch)
    }
  } else {
    wc_icpt_argpm <- matrix(NA, nrow = length(rl), ncol = length(l_wc_batch))
    colnames(wc_icpt_argpm) <- names(l_wc_batch)

    if (xform[2] == "no") {
      wc_icpt_argpm[, "dids"] <- as.numeric(l_icpt[["dids"]][["icpt"]])
    } else {
      wc_icpt_argpm[, "dids"] <- as.numeric(l_icpt[["dids"]][["icpt.orig"]])
    }
  }

  # ---------
  # Delta and WCSL

  if (xform[2] == "no") {
    l_delta <- extract_from_ll_wcsl(ll_wcsl, "delta.lim")
    l_wcsl <- extract_from_ll_wcsl(ll_wcsl, "wcs.lim")

    if (nlevels(d_dat[, batch_vbl]) > 1) {
      m_delta <- extract_wc_x(l1 = l_delta, l2 = l_wc_batch)
      m_wcsl <- extract_wc_x(l1 = l_wcsl, l2 = l_wc_batch)
    } else {
      m_delta <- matrix(NA, nrow = length(rl), ncol = length(l_wc_batch))
      colnames(m_delta) <- names(l_wc_batch)
      m_wcsl <- m_delta

      m_delta[, "dids"] <- as.numeric(l_delta[["dids"]])
      m_wcsl[, "dids"] <- as.numeric(l_wcsl[["dids"]])
    }
  } else {
    l_delta_orig <- extract_from_ll_wcsl(ll_wcsl, "delta.lim.orig")
    l_wcsl_orig <- extract_from_ll_wcsl(ll_wcsl, "wcs.lim.orig")

    if (nlevels(d_dat[, batch_vbl]) > 1) {
      m_delta <- extract_wc_x(l1 = l_delta_orig, l2 = l_wc_batch)
      m_wcsl <- extract_wc_x(l1 = l_wcsl_orig, l2 = l_wc_batch)
    } else {
      m_delta <- matrix(NA, nrow = length(rl), ncol = length(l_wc_batch))
      colnames(m_delta) <- names(l_wc_batch)
      m_wcsl <- m_delta

      m_delta[, "dids"] <- as.numeric(l_delta_orig[["dids"]])
      m_wcsl[, "dids"] <- as.numeric(l_wcsl_orig[["dids"]])
    }
  }

  # ---------
  # Summary data frame compilation

  d_poi <- data.frame(
    Intercept.cics = wc_icpt_argpm[, "cics"],
    Intercept.dics = wc_icpt_argpm[, "dics"],
    Intercept.dids = wc_icpt_argpm[, "dids"],
    Intercept.dids.pmse = wc_icpt_argpm[, "dids.pmse"],
    Delta.cics = m_delta[, "cics"],
    Delta.dics = m_delta[, "dics"],
    Delta.dids = m_delta[, "dids"],
    Delta.dids.pmse = m_delta[, "dids.pmse"],
    WCSL.cics = m_wcsl[, "cics"],
    WCSL.dics = m_wcsl[, "dics"],
    WCSL.dids = m_wcsl[, "dids"],
    WCSL.dids.pmse = m_wcsl[, "dids.pmse"],
    Exp.Spec.Report = rep(limits_list[["sl.orig"]], nrow(m_poi)),
    Exp.Spec = rep(limits_list[["sl"]], nrow(m_poi)),
    Rel.Spec.Report = limits_list[["rl.orig"]],
    Rel.Spec = limits_list[["rl"]],
    Shelf.Life.cics = m_poi[, "cics"],
    Shelf.Life.dics = m_poi[, "dics"],
    Shelf.Life.dids = m_poi[, "dids"],
    Shelf.Life.dids.pmse = m_poi[, "dids.pmse"],
    POI.Model.cics = rep(poi_ich["cics"], nrow(m_poi)),
    POI.Model.dics = rep(poi_ich["dics"], nrow(m_poi)),
    POI.Model.dids = rep(poi_ich["dids"], nrow(m_poi)),
    POI.Model.dids.pmse = rep(poi_ich["dids.pmse"], nrow(m_poi)))

  if (xform[2] != "no") {
    d_poi[, "Exp.Spec"] <- rep(limits_list[["sl.bt"]], nrow(m_poi))
    d_poi[, "Rel.Spec"] <- limits_list[["rl.bt"]]
  }

  rownames(d_poi) <- NULL

  # ---------
  # Compile and return results

  return(list(wc.icpt = wc_icpt_argpm,
              POI = d_poi))
}

#' Print value(s)
#'
#' The function \code{print_val()} generates a character string for the purpose
#' to print a value on a plot (together with associated information).
#'
#' @param val_name A character string that specifies the text preceding the
#'   value of the parameter to be displayed.
#' @param val_value A numeric value that specifies the value of the parameter
#'   to be displayed.
#' @param val_unit A character string that specifies the text following the
#'   value of the parameter to be displayed.
#' @param val_sf A positive integer that specifies the number of significant
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
#' @seealso \code{\link{get_text_annotation}}, \code{\link{get_n_whole_part}},
#' \code{\link[base]{formatC}}, \code{\link[base]{paste}}.
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
  if (!is.numeric(val_value) && !is.na(val_value)) {
    stop("The parameter val_value must be a numeric value of length 1.")
  }
  if (!is.character(val_unit)) {
    stop("The parameter val_unit must be a string.")
  }
  if (!is.numeric(val_sf) || length(val_sf) > 1) {
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

  if (is.na(val_value)) {
    paste(prefix, val_name,
          formatC(signif(val_value, val_sf), digits = val_sf, format = "fg",
                  flag = "#"),
          val_unit, suffix, sep = "")
  } else {
    if (val_sf <= get_n_whole_part(val_value)) {
      paste(prefix, val_name,
            formatC(signif(val_value, val_sf), digits = val_sf, format = "fg"),
            val_unit, suffix, sep = "")
    } else {
      paste(prefix, val_name,
            formatC(signif(val_value, val_sf), digits = val_sf, format = "fg",
                    flag = "#"),
            val_unit, suffix, sep = "")
    }
  }
}

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
#' @seealso \code{\link{print_val}}, \code{\link{set_limits}},
#' \code{\link{get_text_annotation}}.
#'
#' @keywords internal

get_n_whole_part <- function(x) {
  if (!is.numeric(x) && all(!is.na(x))) {
    stop("The parameter x must be a numeric value or NA.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determine n

  check <- function(xx) {
    if (is.na(xx)) {
      NA
    } else {
      if (xx <= 1 && xx >= -1) {
        1
      } else {
        floor(log10(abs(xx))) + 1
      }
    }
  }

  vapply(x, function(xx) check(xx), numeric(1))
}

#' Determine the level of nesting of a list
#'
#' The function \code{get_n_list_levels()} determines the number of levels of
#' a nested list.
#'
#' @param x A list.
#'
#' @details The function \code{get_n_list_levels()} determines the number of
#' levels of a (nested) list.
#'
#' @return An integer representing the number of levels of the list. If an
#' object is passed on to \code{x} that is not a list \code{0} is returned.
#'
#' @seealso \code{\link{extract_from_ll_wcsl}}, \code{\link{extract_wc_x}},
#' \code{\link{get_wisle_poi_list}}.
#'
#' @keywords internal

get_n_list_levels <- function(x) {
  if (is.list(x)) {
    1L + max(vapply(x, get_n_list_levels, numeric(1)))
  } else {
   0L
  }
}

#' Prepare text annotation
#'
#' The function \code{get_text_annotation()} prepares a data frame for putting
#' text on a plot prepared by the \code{ggplot()} function from
#' the \sQuote{\code{ggplot2}} package.
#'
#' @param rvu A character string that specifies the unit associated with the
#'   response variable.
#' @param x_range A numeric vector of the form \code{c(min, max)} that
#'   specifies the range of the time variable to be plotted.
#' @param y_range A numeric vector of the form \code{c(min, max)} that
#'   specifies the range of the response variable to be plotted.
#' @param sl A numeric value or a numeric vector of length \code{2} that
#'   specifies the specification limit or limits.
#' @param sl_sf A positive integer or a vector of positive integers that
#'   specifies the number of \dQuote{significant figures} (sf) of \code{sl}.
#' @param poi_model Point of intersection (POI) with the upper or lower
#'   confidence or prediction interval of the most appropriate model according
#'   to the ICH Q1E guideline.
#' @param ivl_side A character string that specifies if the \dQuote{upper} or
#'   the \dQuote{lower} limit is the relevant limit, i.e. either \code{"upper"}
#'   or \code{"lower"}, respectively.
#' @param poi_woca Point of intersection (POI) with the upper or lower
#'   confidence or prediction interval of the linear regression model
#'   representing the worst case scenario (woca) model. The default is
#'   \code{NULL}.
#' @param wisle_est A data frame of the intercepts, the differences between
#'   release and shelf life limits, the worst case scenario limits (WCSLs),
#'   the expiry and release specification limits, the shelf lives and POI
#'   values. The default is \code{NULL}.
#' @param wc_icpt A data frame of the worst case intercepts of each of the
#'   four fitted models. The default is \code{NULL}.
#' @param rl_sf A positive integer or a vector of positive integers that
#'   specifies the number of \dQuote{significant figures} (sf) of \code{rl}.
#'   The default is \code{NULL}.
#' @param rl_index A positive integer that specifies which of the release limit
#'   values that have been handed over to \code{\link{expirest_wisle}()} should
#'   be displayed. The default is \code{NULL}.
#' @param wcsl_model_name A character string that specifies the name of the
#'   model with the worst case scenario limit. The default is \code{NULL}.
#' @param plot_option A character string of either \code{"full"}, \code{"lean"},
#'   \code{"lean1"}, \code{"lean2"}, \code{"basic1"} and \code{"basic2"},
#'   that specifies if additional information should be shown in the plot
#'   (option \code{"full"}) or only basic information (options \code{"lean"}
#'   and \code{"basic"}). Full means the data points, the fitted regression
#'   line with the confidence or prediction interval, the specification
#'   limit(s) and the estimated shelf life. For \sQuote{expirest_osle} objects,
#'   only the options \code{"full"} and \code{"lean"} are relevant. The default
#'   is \code{"full"}.
#'
#' @details The function \code{get_text_annotation()} expects various pieces
#' of information characterising an \sQuote{\code{expirest_osle}} or an
#' \sQuote{\code{expirest_wisle}} model. Based on the information provided,
#' the function prepares a data frame that can be handed over to the
#' \code{geom_text()} function from the \sQuote{\code{ggplot2}} package.
#'
#' @return A data frame with the columns Time, Response, Label and Colour
#' is returned.
#'
#' @seealso \code{\link{plot_expirest_osle}}, \code{\link{plot_expirest_wisle}},
#' \code{\link{print_val}}, \code{\link[ggplot2]{ggplot}},
#' \code{\link[ggplot2]{geom_text}}.
#'
#' @keywords internal

get_text_annotation <- function(rvu, x_range, y_range, sl, sl_sf, poi_model,
                                ivl_side, poi_woca = NULL, wisle_est = NULL,
                                wc_icpt = NULL, rl_sf = NULL, rl_index = NULL,
                                wcsl_model_name = NULL, plot_option = "full") {
  if (!is.character(rvu)) {
    stop("The parameter rvu must be a string.")
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
  if (!is.numeric(poi_model) && !is.na(poi_model) || length(poi_model) > 1) {
    stop("The parameter poi_model must be a numeric of length 1.")
  }
  if (!(ivl_side %in% c("lower", "upper", "both"))) {
    stop("Please specify ivl_side either as \"lower\", \"upper\" or \"both\".")
  }

  if (!is.null(poi_woca)) {
    if (!is.numeric(poi_woca) && !is.na(poi_woca) || length(poi_woca) > 1) {
      stop("The parameter poi_woca must be a numeric of length 1.")
    }
  }
  if (!is.null(wisle_est)) {
    if (!is.data.frame(wisle_est) || ncol(wisle_est) != 24) {
      stop("The parameter wisle_est must be a data frame with 24 columns.")
    }
  }
  if (!is.null(wc_icpt)) {
    if (!is.numeric(wc_icpt) || length(wc_icpt) > 1) {
      stop("The parameter wc_icpt must be a numeric of length 1.")
    }
  }
  if (!is.null(rl_sf) && !is.null(wisle_est)) {
    if (!is.numeric(rl_sf) && all(!is.na(rl_sf))) {
      stop("The parameter rl_sf must be a positive integer of the same length ",
           "as the parameter wisle_est has rows.")
    }
    if (sum(rl_sf < 0) > 0) {
      stop("The parameter rl_sf must be a positive integer of the same length ",
           "as the parameter wisle_est has rows.")
    }
    if (length(rl_sf) != nrow(wisle_est)) {
      stop("The parameter rl_sf must be a positive integer of the same length ",
           "as the parameter wisle_est has rows.")
    }
    if (!isTRUE(all.equal(rl_sf, as.integer(rl_sf)))) {
      stop("The parameter rl_sf must be a positive integer of the same length ",
           "as the parameter wisle_est has rows.")
    }
  }
  if (!is.null(rl_index) && !is.null(wisle_est)) {
    if (!is.numeric(rl_index) || length(rl_index) > 1) {
      stop("The parameter rl_index must be a positive integer of length 1.")
    }
    if (rl_index != as.integer(rl_index)) {
      stop("The parameter rl_index must be a positive integer of length 1.")
    }
    if (rl_index < 1 || rl_index > nrow(wisle_est)) {
      stop("The parameter rl_index must be between 1 and the number of rows ",
           "of the parameter wisle_est.")
    }
  }
  if (!is.null(wcsl_model_name)) {
    if (!is.character(wcsl_model_name) || length(wcsl_model_name) != 1) {
      stop("The parameter wcsl_model_name must be a single string.")
    }
  }
  if (!(plot_option %in% c("full", "lean1", "lean2", "basic1", "basic2"))) {
    stop("Please specify plot_option either as \"full\", \"lean1\", ",
         "\"lean2\", \"basic1\" or \"basic2\".")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Preparation of data

  y_breaks <- pretty(y_range, 5)
  t_exp <- wisle_est

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (is.null(poi_woca)) {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Display of text elements for plot_expirest_osle()
    # The rows in data frame d_text have the following meaning and position
    # (position in brackets):
    # LSL (lower right), USL (upper right), POI model (low at poi.model)

    if (length(sl) == 2) {
      d_text <- data.frame(
        Time = c(rep(x_range[2], 2), poi_model),
        Response = c(sl, sl[1]),
        Label = c(print_val("LSL: ", sl[1], rvu, sl_sf[1]),
                  print_val("USL: ", sl[2], rvu, sl_sf[2]),
                  print_val("", poi_model, "",
                            get_n_whole_part(poi_model) + 1)),
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
  } else {
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Display of text elements for plot_expirest_wisle()
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
        Colour = c("black", "black", "red", "royalblue", "forestgreen",
                   "grey50"),
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
                 Colour = c("black", "red", "royalblue", "forestgreen",
                            "grey50", "grey0"),
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
                 Colour = c("black", "red", "royalblue", "forestgreen",
                            "grey50", "grey0"),
                 stringsAsFactors = FALSE)
               d_text$Response <- d_text$Response +
                 c(diff(y_breaks[1:2]), 0, 0,
                   rep(diff(y_breaks[1:2]), 2),
                   diff(y_breaks[1:2])) * 1 / c(10, 1, 1, 2, 2, 10)
             })
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  return(d_text)
}

#' Prepare horizontal lines
#'
#' The function \code{get_hlines()} prepares a data frame for putting
#' horizontal lines on a plot prepared by the \code{ggplot()} function from
#' the \sQuote{\code{ggplot2}} package
#'
#' @inheritParams get_text_annotation
#'
#' @details The function \code{get_hlines()} expects various pieces
#' of information characterising an \sQuote{\code{expirest_osle}} or an
#' \sQuote{\code{expirest_wisle}} model. Based on the information provided,
#' the function prepares a data frame that can be handed over to the
#' \code{geom_hline()} function from the \sQuote{\code{ggplot2}} package.
#'
#' @return A data frame with the columns Response, Item, Colour and Type
#' is returned.
#'
#' @seealso \code{\link{plot_expirest_osle}}, \code{\link{plot_expirest_wisle}},
#' \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{geom_text}}.
#'
#' @keywords internal

get_hlines <- function(sl, ivl_side) {
  if (!is.numeric(sl) || length(sl) > 2) {
    stop("The parameter sl must be a numeric or vector of length 1 or 2.")
  }
  if (length(sl) == 2) {
    if (sl[2] < sl[1]) {
      stop("The parameter sl must be of the form c(lower, upper).")
    }
  }
  if (!(ivl_side %in% c("lower", "upper", "both"))) {
    stop("Please specify ivl_side either as \"lower\", \"upper\" or \"both\".")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

  return(d_hlines)
}

#' Prepare segments explaining graphical elements
#'
#' The function \code{get_segments()} prepares a data frame for putting
#' segments on a plot prepared by the \code{ggplot()} function from
#' the \sQuote{\code{ggplot2}} package
#'
#' @param rl A numeric value or a numeric vector that specifies the release
#'   specification limit(s) for which the corresponding expiry should be
#'   estimated.
#' @param sl_model_name A character string that specifies the name of the
#'   model with the shelf life limit of interest.
#' @inheritParams get_text_annotation
#'
#' @details The function \code{get_segments()} expects various pieces
#' of information characterising an \sQuote{\code{expirest_osle}} or an
#' \sQuote{\code{expirest_wisle}} model. Based on the information provided,
#' the function prepares a data frame that can be handed over to the
#' \code{geom_vline()} function from the \sQuote{\code{ggplot2}} package.
#'
#' @return A data frame with the columns Time.1, Time.2, Response.1,
#' Response.2, Item, Colour, Type, Size. Time.1 and Time.2 represent the
#' maximal allowed difference over time from intercept and Response.1 and
#' Response.2 represent the release limit.
#'
#' @seealso \code{\link{plot_expirest_osle}}, \code{\link{plot_expirest_wisle}},
#' \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{geom_text}}.
#'
#' @keywords internal

get_segments <- function(sl, ivl_side, wisle_est, rl, rl_index, poi_woca,
                         wc_icpt, x_range, sl_model_name, wcsl_model_name) {
  if (!is.numeric(sl) || length(sl) > 2) {
    stop("The parameter sl must be a numeric or vector of length 1 or 2.")
  }
  if (length(sl) == 2) {
    if (sl[2] < sl[1]) {
      stop("The parameter sl must be of the form c(lower, upper).")
    }
  }
  if (!(ivl_side %in% c("lower", "upper", "both"))) {
    stop("Please specify ivl_side either as \"lower\", \"upper\" or \"both\".")
  }
  if (!is.data.frame(wisle_est) || ncol(wisle_est) != 24) {
    stop("The parameter wisle_est must be a data frame with 24 columns.")
  }
  if (!is.numeric(rl)) {
    stop("The parameter rl must be a numeric.")
  }
  if (length(rl) != nrow(wisle_est)) {
    stop("The parameter rl must be a positive integer of the same length ",
         "as the parameter wisle_est has rows.")
  }
  if (!is.numeric(rl_index) || length(rl_index) > 1) {
    stop("The parameter rl_index must be a positive integer of length 1.")
  }
  if (rl_index != as.integer(rl_index)) {
    stop("The parameter rl_index must be a positive integer of length 1.")
  }
  if (rl_index < 1 || rl_index > nrow(wisle_est)) {
    stop("The parameter rl_index must be between 1 and the number of rows ",
         "of the parameter wisle_est.")
  }
  if (!is.numeric(poi_woca) && !is.na(poi_woca) || length(poi_woca) > 1) {
    stop("The parameter poi_woca must be a numeric of length 1.")
  }
  if (!is.numeric(wc_icpt) || length(wc_icpt) > 1) {
    stop("The parameter wc_icpt must be a numeric of length 1.")
  }
  if (!is.null(x_range)) {
    if (!is.numeric(x_range) || length(x_range) != 2) {
      stop("The parameter x_range must be a vector of length 2.")
    }
  }
  if (!is.character(sl_model_name) || length(sl_model_name) != 1) {
    stop("The parameter sl_model_name must be a single string.")
  }
  if (!is.character(wcsl_model_name) || length(wcsl_model_name) != 1) {
    stop("The parameter wcsl_model_name must be a single string.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Preparation of data

  t_exp <- wisle_est

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

  return(d_seg)
}

#' Prepare arrow supporting the explanation of graphical elements
#'
#' The function \code{get_arrows()} prepares a data frame for putting an
#' arrow on a plot prepared by the \code{ggplot()} function from
#' the \sQuote{\code{ggplot2}} package
#'
#' @inheritParams get_segments
#'
#' @details The function \code{get_arrows()} expects various pieces
#' of information that characterises an \sQuote{\code{expirest_osle}} or an
#' \sQuote{\code{expirest_wisle}} model. Based on the information provided,
#' the function prepares a data frame that can be handed over to the
#' \code{geom_vline()} function from the \sQuote{\code{ggplot2}} package.
#'
#' @return A data frame with the columns Time.1, Time.2, Response.1, Response.2,
#' Item, Colour, Line.Type, Arrow.Type, Size, Curvature, Angle and Length.
#' Time.1 and Time.2 represent the maximal allowed difference over time from
#' intercept and Response.1 and Response.2 represent the release limit.
#'
#' @seealso \code{\link{plot_expirest_osle}}, \code{\link{plot_expirest_wisle}},
#' \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{geom_text}}.
#'
#' @keywords internal

get_arrow <- function(sl, ivl_side, wisle_est, rl, rl_index,
                      wc_icpt, x_range, sl_model_name, wcsl_model_name) {
  if (!is.numeric(sl) || length(sl) > 2) {
    stop("The parameter sl must be a numeric or vector of length 1 or 2.")
  }
  if (length(sl) == 2) {
    if (sl[2] < sl[1]) {
      stop("The parameter sl must be of the form c(lower, upper).")
    }
  }
  if (!(ivl_side %in% c("lower", "upper", "both"))) {
    stop("Please specify ivl_side either as \"lower\", \"upper\" or \"both\".")
  }
  if (!is.data.frame(wisle_est) || ncol(wisle_est) != 24) {
    stop("The parameter wisle_est must be a data frame with 24 columns.")
  }
  if (!is.numeric(rl)) {
    stop("The parameter rl must be a numeric.")
  }
  if (length(rl) != nrow(wisle_est)) {
    stop("The parameter rl must be a positive integer of the same length ",
         "as the parameter wisle_est has rows.")
  }
  if (!is.numeric(rl_index) || length(rl_index) > 1) {
    stop("The parameter rl_index must be a positive integer of length 1.")
  }
  if (rl_index != as.integer(rl_index)) {
    stop("The parameter rl_index must be a positive integer of length 1.")
  }
  if (rl_index < 1 || rl_index > nrow(wisle_est)) {
    stop("The parameter rl_index must be between 1 and the number of rows ",
         "of the parameter wisle_est.")
  }
  if (!is.numeric(wc_icpt) || length(wc_icpt) > 1) {
    stop("The parameter wc_icpt must be a numeric of length 1.")
  }
  if (!is.null(x_range)) {
    if (!is.numeric(x_range) || length(x_range) != 2) {
      stop("The parameter x_range must be a vector of length 2.")
    }
  }
  if (!is.character(sl_model_name) || length(sl_model_name) != 1) {
    stop("The parameter sl_model_name must be a single string.")
  }
  if (!is.character(wcsl_model_name) || length(wcsl_model_name) != 1) {
    stop("The parameter wcsl_model_name must be a single string.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Preparation of data

  t_exp <- wisle_est

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

  return(d_arr)
}
