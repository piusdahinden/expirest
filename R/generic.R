#' Summary of the shelf life estimation (osle)
#'
#' This is a method for the function \code{summary()} for objects of class
#' \sQuote{\code{expirest_osle}}.
#'
#' @param object An object of class \sQuote{\code{expirest_osle}} returned
#'   by the \code{\link{expirest_osle}()} function.
#' @param ... Further arguments passed to or from other methods or arguments
#'   that can be passed down to the \code{\link[base]{formatC}()} function.
#'
#' @details The function \code{\link{expirest_osle}()} estimates the shelf
#' life, or retest period, following the ICH Q1E guideline. By default, batch
#' poolability is checked as recommended by the guideline at a significance
#' level of 0.25. Other levels can be used, although not recommended, by
#' changing the default of the \code{alpha_pool} parameter. Three possible
#' models may be appropriate, i.e.
#' \itemize{
#'  \item a \emph{common intercept / common slope} model (cics),
#'  \item a \emph{different intercept / common slope} model (dics) or
#'  \item a \emph{different intercept / different slope} model (dids).
#' }
#'
#' The worst case intercept is the intercept of the batch whose confidence
#' limit is the first crossing the acceptance limit. As in case of the
#' \code{cics} model type all batches have a common intercept and a common
#' confidence interval, all batches can be regarded as equally worst case. In
#' case of the \code{dids} model type, shelf life estimation is done using the
#' models obtained from fitting the data of each batch individually.
#'
#' @return The \sQuote{\code{expirest_osle}} object passed to the \code{object}
#' parameter is returned invisibly.
#'
#' @seealso \code{\link{expirest_osle}}, \code{\link{expirest_wisle}},
#' \code{\link[base]{formatC}}, \code{\link[utils]{methods}}.
#'
#' @example man/examples/examples_summary.expirest_osle.R
#'
#' @export

summary.expirest_osle <- function(object, ...) {
  mt <- object[["Model.Type"]]$type.spec
  mtac <- object[["Model.Type"]]$type.acronym
  itac <- ifelse(object[["Model.Type"]]$type.acronym == "n.a.",
                 "dids",
                 object[["Model.Type"]]$type.acronym)

  mf <- match.call(expand.dots = TRUE)
  m <- match("digits", names(mf), 0L)

  if (m == 0) {
    digits <- getOption("digits")
  } else {
    digits <- mf[[m]]
  }

  cat("\nSummary of shelf life estimation following the ICH Q1E guideline")
  cat("\n\nThe best model accepted at a significance level of",
      object[["Parameters"]]$alpha.pool,
      "has\n",
      ifelse(
        is.na(mt[1]),
        c("NA intercepts and"),
        c("Different intercepts and", "Common intercepts and")[mt[1] + 1]
      ),
      ifelse(
        is.na(mt[2]),
        c("NA slopes"),
        c("Different slopes", "Common slopes")[mt[2] + 1]
      ),
      paste0("(acronym: ",
             mtac,
             ")."))

  cat("\n\nWorst case intercept: ",
      ifelse(is.na(object$wc.icpt[itac]),
             NA,
             formatC(as.numeric(object$wc.icpt[itac]), digits = digits)),
      " (", object[["Variables"]]$response, ")", sep = "")

  cat("\nWorst case batch:",
      ifelse(is.na(object$wc.batch[itac]),
             NA,
             levels(object[["Data"]]
                    [, object[["Variables"]]$batch])[object$wc.batch[itac]]))

  cat("\nEstimated shelf life for ",
      mtac,
      " model: ",
      ifelse(is.na(object[["POI"]][itac]),
             NA,
             formatC(object[["POI"]][itac], digits)),
      " (", object[["Variables"]]$time, ")", sep = "")

  cat("\n\nWorst case intercepts, POIs and batches of all models
   (Including information about the side where the confidence
    interval crosses the specification boundary):\n")
  d_res <- data.frame(
    Intercept = formatC(as.numeric(object$wc.icpt), digits = digits),
    POI = formatC(as.numeric(object$POI), digits = digits),
    Side = attributes(object$POI)$side,
    Batch = vapply(object$wc.batch, function(bn) {
      ifelse(is.na(bn),
             "NA",
             levels(object[["Data"]][, object[["Variables"]]$batch])[bn])
    },
    character(1))
  )
  print(d_res)
  cat("\n")

  invisible(object)
}

#' Print a summary of the shelf life estimation (osle)
#'
#' This is a method for the function \code{print()} for objects of class
#' \sQuote{\code{expirest_osle}}.
#'
#' @param x An object of class \sQuote{\code{expirest_osle}} returned by
#'   the \code{\link{expirest_osle}()} function.
#' @inheritParams summary.expirest_osle
#'
#' @return The \sQuote{\code{expirest_osle}} object passed to the \code{x}
#' parameter is returned invisibly.
#'
#' @inherit summary.expirest_osle details seealso
#'
#' @example man/examples/examples_print.expirest_osle.R
#'
#' @export

print.expirest_osle <- function(x, ...) {

  summary(object = x, ...)

  invisible(x)
}

#' Plot illustrating the shelf life estimation (osle)
#'
#' This is a method for the function \code{plot()} for objects of class
#' \sQuote{\code{plot_expirest_osle}}.
#'
#' @param x An object of class \sQuote{\code{plot_expirest_osle}} returned by
#'   the \code{\link{plot_expirest_osle}()} function.
#' @inheritParams summary.expirest_osle
#'
#' @details The element \code{Graph} of the \sQuote{\code{plot_expirest_osle}}
#' object that is returned by the function \code{\link{plot_expirest_osle}()}
#' is an object of class \sQuote{\code{ggplot}}, generated by the function
#' \code{\link[ggplot2]{ggplot}()} from the \sQuote{\code{ggplot2}} package.
#' Thus, the corresponding \code{plot} method is used for plotting. Arguments
#' to the \code{\link[ggplot2]{ggplot}()} function can be passed via the
#' \code{...} parameter.
#'
#' @return The \sQuote{\code{plot_expirest_osle}} object passed to the \code{x}
#' parameter is returned invisibly.
#'
#' @seealso \code{\link{expirest_osle}}, \code{\link{plot_expirest_osle}},
#' \code{\link[ggplot2]{ggplot}()}, \code{\link[utils]{methods}}.
#'
#' @example man/examples/examples_plot.plot_expirest_osle.R
#'
#' @export

plot.plot_expirest_osle <- function(x, ...) {

  plot(x = x$Graph, ...)

  invisible(x)
}

#' Print a plot illustrating the shelf life estimation (osle)
#'
#' This is a method for the function \code{print()} for objects of class
#' \sQuote{\code{plot_expirest_osle}}.
#'
#' @param x An object of class \sQuote{\code{plot_expirest_osle}} returned by
#'   the \code{\link{plot_expirest_osle}()} function.
#' @inheritParams plot.plot_expirest_osle
#'
#' @inherit plot.plot_expirest_osle details return seealso
#'
#' @example man/examples/examples_print.plot_expirest_osle.R
#'
#' @export

print.plot_expirest_osle <- function(x, ...) {

  plot.plot_expirest_osle(x = x, ...)

  invisible(x)
}

#' Summary of the what-if shelf life estimation (wisle)
#'
#' This is a method for the function \code{summary()} for objects of class
#' \sQuote{\code{expirest_wisle}}.
#'
#' @param object An object of class \sQuote{\code{expirest_wisle}} returned
#'   by the \code{\link{expirest_wisle}()} function.
#' @param ... Further arguments passed to or from other methods or arguments
#'   that can be passed down to the \code{\link[base]{formatC}()} function.
#'
#' @details The function \code{\link{expirest_wisle}()} estimates the expiry
#' for the specified release and specification limit following the ARGPM
#' guidance \dQuote{Stability testing for prescription medicines}. By default,
#' batch poolability is checked as recommended by the ICH Q1E guideline at a
#' significance level of 0.25. Other levels can be used, although not
#' recommended, by changing the default of the \code{alpha_pool} parameter.
#' Three possible models may be appropriate, i.e.
#' \itemize{
#'  \item a \emph{common intercept / common slope} model (cics),
#'  \item a \emph{different intercept / common slope} model (dics) or
#'  \item a \emph{different intercept / different slope} model (dids).
#' }
#'
#' The worst case intercept is the intercept of the batch whose confidence
#' limit is the first crossing the acceptance limit. As in case of the
#' \code{cics} model type all batches have a common intercept and a common
#' confidence interval, all batches can be regarded as equally worst case. In
#' case of the \code{dids} model type, shelf life estimation is done using the
#' models obtained from fitting the data of each batch individually. In
#' addition to the shelf life estimated according to the ARGPM also the
#' estimate according to ICH Q1E is shown.
#'
#' @return The \sQuote{\code{expirest_wisle}} object passed to the
#' \code{object} parameter is returned invisibly.
#'
#' @seealso \code{\link{expirest_wisle}}, \code{\link{expirest_osle}},
#' \code{\link[base]{formatC}}, \code{\link[utils]{methods}}.
#'
#' @example man/examples/examples_summary.expirest_wisle.R
#'
#' @export

summary.expirest_wisle <- function(object, ...) {
  mt <- object[["Model.Type"]]$type.spec
  mtac <- object[["Model.Type"]]$type.acronym
  itac <- ifelse(object[["Model.Type"]]$type.acronym == "n.a.",
                 "dids",
                 object[["Model.Type"]]$type.acronym)

  mf <- match.call(expand.dots = TRUE)
  m <- match("digits", names(mf), 0L)

  if (m == 0) {
    digits <- getOption("digits")
  } else {
    digits <- mf[[m]]
  }

  tmp_1 <- object[["POI"]][, -c(grep("Intercept", colnames(object[["POI"]])),
                                grep("Delta", colnames(object[["POI"]])),
                                grep("WCSL", colnames(object[["POI"]])))]

  tmp_1 <- tmp_1[, c("Exp.Spec.Report", "Rel.Spec.Report",
                     colnames(tmp_1)[grep(itac,
                                          colnames(tmp_1))])]
  if (itac %in% c("dids", "dids.pmse")) {
    colnames(tmp_1) <-
      c("SL", "RL", "wisle", "wisle (pmse)", "osle", "osle (pmse)")
  } else {
    colnames(tmp_1) <- c("SL", "RL", "wisle", "osle")
  }
  rownames(tmp_1) <- NULL

  wc_batch <- rep(NA, length(object$wc.batch[[itac]]))
  for (i in seq_along(object$wc.batch[[itac]])) {
    if (!is.na(object$wc.batch[[itac]][i]))
      wc_batch[i] <- levels(object[["Data"]][[object[["Variables"]]$batch]])[
        object$wc.batch[[itac]][i]]
  }

  tmp_2 <- data.frame(RL = object[["POI"]][, "Rel.Spec.Report"],
                      Batch = wc_batch,
                      Intercept = object$wc.icpt[, itac])
  rownames(tmp_2) <- NULL

  cat("\nSummary of shelf life estimation following the ARGPM
  guidance \"Stability testing for prescription medicines\"")
  cat("\n\nThe best model accepted at a significance level of",
      object[["Parameters"]]$alpha.pool,
      "has\n",
      ifelse(
        is.na(mt[1]),
              c("NA intercepts and"),
              c("Different intercepts and", "Common intercepts and")[mt[1] + 1]
        ),
      ifelse(
        is.na(mt[2]),
        c("NA slopes"),
        c("Different slopes", "Common slopes")[mt[2] + 1]
      ),
      paste0("(acronym: ",
             mtac,
             ")."))

  if (nrow(tmp_2) == 1) {
    cat("\n\nWorst case intercept and batch:\n")
    print(tmp_2, digits = digits)
  } else {
    cat("\n\nWorst case intercepts and batches:\n")
    print(tmp_2, digits = digits)
  }

  cat("\nEstimated shelf lives for the",
      mtac)
  if (itac != "dids") {
    cat(" model:\n")
  } else {
    cat(" model (for information, the results of
  the model fitted with pooled mean square error (pmse) are also shown:\n")
  }

  print(tmp_1, digits = digits)
  cat("\nAbbreviations:
  ARGPM: Australian Regulatory Guidelines for Prescription Medicines;
  ICH: International Council for Harmonisation;
  osle: Ordinary shelf life estimation (i.e. following the ICH guidance);")
  if (itac == "dids") cat("\n  pmse: Pooled mean square error;")
  cat("\n  RL: Release Limit;
  SL: Specification Limit;
  wisle: What-if (approach for) shelf life estimation (see ARGPM guidance).")

  invisible(object)
}

#' Print a summary of the what-if shelf life estimation (wisle)
#'
#' This is a method for the function \code{print()} for objects of class
#' \sQuote{\code{expirest_wisle}}.
#'
#' @param x An object of class \sQuote{\code{expirest_wisle}} returned by
#'   the \code{expirest_wisle()} function.
#' @inheritParams summary.expirest_wisle
#'
#' @return The \sQuote{\code{expirest_wisle}} object passed to the
#' \code{x} parameter is returned invisibly.
#'
#' @inherit summary.expirest_wisle details seealso
#'
#' @example man/examples/examples_print.expirest_wisle.R
#'
#' @export

print.expirest_wisle <- function(x, ...) {

  summary(object = x, ...)

  invisible(x)
}

#' Plot illustrating the what-if shelf life estimation (wisle)
#'
#' This is a method for the function \code{plot()} for objects of class
#' \sQuote{\code{plot_expirest_wisle}}.
#'
#' @param x An object of class \sQuote{\code{plot_expirest_wisle}} returned by
#'   the \code{\link{plot_expirest_wisle}()} function.
#' @inheritParams summary.expirest_wisle
#'
#' @details The element \code{Graph} of the \sQuote{\code{plot_expirest_wisle}}
#' object that is returned by the function \code{\link{plot_expirest_wisle}()}
#' is an object of class \sQuote{\code{ggplot}}, generated by the function
#' \code{\link[ggplot2]{ggplot}()} from the \sQuote{\code{ggplot2}} package.
#' Thus, the corresponding \code{plot} method is used for plotting. Arguments
#' to the \code{\link[ggplot2]{ggplot}()} function can be passed via the
#' \code{...} parameter.
#'
#' @return The \sQuote{\code{plot_expirest_wisle}} object passed to the \code{x}
#' parameter is returned invisibly.
#'
#' @seealso \code{\link{expirest_wisle}}, \code{\link{plot_expirest_wisle}},
#' \code{\link[ggplot2]{ggplot}()}, \code{\link[utils]{methods}}.
#'
#' @example man/examples/examples_plot.plot_expirest_wisle.R
#'
#' @export

plot.plot_expirest_wisle <- function(x, ...) {

  plot(x = x$Graph, ...)

  invisible(x)
}

#' Print a plot illustrating the what-if shelf life estimation (wisle)
#'
#' This is a method for the function \code{print()} for objects of class
#' \sQuote{\code{plot_expirest_wisle}}.
#'
#' @param x An object of class \sQuote{\code{plot_expirest_wisle}} returned by
#'   the \code{\link{plot_expirest_wisle}()} function.
#' @inheritParams plot.plot_expirest_wisle
#'
#' @inherit plot.plot_expirest_wisle details return seealso
#'
#' @example man/examples/examples_print.plot_expirest_wisle.R
#'
#' @export

print.plot_expirest_wisle <- function(x, ...) {

  plot.plot_expirest_wisle(x = x, ...)

  invisible(x)
}
