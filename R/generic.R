#' Summary of the shelf life estimation (osle)
#'
#' This is a method for the function \code{summary()} for objects of class
#' \code{expirest_osle}.
#'
#' @param object an \code{expirest_osle} object, i.e. a list returned by the
#'   \sQuote{expirest_osle()} function.
#' @param ... further arguments passed to or from other methods or arguments
#'   that can be passed down to the \code{\link[base]{formatC}} function.
#'
#' @details The function \code{summary.expirest_osle()} prints the most
#' relevant information in an \code{expirest_osle} object.
#'
#' @return The \code{expirest_osle} object passed to the \code{model} parameter
#' is returned invisibly.
#'
#' @seealso \code{\link{expirest_osle}}, \code{\link{expirest_wisle}},
#' \code{\link[base]{formatC}}, \code{\link[utils]{methods}}.
#'
#' @export

summary.expirest_osle <- function(object, ...) {
  mt <- object[["Model.Type"]]$type.spec

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
      c("Different intercepts and", "Common intercepts and")[mt[1] + 1],
      c("Different slopes", "Common slopes")[mt[2] + 1],
      paste0("(acronym: ",
             object[["Model.Type"]]$type.acronym,
             ")."))

  cat("\n\nWorst case intercept:",
      ifelse(is.na(object$wc.icpt),
             NA,
             formatC(as.numeric(object$wc.icpt), digits = digits)))

  cat("\nWorst case batch:",
      levels(object[["Data"]]
             [, object[["Variables"]]$batch])[object[["wc.batch"]]])

  cat("\n\nEstimated shelf life for",
      object[["Model.Type"]]$type.acronym,
      "model:",
      ifelse(is.na(object[["POI"]][object[["Model.Type"]]$type.acronym]),
             NA,
             formatC(object[["POI"]][object[["Model.Type"]]$type.acronym],
                     digits)),
      "\n")

  invisible(object)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Print a summary of the shelf life estimation (osle)
#'
#' This is a method for the function \code{print()} for objects of class
#' \code{expirest_osle}.
#'
#' @param x an \code{expirest_osle} object, i.e. a list returned by the
#'   \sQuote{expirest_osle()} function.
#' @inheritParams summary.expirest_osle
#'
#' @details The function \code{print.expirest_osle()} prints the most relevant
#' information in an \code{expirest_osle} object.
#'
#' @inherit summary.expirest_osle return seealso
#'
#' @export

print.expirest_osle <- function(x, ...) {

  summary(object = x, ...)

  invisible(x)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Summary of the what-if shelf life estimation (wisle)
#'
#' This is a method for the function \code{summary()} for objects of class
#' \code{expirest_wisle}.
#'
#' @param object an \code{expirest_wisle} object, i.e. a list returned by the
#'   \sQuote{expirest_wisle()} function.
#' @param ... further arguments passed to or from other methods or arguments
#'   that can be passed down to the \code{\link[base]{formatC}} function.
#'
#' @details The function \code{summary.expirest_wisle()} prints the most
#' relevant information in an \code{expirest_wisle} object.
#'
#' @return The \code{expirest_wisle} object passed to the \code{model} parameter
#' is returned invisibly.
#'
#' @seealso \code{\link{expirest_wisle}}, \code{\link{expirest_osle}},
#' \code{\link[base]{formatC}}, \code{\link[utils]{methods}}.
#'
#' @export

summary.expirest_wisle <- function(object, ...) {
  mt <- object[["Model.Type"]]$type.spec

  mf <- match.call(expand.dots = TRUE)
  m <- match("digits", names(mf), 0L)

  if (m == 0) {
    digits <- getOption("digits")
  } else {
    digits <- mf[[m]]
  }

  tmp <- object[["POI"]][, -c(grep("Intercept", colnames(object[["POI"]])),
                             grep("Delta", colnames(object[["POI"]])),
                             grep("WCSL", colnames(object[["POI"]])))]

  tmp <- tmp[, c("Exp.Spec.Report", "Rel.Spec.Report",
                 colnames(tmp)[grep(object[["Model.Type"]]$type.acronym,
                                    colnames(tmp))])]
  colnames(tmp) <- c("SL", "RL", "wisle", "osle")

  cat("\nSummary of shelf life estimation following the ARGPM
  guidance \"Stability testing for prescription medicines\"")
  cat("\n\nThe best model accepted at a significance level of",
      object[["Parameters"]]$alpha.pool,
      "has\n",
      c("Different intercepts and", "Common intercepts and")[mt[1] + 1],
      c("Different slopes", "Common slopes")[mt[2] + 1],
      paste0("(acronym: ",
             object[["Model.Type"]]$type.acronym,
             ")."))

  cat("\n\nWorst case intercept(s):",
      ifelse(is.na(object$wc.icpt[, object[["Model.Type"]]$type.acronym]),
             NA,
             formatC(object$wc.icpt[, object[["Model.Type"]]$type.acronym],
                     digits = digits)))

  cat("\nWorst case batch(es):",
      levels(object[["Data"]][, object[["Variables"]]$batch])
      [object[["wc.batch"]][[object[["Model.Type"]]$type.acronym]]])

  cat("\n\nEstimated shelf life (lives) for",
      object[["Model.Type"]]$type.acronym,
      "model:\n")
  print(tmp, digits = digits)
  cat("\nAbbreviations:\n")
  cat("ARGPM: Australian Regulatory Guidelines for Prescription Medicines;",
      "ICH: International Conference on Harmonisation;",
      "osle: Ordinary shelf life estimation (i.e. following the ICH guidance);",
      "RL: Release Limit;",
      "SL: Specification Limit;",
      "wisle: What-if (approach for) shelf life estimation",
      "(i.e. following ARGPM guidance).\n\n")

  invisible(object)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Print a summary of the what-if shelf life estimation (wisle)
#'
#' This is a method for the function \code{print()} for objects of class
#' \code{expirest_wisle}.
#'
#' @param x an \code{expirest_wisle} object, i.e. a list returned by the
#'   \sQuote{expirest_wisle()} function.
#' @inheritParams summary.expirest_wisle
#'
#' @details The function \code{print.expirest_wisle()} prints the most relevant
#' information in an \code{expirest_wisle} object.
#'
#' @inherit summary.expirest_wisle return seealso
#'
#' @export

print.expirest_wisle <- function(x, ...) {

  summary(object = x, ...)

  invisible(x)
}

## <><><><><><><><><><><><><><><><><><><><><><><><><><><><>
