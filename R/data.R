#' Stability potency data of five batches
#'
#' A data set containing the potency stability data (in \% of label claim (LC))
#' of five batches of a drug product obtained over a 24 months period. A total
#' of \eqn{n = 53} independent measurements are available (corresponding to
#' data shown in Tables IV, VI and VIII in LeBlond et al. (2011)). Data in
#' Table IV (batches \code{b2}, \code{b5} and \code{b7}) are compatible with a
#' common intercept / common slope (cics) model, data in Table VI (batches
#' \code{b3}, \code{b4} and \code{b5}) with a different intercept / common slope
#' (dics) model  and data in Table VIII (bathes \code{b4}, \code{b5} and
#' \code{b8}) with a different intercept / different slope (dids) model.
#'
#' @docType data
#' @usage data(exp1)
#'
#' @format A data frame with 53 observations and 3 variables:
#' \describe{
#'   \item{Batch}{Factor with levels \code{b2}, \code{b3}, \code{b4}, \code{b5},
#'                \code{b7} and \code{b8}}
#'   \item{Month}{Numeric representing the time points of testing from the
#'                start (0 months) to the end (24 months) of the study.}
#'   \item{Potency}{Numeric of the measured potency values in \%LC}
#' }
#'
#' @references
#' LeBlond, D., Griffith, D. and Aubuchon, K. Linear Regression 102: Stability
#' Shelf Life Estimation Using Analysis of Covariance. \emph{J Valid Technol}
#' (2011) \strong{17}(3): 47-68.\cr
#' \url{http://www.ivtnetwork.com/sites/default/files/LinearRegression.pdf}
#'
#' @source
#' See reference: Example data sets shown in Tables IV, VI and VIII.
#'
#' @examples
#' \dontrun{exp1}
"exp1"


#' Stability related substance data of three batches
#'
#' A data set containing the related substance stability data (in \% of label
#' claim (LC)) of three batches of a drug product obtained over a 24 months
#' period. A total of \eqn{n = 24} independent measurements are available
#' (corresponding to data shown in Table XI in LeBlond et al. (2011)).
#'
#' @docType data
#' @usage data(exp2)
#'
#' @format A data frame with 48 observations and 3 variables:
#' \describe{
#'   \item{Batch}{Factor with levels \code{b4}, \code{b5} and \code{b8}}
#'   \item{Month}{Numeric representing the time points of testing from the
#'                start (0 months) to the end (24 months) of the study.}
#'   \item{Related}{Numeric of the measured related substance levels in \%LC}
#' }
#'
#' @references
#' LeBlond, D., Griffith, D. and Aubuchon, K. Linear Regression 102: Stability
#' Shelf Life Estimation Using Analysis of Covariance. \emph{J Valid Technol}
#' (2011) \strong{17}(3): 47-68.\cr
#' \url{http://www.ivtnetwork.com/sites/default/files/LinearRegression.pdf}
#'
#' @source
#' See reference: Example data sets shown in Table XI.
#'
#' @examples
#' \dontrun{exp2}
"exp2"


#' Stability moisture data of three batches
#'
#' A data set containing the moisture stability data (\% (w/w)) of three
#' batches of a drug product obtained over a 24 months period. A total of
#' \eqn{n = 33} independent measurements are available (corresponding to data
#' shown in Table XIII in LeBlond et al. (2011)).
#'
#' @docType data
#' @usage data(exp3)
#'
#' @format A data frame with 33 observations and 3 variables:
#' \describe{
#'   \item{Batch}{Factor with levels \code{b1}, \code{b2} and \code{b3}}
#'   \item{Month}{Numeric representing the time points of testing from the
#'                start (0 months) to the end (24 months) of the study.}
#'   \item{Moisture}{Numeric of the measured moisture levels \%(w/w)}
#' }
#'
#' @references
#' LeBlond, D., Griffith, D. and Aubuchon, K. Linear Regression 102: Stability
#' Shelf Life Estimation Using Analysis of Covariance. \emph{J Valid Technol}
#' (2011) \strong{17}(3): 47-68.\cr
#' \url{http://www.ivtnetwork.com/sites/default/files/LinearRegression.pdf}
#'
#' @source
#' See reference: Example data sets shown in Table XIII.
#'
#' @examples
#' \dontrun{exp3}
"exp3"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Stability data of 4 batches
#'
#' A data set containing the concentration data (mg/kg) of four batches
#' obtained over a 24 months period of a drug product. A total of \eqn{n = 36}
#' independent measurements are available (corresponding to the data set
#' \emph{Reliability/Stability.jmp} in \emph{JMP(R) 12 Reliability and Survival
#' Methods} manual).
#'
#' @docType data
#' @usage data(exp4)
#'
#' @format A data frame with 36 observations and 3 variables:
#' \describe{
#'   \item{Batch}{Factor with levels \code{1_11}, \code{2_12}, \code{3_13} and
#'     \code{4_14}.}
#'   \item{Month}{Numeric representing the time points of testing from the
#'                start (0 months) to the end (24 months) of the study.}
#'   \item{Conc}{Numeric of the meausred concentrations in \eqn{mg / kg}.}
#' }
#'
#' @keywords datasets
#'
#' @references
#' SAS Institute Inc. 2015. \emph{JMP(R) 12 Reliability and Survival Methods}.
#' Cary, NC: SAS Institute Inc.\cr
#' \cr
#' \url{https://support.sas.com/documentation/onlinedoc/jmp/12.0/
#' ReliabilitySurvivalMethods.pdf}
#'
#' @source
#' See reference: Example data set (Stability.jmp) used in chapter
#' \emph{Stability Analysis}, p. 174-176.
#'
#' @examples
#' \dontrun{exp4}
"exp4"

## <><><><><><><><><><><><><><><><><><><><><><><><><><><><>
