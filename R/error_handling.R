#' Try getting results from expression
#'
#' The function \code{get_trial_list()} tries to execute the expression handed
#' over and returns a list of the outcome.
#'
#' @param expr An expression to be executed.
#'
#' @details The function tries to execute the expression handed over. It case
#' of an error it does not stop execution but in any case returns a list as
#' specified in chapter \emph{Value}. If execution of the expression was
#' successful, the results are stored in element \emph{Model}. In case of
#' warnings or errors, the messages are silently redirected to the elements
#' \emph{Warning} or \emph{Error}, respectively.
#'
#' @return A list of length three with the following elements is returned
#' \item{Model}{The object returned by the expression, if applicable,
#'   or otherwise NULL}
#' \item{Warning}{A warning message, if applicable, or otherwise NULL}
#' \item{Error}{An error message, if applicable, or otherwise NULL}
#'
#' @keywords internal

try_get_model <- function(expr) {
  warning_msg <- error_msg <- NULL

  result <- withCallingHandlers(
    tryCatch(expr,
             error = function(err) {
               error_msg <<- err
               NULL
             }),
    warning = function(warn) {
      warning_msg <<- warn
    })

  list(Model = result, Warning = warning_msg, Error = error_msg)
}

## <><><><><><><><><><><><><><><><><><><><><><><><><><><><>
