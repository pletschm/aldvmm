#' Obtain Adjusted Limited Dependent Variable Mixture Model Number of Observations
#'
#' The generic function
#' \ifelse{html}{\code{\link[stats]{nobs}}}{\code{stats::nobs()}} extracts the 
#' number of observations from an object of class "aldvmm".
#'
#' @param object an object inheriting from class "aldvmm".
#' @param ... further arguments passed to or from other methods.
#'
#' @method nobs aldvmm
#' @rdname nobs
#' @export nobs.aldvmm
#'
#' @export

nobs.aldvmm <- function(object,
                         ...) {
  object$n
}