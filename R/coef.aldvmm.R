#' Extract Coefficients of Adjusted Limited Dependent Variable Mixture 
#' Model Fits
#'
#' The generic function
#' \ifelse{html}{\code{\link[stats]{coef}}}{\code{stats::coef()}} extracts the 
#' vector of coefficients from an object of class "aldvmm".
#'
#' @param object an object inheriting from class "aldvmm".
#' @param ... further arguments passed to or from other methods.
#'
#' @return a named vector of numeric parameter estimates.
#'
#' @method coef aldvmm
#' @rdname coef
#' @export coef.aldvmm
#'
#' @export

coef.aldvmm <- function (object, 
                         ...) {
  object$coef
}