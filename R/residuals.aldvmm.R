#' Extract Adjusted Limited Dependent Variable Mixture Model Residuals
#'
#' The method \code{residuals.aldvmm} for the generic function
#' \ifelse{html}{\code{\link[stats]{residuals}}}{\code{stats::residuals()}} returns the 
#' covariance matrix from an object of class "aldvmm".
#'
#' @param object an object inheriting from class "aldvmm".
#' @param ... further arguments passed to or from other methods.
#'
#' @return a numeric vector of residuals.
#'
#' @method residuals aldvmm
#' @rdname residuals
#' @export residuals.aldvmm
#'
#' @export 

residuals.aldvmm <- function(object,
                        ...) {
  object$pred$res
}