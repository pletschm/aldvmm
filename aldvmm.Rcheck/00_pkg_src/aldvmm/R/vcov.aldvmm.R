#' Extract Adjusted Limited Dependent Variable Mixture Model Covariance Matrix
#'
#' The method \code{vcov.aldvmm} for the generic function
#' \ifelse{html}{\code{\link[stats]{vcov}}}{\code{stats::vcov()}} returns the 
#' covariance matrix from an object of class "aldvmm".
#'
#' @param object an object inheriting from class "aldvmm".
#' @param ... further arguments passed to or from other methods.
#'
#' @return a numeric matrix.
#'
#' @method vcov aldvmm
#' @rdname vcov
#' @export vcov.aldvmm
#'
#' @export 

vcov.aldvmm <- function(object,
                         ...) {
  object$cov
}