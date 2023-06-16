#' Extract Adjusted Limited Dependent Variable Mixture Model Formula
#'
#' The method \code{formula.aldvmm} for the generic function
#' \ifelse{html}{\code{\link[stats]{formula}}}{\code{stats::formula()}} returns the 
#' formula object from an object of class "aldvmm".
#'
#' @param object an object inheriting from class "aldvmm".
#' @param ... further arguments passed to or from other methods.
#'
#' @return an object of class "formula"
#'
#' @method formula aldvmm
#' @rdname formula
#' @export formula.aldvmm
#'
#' @export 

formula.aldvmm <- function(object,
                        ...) {
  object$formula
}