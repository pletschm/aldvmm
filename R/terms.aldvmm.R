#' Extract Adjusted Limited Dependent Variable Mixture Model Terms
#'
#' The method \code{terms.aldvmm} for the generic function
#' \ifelse{html}{\code{\link[stats]{terms}}}{\code{stats::terms()}} returns the 
#' terms object for the combined model of component means and probabilities of
#' component membership from an object of class "aldvmm".
#'
#' @param object an object inheriting from class "aldvmm".
#' @param ... further arguments passed to or from other methods.
#'
#' @return an object of class "terms"
#'
#' @method terms aldvmm
#' @rdname terms
#' @export terms.aldvmm
#'
#' @export 

terms.aldvmm <- function(object,
                        ...) {
  object$terms$full
}