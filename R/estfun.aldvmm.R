#' Create Matrix of Adjusted Limited Dependent Variable Mixture Model Gradients per Observation
#'
#' The method \code{estfun.aldvmm} for the generic function 
#' \ifelse{html}{\code{\link[sandwich]{estfun}}}{\code{sandwich::estfun()}} calculates the gradient of the aldvmm 
#' log-likelihood 
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.ll}}}{\code{aldvmm::aldvmm.ll()}} 
#' with respect to parameter values for each observation. 
#'
#' @param x an object inheriting from class "aldvmm".
#' @param ... further arguments passed to or from other methods.
#'
#' @return a numeric matrix of gradients with one row per 
#' observation and one column per parameter.
#'
#' @method estfun aldvmm
#' @rdname estfun
#' @export estfun.aldvmm
#'
#' @export

estfun.aldvmm <- function(x,
                          ...) {
  
  aldvmm.gr(par = x$coef,
            X = model.matrix(x),
            y = x$pred$y,
            psi = x$psi,
            ncmp = x$k,
            dist = x$dist,
            lcoef = x$label$lcoef,
            lcmp  = x$label$lcmp,
            lcpar = x$label$lcpar,
            optim.method = x$optim.method)
  
}