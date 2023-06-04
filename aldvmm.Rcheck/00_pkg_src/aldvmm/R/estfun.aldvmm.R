#' Create Matrix of Adjusted Limited Dependent Variable Mixture Model Gradients per Observation
#'
#' The method \code{estfun.aldvmm} for the generic function 
#' \ifelse{html}{\code{\link[sandwich]{estfun}}}{\code{sandwich::estfun()}} calculates the gradient of the aldvmm 
#' log-likelihood 
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.ll}}}{\code{aldvmm::aldvmm.ll()}} 
#' with respect to parameter values for each observation using 
#' \ifelse{html}{\code{\link[numDeriv]{jacobian}}}{\code{numDeriv::jacobian()}}. 
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
  
  X <- model.matrix(x)
  
  ef <- lapply(1:nobs(x), function(i) {
    numDeriv::jacobian(
      func = function(z) aldvmm::aldvmm.ll(par = z,
                                           X = lapply(X, function (m) 
                                             t(as.matrix(m[i, ]))),
                                           y = x$pred$y[i], 
                                           psi = x$psi,
                                           ncmp = x$k,
                                           dist = x$dist,
                                           lcoef = x$label$lcoef,
                                           lcpar = x$label$lcpar,
                                           lcmp = x$label$lcmp,
                                           optim.method = x$optim.method),
      x = x$coef
    )
  })
  
  ef <- do.call("rbind", ef)
  
  colnames(ef) <- names(x$coef)
  
  return(ef)
  
}