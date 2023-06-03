#' Create Matrix of Adjusted Limited Dependent Variable Mixture Model Gradients per Observation
#'
#' The generic function 
#' \ifelse{html}{\code{\link[sandwich]{estfun}}}{\code{sandwich::estfun()}} calculates the gradient of the aldvmm 
#' log-likelihood 
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.ll}}}{\code{aldvmm::aldvmm.ll()}} 
#' with respect to parameter values for each observation using 
#' \ifelse{html}{\code{\link[numDeriv]{jacobian}}}{\code{numDeriv::jacobian()}}. 
#'
#' @param object an object inheriting from class "aldvmm".
#' @param ... further arguments passed to or from other methods.
#'
#' @return \code{estfun.aldvmm} returns a matrix with one row per observation and one column per parameter.
#'
#' @method estfun aldvmm
#' @rdname estfun
#' @export estfun.aldvmm
#'
#' @export

estfun.aldvmm <- function(object,
                          ...) {
  
  X <- model.matrix(object)
  
  ef <- lapply(1:nobs(object), function(i) {
    numDeriv::jacobian(
      func = function(z) aldvmm::aldvmm.ll(par = z,
                                           X = lapply(X, function (m) 
                                             t(as.matrix(m[i, ]))),
                                           y = object$pred$y[i], 
                                           psi = object$psi,
                                           ncmp = object$k,
                                           dist = object$dist,
                                           lcoef = object$label$lcoef,
                                           lcpar = object$label$lcpar,
                                           lcmp = object$label$lcmp,
                                           optim.method = object$optim.method),
      x = object$coef
    )
  })
  
  ef <- do.call("rbind", ef)
  
  colnames(ef) <- names(object$coef)
  
  return(ef)
  
}