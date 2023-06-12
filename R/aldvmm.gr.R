#' Calculating Numeric Gradients of the Negative Log-Likelihood
#'
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.gr}}}{\code{aldvmm.gr()}}
#' calculates numerical gradients of the negative log-likelihood returned by
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.ll}}}{\code{aldvmm.ll()}} with
#' respect to parameter values in \code{'par'}.
#'
#' @inheritParams aldvmm
#' @inheritParams aldvmm.cv
#'
#' @details \ifelse{html}{\code{\link[aldvmm]{aldvmm.gr}}}{\code{aldvmm.gr()}}
#' uses \ifelse{html}{\code{\link[numDeriv]{grad}}}{\code{numDeriv::grad()}} to
#' perform numerical approximation of gradients of the negative log-likelihood
#' returned by
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.ll}}}{\code{aldvmm.ll()}}.
#'
#' @return a named numeric vector of first derivatives of the negative
#'   log-likelihood of the data with respect to parameters in \code{'par'}.
#'
#' @export

aldvmm.gr <- function(par,
                      X,
                      y,
                      psi,
                      dist,
                      ncmp,
                      lcoef,
                      lcmp,
                      lcpar,
                      optim.method,
                      num.grad) {
  
  if (num.grad == TRUE) {
    grad <- numDeriv::grad(func = function(z) aldvmm.ll(par = z,
                                                        X = X,
                                                        y = y,
                                                        psi = psi,
                                                        dist = dist,
                                                        ncmp = ncmp,
                                                        lcoef = lcoef,
                                                        lcmp = lcmp,
                                                        lcpar = lcpar,
                                                        optim.method = optim.method), 
                           x = par)
  } else {
    grad <- colSums(aldvmm.sc(par = par,
                              X = X,
                              y = y,
                              psi = psi,
                              ncmp = ncmp,
                              dist = dist,
                              lcoef = lcoef,
                              lcmp  = lcmp,
                              lcpar = lcpar))
  }
  
  
  return(grad)
}