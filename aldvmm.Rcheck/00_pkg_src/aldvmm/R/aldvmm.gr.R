#' Calculating Numeric Gradients of the Negative Log-Likelihood
#'
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.gr}}}{\code{aldvmm.gr()}}
#' calculates numerical gradients of the negative log-likelihood of the entire
#' estimation data with respect to parameter values in \code{'par'}.
#'
#' @inheritParams aldvmm
#' @inheritParams aldvmm.cv
#'
#' @details \ifelse{html}{\code{\link[aldvmm]{aldvmm.gr}}}{\code{aldvmm.gr()}}
#' uses \ifelse{html}{\code{\link[aldvmm]{aldvmm.sc}}}{\code{aldvmm.sc()}} to
#' calculate analytical gradients of the negative log-likelihood.
#' 
#' If \code{'par'} includes infinite values 
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.gr}}}{\code{aldvmm.gr()}} returns a 
#' gradient of zero.
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
                      optim.method) {
  
  out <- colSums(aldvmm.sc(par = par,
                           X = X,
                           y = y,
                           psi = psi,
                           ncmp = ncmp,
                           dist = dist,
                           lcoef = lcoef,
                           lcmp  = lcmp,
                           lcpar = lcpar,
                           optim.method))
  
  if (optim.method %in% c("L-BFGS-B", "Rcgmin")) {
    out[!is.finite(out)] <- 0
  }
  
  return(out)
  
}