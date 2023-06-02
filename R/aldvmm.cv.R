#' Numerical Approximation of Covariance Matrix
#'
#' @description
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.cv}}}{\code{aldvmm.cv()}} performs
#' a numerical approximation of the covariance matrix of parameter estimates.
#'
#' @inheritParams aldvmm.ll
#' @inheritParams aldvmm
#'
#' @param ll a function returning the negative log-likelihood of the adjusted
#'   limited dependent variable mixture model as a scalar result
#'   (\ifelse{html}{\code{\link[aldvmm]{aldvmm.ll}}}{\code{aldvmm.ll()}}).
#'
#' @details \ifelse{html}{\code{\link[aldvmm]{aldvmm.cv}}}{\code{aldvmm.cv()}}
#'   uses
#'   \ifelse{html}{\code{\link[numDeriv]{hessian}}}{\code{numDeriv::hessian()}}
#'   to calculate the hessian matrix of the log-likelihood function supplied to
#'   \code{'ll'} at parameter values supplied to \code{'par'}.
#'
#' @return \code{aldvmm.cv} returns a list with the following objects.
#'   \item{\code{hessian}}{a numeric matrix with second-order partial
#'   derivatives of the likelihood function \code{'ll'}.} \item{\code{cv}}{a
#'   numeric matrix with covariances/variances of parameters in \code{'par'}.}
#'   \item{\code{se}}{a numeric vector of standard errors of parameters in
#'   \code{'par'}.} \item{\code{z}}{a numeric vector of z-values of parameters
#'   in \code{'par'}.} \item{\code{p}}{a numeric vector of p-values of
#'   parameter estimates.} \item{\code{upper}}{a numeric vector of upper 95\%
#'   confindence limits of paramter estimates in \code{'par'}.}
#'   \item{\code{lower}}{a numeric vector of lower 95\% confindence limits of
#'   paramter estimates in \code{'par'}.}
#'
#' @export

aldvmm.cv <- function(ll,
                      par,
                      X,
                      y,
                      dist,
                      psi,
                      ncmp,
                      lcoef,
                      lcpar,
                      lcmp,
                      optim.method) {
  
  outlist <- list()
  
  # Hessian
  #--------
  
  outlist[["hessian"]] <- numDeriv::hessian(func = ll,
                                            x = par,
                                            X = X,
                                            y = y,
                                            psi = psi,
                                            ncmp = ncmp,
                                            dist = dist,
                                            lcoef = lcoef,
                                            lcmp = lcmp,
                                            lcpar = lcpar,
                                            optim.method = optim.method)
  
  rownames(outlist[["hessian"]]) <- names(par)
  colnames(outlist[["hessian"]]) <- names(par)
  
  # Covariance matrix
  #------------------
  
  outlist[["cv"]] <- tryCatch({
    solve(outlist[["hessian"]])
  }, error = function(e) {
    #message(e)
    matrix(data = NA, 
           nrow = nrow(outlist[["hessian"]]), 
           ncol = ncol(outlist[["hessian"]]))
  })
  
  rownames(outlist[["cv"]]) <- names(par)
  colnames(outlist[["cv"]]) <- names(par)
  
  # # Standard errors, significance and confidence intervals of parameters
  # #---------------------------------------------------------------------
  # 
  # if (all(is.na(outlist[["cv"]]))) {
  #   
  #   base::warning("no covariance matrix is obtained\n",
  #                 call. = FALSE)
  #   outlist[["se"]] <- rep(NA, times = length(par))
  #   
  # } else {
  #   
  #   suppressWarnings(
  #     outlist[["se"]] <- sqrt(diag(outlist[["cv"]]))
  #   )
  #   
  #   if (all(is.na(diag(outlist[["cv"]])))) {
  #     base::warning("covariance matrix includes only missing diagonals\n",
  #                   call. = FALSE)
  #   } 
  #   
  #   if (any(diag(outlist[["cv"]]) <= 0)) {
  #     base::warning("covariance matrix includes non-positive diagnoals\n",
  #                   call. = FALSE)
  #   }
  #   
  #   if (any(is.na(outlist[["se"]]))) {
  #     base::warning("missing standard errors are obtained\n",
  #                   call. = FALSE)
  #   }
  #   
  # }
  # 
  # names(outlist[["se"]]) <- names(par)
  # 
  # outlist[["z"]] <- par / outlist[["se"]]
  # names(outlist[["z"]]) <- names(par)
  # 
  # outlist[["p"]] <- 2 * stats::pnorm(abs(outlist[["z"]]), lower.tail = FALSE)
  # names(outlist[["p"]]) <- names(par)
  # 
  # outlist[["upper"]] <- par + stats::qnorm(0.975) * outlist[["se"]]
  # names(outlist[["upper"]]) <- names(par)
  # 
  # outlist[["lower"]] <- par - stats::qnorm(0.975) * outlist[["se"]]
  # names(outlist[["lower"]]) <- names(par)
  
  return(outlist)
}
