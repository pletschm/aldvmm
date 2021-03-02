#' Numerical Approximation of Covariance Matrix
#'
#' @description
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.cv}}}{\code{aldvmm::aldvmm.cv()}}
#' performs a numerical approximation of the covariance matrix of parameter
#' estimates.
#'
#' @inheritParams aldvmm.ll
#' @inheritParams aldvmm
#'
#' @param ll a function
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm.ll}}}{\code{aldvmm::aldvmm.ll()}}
#'   returning the negative log-likelihood of the adjusted limited dependent
#'   variable mixture model as a scalar result.
#'
#' @details
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.cv}}}{\code{aldvmm::aldvmm.cv()}}
#' uses
#' \ifelse{html}{\code{\link[numDeriv]{hessian}}}{\code{numDeriv::hessian()}}
#' to calculate the hessian matrix of the log-likelihood function in
#' \code{'ll'} at parameter values in \code{'par'}.
#'
#' @return \code{aldvmm.cv} returns a list with the following objects.
#'   \item{\code{hessian}}{a numeric matrix with second-order partial
#'   derivatives of the likelihood function \code{'ll'}.} \item{\code{cv}}{a
#'   numeric matrix with covariances/variances of parameters in \code{'par'}.}
#'   \item{\code{se}}{a numeric vector of standard errors of parameters in
#'   \code{'par'}.} \item{\code{z}}{a numeric vector of z-values (parameter
#'   values in \code{'par'} divided by standard error \code{'se'}).}
#'   \item{\code{p}}{a numeric vector of p-values of parameter estimates
#'   (probabilities of \code{'z'} smaller than zero).} \item{\code{upper}}{a
#'   numeric vector of upper 95\% confindence limits of paramter estimates in
#'   \code{'par'}.} \item{\code{lower}}{a numeric vector of lower 95\%
#'   confindence limits of paramter estimates in \code{'par'}.}
#'
#' @author Mark Pletscher, <pletscher.mark@gmail.com>
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
  
  # Covariance matrix
  #------------------
  
  outlist[["cv"]] <- tryCatch({
    solve(outlist[["hessian"]])
  }, warning = function(w) {
    message(w)  
    solve(outlist[["hessian"]])
  }, error = function(e) {
    #message(e)
    matrix(data = NA, 
           nrow = nrow(outlist[["hessian"]]), 
           ncol = ncol(outlist[["hessian"]]))
  })
  
  # Standard errors, significance and confidence intervals of parameters
  #---------------------------------------------------------------------
  
  if (sum(is.na(outlist[["cv"]])) == nrow(outlist[["cv"]]) * 
      ncol(outlist[["cv"]])) {
    warning("no covariance matrix was obtained.",
            "\n")
    outlist[["se"]] <- rep(NA, times = length(par))
  } else if (sum(!is.na(diag(outlist[["cv"]]))) == 0) {
    warning("covariance matrix includes only missing diagonals",
            "\n")
    outlist[["se"]] <- rep(NA, times = length(par))
  } else if (sum(diag(outlist[["cv"]]) <= 0) > 0) {
    warning("covariance matrix includes non-positive diagnoals",
            "\n")
    outlist[["se"]] <- sqrt(diag(outlist[["cv"]]))
  } else if (sum(is.na(outlist[["se"]])) > 0) {
    warning("missing standard errors were obtained",
            "\n")
    outlist[["se"]] <- sqrt(diag(outlist[["cv"]]))
  } else {
    outlist[["se"]] <- sqrt(diag(outlist[["cv"]]))
  }
  names(outlist[["se"]]) <- names(par)
  
  outlist[["z"]] <- par / outlist[["se"]]
  names(outlist[["z"]]) <- names(par)
  
  outlist[["p"]] <- stats::pnorm(outlist[["z"]], lower.tail = FALSE)
  names(outlist[["p"]]) <- names(par)
  
  outlist[["upper"]] <- par + stats::qnorm(0.975) * outlist[["se"]]
  names(outlist[["upper"]]) <- names(par)
  
  outlist[["lower"]] <- par - stats::qnorm(0.975) * outlist[["se"]]
  names(outlist[["lower"]]) <- names(par)
  
  return(outlist)
}