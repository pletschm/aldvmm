#' Calculating the Negative Log-Likelihood of the Adjusted Limited Dependent
#' Variable Mixture Model
#'
#'
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.ll}}}{\code{aldvmm.ll()}}
#' calculates the negative log-likelihood of \code{'data'} supplied to
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{\code{aldvmm()}} at the
#' parameter values in \code{'par'}.
#'
#' @param par a named numeric vector of parameter values.
#' @param X a list of design matrices returned by
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm.mm}}}{\code{aldvmm.mm()}}.
#'   \code{'X'} is of length 2 and includes a design matrix for the model of
#'   component distributions and a design matrix for the model of probabilities
#'   of group membership.
#' @param y a numeric vector of observed outcomes from complete observations in
#'   \code{'data'} supplied to
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{\code{aldvmm()}}.
#' @param lcoef a character vector of length 2 with labels of objects including
#'   regression coefficients of component distributions (default \code{"beta"})
#'   and coefficients of probabilities of component membership (default
#'   \code{"delta"}).
#' @param lcpar a character vector with the labels of objects including
#'   constant parameters of component distributions (e.g. the standard
#'   deviation of the normal distribution). The length of \code{'lcpar'}
#'   depends on the distribution supplied to \code{'dist'}.
#' @param lcmp a character value representing a stub (default \code{"Comp"})
#'   for labeling objects including regression coefficients in different
#'   components (e.g. "Comp1", "Comp2", ...). This label is also used in
#'   summary tables returned by
#'   \ifelse{html}{\code{\link[aldvmm]{summary.aldvmm}}}{\code{summary.aldvmm()}}.
#'
#'
#'
#'
#'
#'
#'
#' @inheritParams aldvmm
#'
#' @details \ifelse{html}{\code{\link[aldvmm]{aldvmm.ll}}}{\code{aldvmm.ll()}}
#'   calculates the negative log-likelihood of the adjusted limited dependent
#'   variable mixture model using the likelihood function published in
#'   Hernandez Alava and Wailoo (2015). Constant distribution parameters that
#'   need to be non-negative (i.e. the standard deviations of normal
#'   distributions) enter the likelihood function as log-transformed values.
#'
#'   As the "L-BFGS-B" and "Rcgmin" methods in
#'   \ifelse{html}{\code{\link[optimr]{optimr}}}{\code{optimr::optimr()}} fail
#'   if they encounter infinite values, the log-likelihood function takes the
#'   value -1e+20 if it is infinite during these algorithms.
#'
#'   The names of the parameter vector supplied to \code{'par'} must be
#'   generated using \ifelse{html}{\code{\link[aldvmm]{aldvmm.getnames}}}{
#'   \code{aldvmm.getnames()}} because they will be inherited by return values
#'   of other functions in the package \code{'aldvmm'}. The names will also be
#'   used in the extraction of parameters from parameter vectors into nested
#'   lists using
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm.getpar}}}{\code{aldvmm.getpar()}}.
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @references Alava, M. H. and Wailoo, A. (2015) Fitting adjusted limited
#'   dependent variable mixture models to EQ-5D. \emph{The Stata Journal},
#'   \bold{15(3)}, 737--750. \doi{10.1177/1536867X1501500307} \cr
#'
#' @return a scalar of the negative log-likelihood of the data at parameter
#'   values in \code{'par'}.
#'
#' @export

aldvmm.ll <- function(par,
                      X,
                      y,
                      psi,
                      ncmp,
                      dist,
                      lcoef,
                      lcpar,
                      lcmp,
                      optim.method) {
  
  # Prepare list of parameters
  #---------------------------
  
  parlist <- aldvmm.getpar(par   = par,
                           lcoef = lcoef,
                           lcmp  = lcmp,
                           lcpar = lcpar,
                           ncmp  = ncmp)
  
  # Calculate elements of likelihood function for each component and obs.
  #----------------------------------------------------------------------
  
  # Mulinomial logit (parameters are only estimated for the first K - 1 
  # components).
  
  if (ncmp > 1) {
    exp_xd <- matrix(data = NA, 
                     nrow = nrow(X[[2]]), 
                     ncol = (ncmp - 1),
                     dimnames = list(rownames(X[[2]]),
                                     paste0(lcmp, 1:(ncmp - 1))))
    
    for (c in 1:(ncmp - 1)) {
      exp_xd[, c] <- exp(X[[2]] %*% parlist[[lcoef[2]]][[c]])
    }
    
    p_c <- matrix(data = NA, 
                  nrow = nrow(X[[2]]), 
                  ncol = ncmp,
                  dimnames = list(rownames(X[[2]]),
                                  paste0(lcmp, 1:ncmp)))
    
    for (c in 1:(ncmp - 1)) {
      p_c[, c] <- exp_xd[, c] / (1 + rowSums(exp_xd))
    }
    p_c[, ncmp] <- 1 -  rowSums(p_c, na.rm = TRUE)
    
  } else {
    p_c <- matrix(data = 1, 
                  nrow = nrow(X[[1]]), 
                  ncol = 1,
                  dimnames = list(rownames(X[[1]]),
                                  paste0(lcmp, 1)))
  }
  
  # Indicators of value range of y
  #-------------------------------
  
  I <- cbind(as.numeric(y >  max(psi)),
             as.numeric(y <= min(psi)),
             as.numeric(y <= max(psi) & y > min(psi)))
  
  # Densities
  #----------
  
  density <- list()
  
  if (dist == "normal") {
    
    for (c in 1:ncmp) {
      
      max  <- 1 - stats::pnorm((max(psi) - 
                                  X[[1]] %*% parlist[[lcoef[1]]][[c]]) /
                                 exp(parlist[[lcpar[1]]][[c]]),
                               mean = 0,
                               sd   = 1)
      min  <-     stats::pnorm((min(psi) - 
                                  X[[1]] %*% parlist[[lcoef[1]]][[c]]) /
                                 exp(parlist[[lcpar[1]]][[c]]),
                               mean = 0,
                               sd   = 1)
      prob <-     stats::dnorm((y - X[[1]] %*% parlist[[lcoef[1]]][[c]]) /
                                 exp(parlist[[lcpar[1]]][[c]]),
                               mean = 0,
                               sd   = 1) / exp(parlist[[lcpar[1]]][[c]])
      
      density[[c]] <- cbind(max, min, prob)
      
    }
    
  }
  
  # Calculate likelihood
  #---------------------
  
  cont <- matrix(data = NA, 
                 nrow = nrow(X[[1]]), 
                 ncol = ncmp,
                 dimnames = list(rownames(X[[1]]),
                                 paste0(lcmp, 1:ncmp)))
  
  for (c in 1:ncmp) {
    cont[, c] <- p_c[, c] * rowSums(I * density[[c]])
  }
  
  ll <- sum(log(rowSums(cont)))
  
  if (optim.method %in% c("L-BFGS-B", "Rcgmin") & !is.finite(ll)) {
      ll <- -1e+20 
  }
  
  return(-ll)
  
}
