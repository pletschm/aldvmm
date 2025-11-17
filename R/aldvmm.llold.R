#' Calculating the Negative Log-Likelihood of the Adjusted Limited Dependent
#' Variable Mixture Model
#'
#'
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.llold}}}{\code{aldvmm.llold()}}
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
#' @inheritParams aldvmm
#'
#' @details \ifelse{html}{\code{\link[aldvmm]{aldvmm.llold}}}{\code{aldvmm.llold()}}
#'   calculates the negative log-likelihood of the adjusted limited dependent
#'   variable mixture model using the likelihood function published in
#'   Hernandez Alava and Wailoo (2015). Constant distribution parameters that
#'   need to be non-negative (i.e. the standard deviations of normal
#'   distributions) enter the likelihood function as log-transformed values.
#'
#'   As the "L-BFGS-B" and "Rcgmin" methods in
#'   \ifelse{html}{\code{\link[optimx]{optimr}}}{\code{optimx::optimr()}} fail
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
#' @references Alava, M. H. and Wailoo, A. (2015) Fitting adjusted limited
#'   dependent variable mixture models to EQ-5D. \emph{The Stata Journal},
#'   \bold{15(3)}, 737--750. \doi{10.1177/1536867X1501500307} \cr
#'
#' @return a scalar of the negative log-likelihood of the data at parameter
#'   values in \code{'par'}.
#'
#' @export

aldvmm.llold <- function(par,
                      X,
                      y,
                      psi,
                      ncmp,
                      dist,
                      lcoef,
                      lcpar,
                      lcmp,
                      optim.method) {
  
  psi1 <- max(psi)
  psi2 <- min(psi)
  
  # Check if par has names
  #-----------------------
  
  checkmate::assert_numeric(par, names = "named")
  
  # Create list of parameters
  #--------------------------
  
  parlist <- aldvmm.getpar(par   = par,
                           lcoef = lcoef,
                           lcmp  = lcmp,
                           lcpar = lcpar,
                           ncmp  = ncmp)
  
  # Multinomial logit
  #------------------
  
  if (ncmp > 1) {
    
    # Linear predictor
    wd <- lapply(names(parlist[[lcoef[2]]]), function (x) {
      rowSums(sweep(X[[lcoef[2]]], 
                    MARGIN = 2, 
                    parlist[[lcoef[2]]][[x]], 
                    `*`))
    })
    names(wd) <- names(parlist[[lcoef[2]]])
    
    # Denominator
    sumexp <- 1 + Reduce("+",
                         lapply(names(parlist[[lcoef[2]]]), function (z) {
                           exp(rowSums(sweep(X[[lcoef[2]]], 
                                             MARGIN = 2, 
                                             parlist[[lcoef[2]]][[z]], 
                                             `*`)))
                         }))
    
    # Probability of component membership
    A <- lapply(names(parlist[[lcoef[2]]]), function (x) {
      exp(wd[[x]]) / sumexp
    })
    A[[ncmp]] <- 1 - Reduce("+", A)
  } else {
    A <- list(
      matrix(data = 1, 
             nrow = nrow(X[[lcoef[1]]]), 
             ncol = 1,
             dimnames = list(rownames(X[[lcoef[2]]]),
                             paste0(lcmp, 1)))
    )
  }
  names(A) <- names(parlist[[lcoef[1]]])
  
  # Component distributions
  #------------------------
  
  if (dist == "normal") {
    
    # Linear predictor
    xb <- lapply(parlist[[lcoef[1]]], function (x) {
      rowSums(sweep(X[[lcoef[1]]], 
                    MARGIN = 2, 
                    x, 
                    `*`))
    })
    names(xb) <- names(parlist[[lcoef[1]]])
    
    # Density of values above maximum
    C <- lapply(names(parlist[[lcoef[1]]]), function (x) {
      1 - stats::pnorm((psi1 - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                       mean = 0, 
                       sd = 1)
    })
    names(C) <- names(parlist[[lcoef[1]]])
    
    # Density of values below minimum
    D <- lapply(names(parlist[[lcoef[1]]]), function (x) {
      stats::pnorm((psi2 - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                   mean = 0, 
                   sd = 1)
    })
    names(D) <- names(parlist[[lcoef[1]]])
    
    # Density of value within range
    E <- lapply(names(parlist[[lcoef[1]]]), function (x) {
      stats::dnorm((y - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                   mean = 0, 
                   sd = 1) / exp(parlist[[lcpar]][[x]])
    })
    names(E) <- names(parlist[[lcoef[1]]])
    
    # Density of observed value
    B <- lapply(names(parlist[[lcoef[1]]]), function (x) {
      as.numeric(y >  psi1) * C[[x]] + 
        as.numeric(y <= psi2) * D[[x]] + 
        as.numeric(y <= psi1 & y > psi2) * E[[x]]
    })
    names(B) <- names(parlist[[lcoef[1]]])
    
  }
  
  # Likelihood
  #-----------
  
  L <- Reduce("+",
              lapply(names(A), function (x) {
                A[[x]] * B[[x]]
              })
  )
  
  ll <- sum(log(L))
  
  if (optim.method %in% c("L-BFGS-B", "Rcgmin") & !is.finite(ll)) {
    ll <- -1e+20
  }
  
  return(-ll)
  
}