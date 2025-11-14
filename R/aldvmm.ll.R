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
  
  # Create design matrices
  #-----------------------
  
  # Model of expected values per component
  X_beta  <- X[[lcoef[1]]]
  storage.mode(X_beta)  <- "double"
  
  # Model of probabilities of component membership
  if (ncmp > 1) {
    W <- X[[lcoef[2]]]
    storage.mode(W) <- "double"
  } else {
    W <- NULL
  }
  
  # Number of observations
  n <- nrow(X_beta)
  
  # Create matrices of parameters
  #------------------------------
  
  # Model of expected values per component
  Beta <- do.call(cbind, parlist[[lcoef[1]]])
  storage.mode(Beta) <- "double"
  
  # Model of probability of component membership
  if (ncmp > 1) {
    Delta <- do.call(cbind, parlist[[lcoef[2]]])
    storage.mode(Delta) <- "double"
  }
  
  # Multinomial logit
  #------------------
  
  # Last component is the baseline
  
  if (ncmp > 1) {
    
    # Linear predictor
    wd_mat <- W %*% Delta
    
    # Softmax relative to last component
    row_max <- do.call(pmax, as.data.frame(wd_mat)) # Maximum value within each row of wd_mat
    wd_shift <- wd_mat - row_max # Subtract maximum in each row to make numerically stable
    exp_wd <- exp(wd_shift)
    sumexp <- 1 + rowSums(exp_wd)
    
    # Probability of component membership
    A_tmp <- exp_wd / sumexp
    A <- matrix(NA_real_, nrow = n, ncol = ncmp)
    A[, seq_len(ncmp - 1)] <- A_tmp
    A[, ncmp] <- 1 - rowSums(A_tmp)
    
  } else {
    
    A <- matrix(1, nrow = n, ncol = 1)
    
  }
  
  # Component distributions
  #------------------------
  
  if (dist == "normal") {
    
    # Standard deviation per component
    sigma <- exp(unlist(parlist[[lcpar]]))
    sigma_rep <- rep(sigma, each = n) # Repeat for each patient per component
    
    # Linear predictor
    xb_mat <- X_beta %*% Beta
    xb_vec <- as.vector(xb_mat)
    
    # Density of values above maximum
    z_C <- (psi1 - xb_vec) / sigma_rep
    C_mat <- matrix(1 - stats::pnorm(z_C), n, ncmp)
    
    # Density of values below minimum
    z_D <- (psi2 - xb_vec) / sigma_rep
    D_mat <- matrix(stats::pnorm(z_D), n, ncmp)
    
    # Density of value within range
    z_E   <- (rep(y, times = ncmp) - xb_vec) / sigma_rep
    E_mat <- matrix(stats::dnorm(z_E) / sigma_rep, n, ncmp)
    
    # Density of observed value
    m_above  <- as.numeric(y >  psi1)
    m_below  <- as.numeric(y <= psi2)
    m_inside <- 1 - m_above - m_below
    
    B <- C_mat * m_above + D_mat * m_below + E_mat * m_inside
    
  }
  
  # Likelihood
  #-----------
  
  L <- rowSums(A * B)
  L <- pmax(L, .Machine$double.xmin) # Guard against numerical underflow to zero before log
  
  ll <- sum(log(L))
  
  if (optim.method %in% c("L-BFGS-B", "Rcgmin") & !is.finite(ll)) {
    ll <- -1e+20
  }
  
  return(-ll)
  
}
