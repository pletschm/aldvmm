#' Calculating analytical Gradients of the Negative Log-Likelihood for each observation
#'
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.sc}}}{\code{aldvmm.sc()}}
#' calculates analytical gradients of the negative log-likelihood with respect 
#' to parameter values in \code{'par'} for each observation in the estimation 
#' data.
#'
#' @inheritParams aldvmm.ll
#'
#' @details \ifelse{html}{\code{\link[aldvmm]{aldvmm.sc}}}{\code{aldvmm.sc()}}
#' calculates gradients of the negative log-likelihood.
#'
#' @return a named numeric matrix of first derivatives of the negative
#'   log-likelihood of the data with respect to parameters in \code{'par'}.
#'
#' @export

aldvmm.sc <- function(par,
                      X,
                      y,
                      psi,
                      dist,
                      ncmp,
                      lcoef = lcoef,
                      lcmp  = lcmp,
                      lcpar = lcpar,
                      optim.method) {
  
  psi1 <- max(psi)
  psi2 <- min(psi)
  
  # Prepare list of parameters
  #---------------------------
  
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
  
  if (ncmp > 1) {
    
    # Linear predictor
    wd_mat <- W %*% Delta
    
    # Softmax relative to last component
    row_max <- do.call(pmax, as.data.frame(wd_mat)) # Maximum value within each row of wd_mat
    row_max <- pmax(row_max, 0)
    wd_shift <- wd_mat - row_max # Subtract maximum in each row to make numerically stable
    exp_wd <- exp(wd_shift)
    sumexp <- exp(-row_max) + rowSums(exp_wd)
    
    # Probability of component membership
    A_tmp <- exp_wd / sumexp
    A <- cbind(A_tmp, exp(-row_max) / sumexp)
    
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
  
  A[!is.finite(A)] <- 0
  B[!is.finite(B)] <- 0
  
  L <- rowSums(A * B)
  #L <- pmax(L, .Machine$double.xmin) # Guard against numerical underflow to zero before log
  #small <- (L <= .Machine$double.xmin) # Indicator or small L
  
  # Derivative w.r.t. delta
  #------------------------
  
  if (ncmp > 1) {
    
    # Number of predictors
    k_delta <- ncol(W)
    
    # Empty matrix of derivatives
    dll_dd_mat <- matrix(NA_real_, nrow = n, ncol = (ncmp - 1) * k_delta)
    
    col_start <- 1
    for (k in seq_len(ncmp - 1)) { # Loop over components
      
      # Case r = c
      dA_dwd_k <- matrix(0.0, n, ncmp)
      dA_dwd_k[, k] <- A[, k] * (1 - A[, k])
      
      # Case r != c
      for (r in seq_len(ncmp)) {
        if (r != k) {
          dA_dwd_k[, r] <- dA_dwd_k[, r] - A[, r] * A[, k]
        }
      }
      
      # Derivative of log-likelihood
      idx <- col_start:(col_start + k_delta - 1)
      dL_dwd_k <- rowSums(B * dA_dwd_k)
      dll_dd_mat[, idx] <- W * (dL_dwd_k / L)
      
      # Update column index
      col_start <- col_start + k_delta
    }
    
    # Set gradient at small values of L (-Inf of ll) to zero
    #dll_dd_mat[small, ] <- 0
    
  } else {
    
    # Derivative of log-likelihood
    dll_dd_mat <- matrix(numeric(0), nrow = n, ncol = 0)
    
  }
  
  # Derivative w.r.t. beta
  #-----------------------
  
  # Derivative w.r.t. b = derivative w.r.t. Xb multiplied by X (chain rule)
  
  if (dist == "normal") {
    
    # Density of values above maximum
    dC_dxb_vec <- stats::dnorm(z_C) / sigma_rep
    dC_dxb_mat <- matrix(dC_dxb_vec, n, ncmp)
    
    # Density of values below minimum
    dD_dxb_vec <- -stats::dnorm(z_D) / sigma_rep
    dD_dxb_mat <- matrix(dD_dxb_vec, n, ncmp)
    
    # Density of values within range
    dE_dxb_vec <- stats::dnorm(z_E) * z_E / (sigma_rep^2)
    dE_dxb_mat <- matrix(dE_dxb_vec, n, ncmp)
    
    # Derivative of density of observed value
    dB_dxb_mat <- dC_dxb_mat * m_above + dD_dxb_mat * m_below + dE_dxb_mat * m_inside
    
    # Derivative of log-likelihood
    dll_db_list <- vector("list", ncmp)
    for (j in seq_len(ncmp)) {
      dll_db_list[[j]] <- X_beta * (A[, j] * dB_dxb_mat[, j]) / L
      #dll_db_list[[j]][small, ] <- 0 # Set gradient at small values of L (-Inf of ll) to zero
    }
    
    dll_db_mat <- do.call(cbind, dll_db_list)
    
  }
  
  # Derivative w.r.t. sigma
  #------------------------
  
  if (dist == "normal") {
    
    # Density of values above maximum
    dC_ds_vec <- stats::dnorm(z_C) * z_C / sigma_rep
    dC_ds_mat <- matrix(dC_ds_vec, n, ncmp)
    
    # Density of values below minimum
    dD_ds_vec <- -stats::dnorm(z_D) * z_D / sigma_rep
    dD_ds_mat <- matrix(dD_ds_vec, n, ncmp)
    
    # Derivative of density of observed value
    dE_ds_vec <- stats::dnorm(z_E) * (z_E^2 - 1) / (sigma_rep^2)
    dE_ds_mat <- matrix(dE_ds_vec, n, ncmp)
    
    # Derivative of density of observed value
    dB_ds_mat <- dC_ds_mat * m_above + dD_ds_mat * m_below + dE_ds_mat * m_inside
    
    # Transform to ln(sigma)
    sigma_mat <- matrix(sigma, nrow = n, ncol = ncmp, byrow = TRUE)
    dB_dlns_mat <- dB_ds_mat * sigma_mat
    
    # Derivative of log-likelihood
    dll_ds_list <- vector("list", ncmp)
    for (j in seq_len(ncmp)) {
      dll_ds_list[[j]] <- (A[, j] * dB_dlns_mat[, j]) / L
      #dll_ds_list[[j]][small] <- 0 # Set gradient at small values of L (-Inf of ll) to zero
    }
    
    dll_ds_mat <- do.call(cbind, dll_ds_list)
  }
  
  # Collect and return
  #-------------------
  
  # Negative log-likelihood gradients
  outmat <- -(cbind(dll_db_mat, dll_dd_mat, dll_ds_mat))
  
  colnames(outmat) <- aldvmm.getnames(X,
                                      names = c(lcoef, lcpar),
                                      lcoef = lcoef,
                                      lcmp  = lcmp,
                                      lcpar = lcpar,
                                      ncmp  = ncmp)
  
  rownames(outmat) <- rownames(X[[lcoef[1]]])
  
  return(outmat)
}
