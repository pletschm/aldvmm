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
  
  # Derivative w.r.t. beta
  #-----------------------
  
  # Derivative w.r.t. b = derivative w.r.t. Xb multiplied by X (chain rule)
  
  if (dist == "normal") {
    
    # Derivative of density of values above maximum
    dC_dxb_vec <- stats::dnorm(z_C) / sigma_rep
    dC_dxb_mat <- matrix(dC_dxb_vec, n, ncmp)
    
    # Derivative of density of values below minimum
    dD_dxb_vec <- -stats::dnorm(z_D) / sigma_rep
    dD_dxb_mat <- matrix(dD_dxb_vec, n, ncmp)
    
    # Derivative of density of values within range
    dE_dxb_vec <- stats::dnorm(z_E) * (rep(y, times = ncmp) - xb_vec) / (sigma_rep^2)
    dE_dxb_mat <- matrix(dE_dxb_vec, n, ncmp)
    
    # Derivative of density of observed value
    dB_dxb <- dC_dxb_mat * m_above + dD_dxb_mat * m_below + dE_dxb_mat * m_inside
    
    # Derivative of log-likelihood
    # For component j: dL/dbeta_j = A[, j] * dB_dxb[, j] * X
    # Scale each row of X by A[, j] * dB_dxb[, j] / L
    
    dll_db_list <- vector("list", ncmp)
    for (j in seq_len(ncmp)) {
      dll_db_list[[j]] <- X_beta * (A[, j] * dB_dxb[, j]) / L
    }
    
    dll_db_mat <- do.call(cbind, dll_db_list)
    
  }
  
  # Derivative w.r.t. delta
  #------------------------
  
  # wd_mat are logits for components k in 1..(ncmp-1)
  # baseline is last component with value 0
  # For k in 1..(ncmp-1):
  # dA[, k]/dwd[, k] = A[, k] * (1 - A[, k])
  # dA[, r]/dwd[, k] = -A[, r] * A[, k], for r != k
  # dwd/d(delta_k) = W
  
  if (ncmp > 1) {
    
    p_delta <- ncol(W) # Number of predictors
    
    # Precompute A interactions
    # For each k, build dA/dwd_k: n × ncmp
    # Then dL/dwd_k = sum_j B[, j] * dA[, j]/dwd_k
    # Finally, dL/d(delta_k) = (dL/dwd_k) * W (row-wise scaling across columns)
    
    dll_dd_mat <- matrix(NA_real_, nrow = n, ncol = (ncmp - 1) * p_delta)
    
    col_start <- 1
    for (k in seq_len(ncmp - 1)) { # Loop over components
      
      dA_dwd_k <- matrix(0.0, n, ncmp)
      
      dA_dwd_k[, k] <- A[, k] * (1 - A[, k])
      
      for (r in seq_len(ncmp)) {
        if (r != k) {
          dA_dwd_k[, r] <- dA_dwd_k[, r] - A[, r] * A[, k]
        }
      }
      
      dL_dwd_k <- rowSums(B * dA_dwd_k)
      
      idx <- col_start:(col_start + p_delta - 1)
      
      dll_dd_mat[, idx] <- W * (dL_dwd_k / L)
      
      col_start <- col_start + p_delta
    }
  } else {
    
    dll_dd_mat <- matrix(numeric(0), nrow = n, ncol = 0)
    
  }
  
  # Derivative w.r.t. sigma
  #------------------------
  
  if (dist == "normal") {
    
    # dC/dsigma = dnorm(z_C) * (psi1 - mu) / sigma^2
    dC_ds_vec <- stats::dnorm(z_C) * (psi1 - xb_vec) / (sigma_rep^2)
    
    # dD/dsigma = dnorm(z_D) * (-(psi2 - mu)) / sigma^2
    dD_ds_vec <- stats::dnorm(z_D) * (-(psi2 - xb_vec)) / (sigma_rep^2)
    
    # dE/dsigma: derivative of dnorm((y - mu)/sigma)/sigma
    # E = dnorm(z_E)/sigma, z_E = (y - mu)/sigma
    # dE/dsigma = dnorm(z_E) * ((y - mu)^2 / sigma^4 - 1 / sigma^2)
    res_vec <- rep(y, times = ncmp) - xb_vec
    dE_ds_vec <- stats::dnorm(z_E) * ( (res_vec^2) / (sigma_rep^4) - 1 / (sigma_rep^2) )
    
    dC_ds <- matrix(dC_ds_vec, n, ncmp)
    dD_ds <- matrix(dD_ds_vec, n, ncmp)
    dE_ds <- matrix(dE_ds_vec, n, ncmp)
    
    dB_ds <- dC_ds * m_above + dD_ds * m_below + dE_ds * m_inside  # n × ncmp
    
    # Convert derivative w.r.t. sigma to derivative w.r.t. log-sigma (chain rule)
    # d/d(log_sigma) = d/dsigma * dsigma/dlog_sigma = d/dsigma * sigma
    # Multiply each component column by sigma_j
    
    dB_dlogs <- sweep(dB_ds, 2, sigma, `*`)
    
    # dL/dlogsigma_j = A[, j] * dB_dlogs[, j]
    dll_ds_list <- vector("list", ncmp)
    
    for (j in seq_len(ncmp)) {
      dll_ds_list[[j]] <- (A[, j] * dB_dlogs[, j])
    }
    
    dL_dlogs_mat <- do.call(cbind, dll_ds_list)
    dll_ds_mat <- dL_dlogs_mat / L
    
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
