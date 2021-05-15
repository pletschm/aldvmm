#' Predicting Expected Values from Adjusted Limited Dependent Variable Mixture
#' Models
#'
#' @description
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.pred}}}{\code{aldvmm::aldvmm.pred()}}
#' makes predictions of observations in design matrices in \code{'X'} using the
#' results of
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{\code{aldvmm::aldvmm()}}.
#'
#' @inheritParams aldvmm.ll
#'
#' @details
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm.pred}}}{\code{aldvmm::aldvmm.pred()}}
#'   calculates expected values for observations in design matrices in
#'   \code{'X'} using the expected value function published in Hernandez Alava
#'   and Wailoo (2015). Constant distribution parameters that need to be
#'   non-negative (i.e. standard deviations of normal distributions) enter the
#'   expected value function as log-transformed values.
#'
#' @return a named numeric vector of predicted outcomes. The names of the
#'   elements in the vector are identical to the row names of design matrices
#'   in \code{'X'}.
#'
#' @export

aldvmm.pred <- function(par,
                        X,
                        y = NULL,
                        psi,
                        ncmp,
                        dist,
                        lcoef,
                        lcpar,
                        lcmp) {
  
  # Prepare list of parameters
  #---------------------------
  
  parlist <- aldvmm.getpar(par   = par,
                           lcoef = lcoef,
                           lcmp  = lcmp,
                           lcpar = lcpar,
                           ncmp  = ncmp)
  
  # Calculate elements of likelihood function for each component and obs.
  #----------------------------------------------------------------------
  
  # Multinomial logit (parameters are only estimated for the first K - 1 
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
    
    p_c[, ncmp] <- 1 - rowSums(p_c[, 1:(ncmp - 1), drop = FALSE])
    
  } else {
    p_c <- matrix(data = 1, 
                  nrow = nrow(X[[1]]), 
                  ncol = ncmp,
                  dimnames = list(rownames(X[[1]]),
                                  paste0(lcmp, 1)))
  }
  
  # Densities
  #----------
  
  if (dist == "normal") {
    
    ev <- matrix(data = NA, 
                      nrow = nrow(X[[1]]), 
                      ncol = ncmp,
                      dimnames = list(rownames(X[[1]]),
                                      paste0(lcmp, 1:ncmp)))
    
    for (c in 1:ncmp) {
      
      max <- 1 - stats::pnorm((max(psi) - X[[1]] %*% parlist[[lcoef[1]]][[c]]) /
                                exp(parlist[[lcpar[1]]][[c]]),
                              mean = 0, 
                              sd   = 1)
      
      min <-     stats::pnorm((min(psi) - X[[1]] %*% parlist[[lcoef[1]]][[c]]) /
                                exp(parlist[[lcpar[1]]][[c]]), 
                              mean = 0, 
                              sd   = 1) * min(psi)
      
      mid_a <-  stats::pnorm((max(psi) - X[[1]] %*% parlist[[lcoef[1]]][[c]]) /
                                exp(parlist[[lcpar[1]]][[c]]), 
                              mean = 0, 
                              sd   = 1)
      
      mid_b <-  stats::pnorm((min(psi) - X[[1]] %*% parlist[[lcoef[1]]][[c]]) /
                                exp(parlist[[lcpar[1]]][[c]]), 
                              mean = 0, 
                              sd   = 1)
      
      mid_c <-  stats::dnorm((max(psi) - X[[1]] %*% parlist[[lcoef[1]]][[c]]) /
                                exp(parlist[[lcpar[1]]][[c]]), 
                              mean = 0, 
                              sd   = 1)
      
      mid_d <-  stats::dnorm((min(psi) - X[[1]] %*% parlist[[lcoef[1]]][[c]]) /
                                exp(parlist[[lcpar[1]]][[c]]), 
                              mean = 0, 
                              sd   = 1)
      
      mid_e <-  stats::pnorm((min(psi) - X[[1]] %*% parlist[[lcoef[1]]][[c]]) /
                                exp(parlist[[lcpar[1]]][[c]]), 
                              mean = 0, 
                              sd   = 1)
      
      mid_f <-  stats::pnorm((max(psi) - X[[1]] %*% parlist[[lcoef[1]]][[c]]) /
                                exp(parlist[[lcpar[1]]][[c]]), 
                              mean = 0, 
                              sd   = 1)
      
      mid   <-  (mid_a - mid_b) * (X[[1]] %*% parlist[[lcoef[1]]][[c]] +
                                        exp(parlist[[lcpar[1]]][[c]]) * 
                                        (mid_c - mid_d) / (mid_e - mid_f))
      
      ev[, c] <- max + min + mid
      
    }
    
  }
  
  # Predict
  #--------
  
  pred <- list()
  
  # Probabilities of group membership
  if (any(is.na(p_c))) {
    warning("fitted probabilities of component membership include missing 
            values\n",
            call. = FALSE)
  }
  
  pred[["prob"]] <- colMeans(p_c)
  names(pred[["prob"]]) <- paste0(lcmp, 1:ncmp)
  
  
  # Outcomes
  pred[["yhat"]] <- rowSums(p_c * ev)
  names(pred[["yhat"]]) <- rownames(X[[1]])
  
  if (!is.null(y)) {
    pred[["y"]] <- y
    names(pred[["y"]]) <- rownames(X[[1]])
    
    pred[["res"]]  <- pred[["y"]] - pred[["yhat"]]
    names(pred[["res"]]) <- rownames(X[[1]])
  }

  if (any(is.na(pred[["yhat"]]))) {
    warning("fitted values include missing values\n",
            call. = FALSE)
  }
  
  return(pred)
  
}