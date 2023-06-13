#' Calculating analytical Gradients of the Negative Log-Likelihood
#'
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.gr}}}{\code{aldvmm.gr()}}
#' calculates analytical gradients of the negative log-likelihood with respect 
#' to parameter values in \code{'par'} for each observation in the estimation 
#' data.
#'
#' @inheritParams aldvmm.ll
#'
#' @details \ifelse{html}{\code{\link[aldvmm]{aldvmm.gr}}}{\code{aldvmm.gr()}}
#' calculates gradients of the negative log-likelihood.
#'
#' @return a named numeric matrix of first derivatives of the negative
#'   log-likelihood of the data with respect to parameters in \code{'par'}.
#'
#' @export

aldvmm.gr <- function(par,
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
  
  #----------------------------------------------------------------------------
  # Prepare list of parameters
  #----------------------------------------------------------------------------
  
  parlist <- aldvmm.getpar(par   = par,
                           lcoef = lcoef,
                           lcmp  = lcmp,
                           lcpar = lcpar,
                           ncmp  = ncmp)
  
  #----------------------------------------------------------------------------
  # Elements of likelihood function
  #----------------------------------------------------------------------------
  
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
  
  #----------------------------------------------------------------------------
  # Derivative of log-likelihood
  #----------------------------------------------------------------------------
  
  # Derivative w.r.t. beta
  #-----------------------
  
  if (dist == "normal") {
    # Density of values above maximum
    dCdb <- lapply(names(parlist[[lcoef[1]]]), function (x) {
      stats::dnorm((psi1 - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                   mean = 0, 
                   sd   = 1) * X[[lcoef[1]]] / exp(parlist[[lcpar]][[x]])
    })
    names(dCdb) <- names(parlist[[lcoef[1]]])
    
    # Density of values below minimum
    dDdb <- lapply(names(parlist[[lcoef[1]]]), function (x) {
      stats::dnorm((psi2 - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                   mean = 0, 
                   sd   = 1) * -X[[lcoef[1]]] / exp(parlist[[lcpar]][[x]])
    })
    names(dDdb) <- names(parlist[[lcoef[1]]])
    
    # Density of values within range
    dEdb <- lapply(names(parlist[[lcoef[1]]]), function (x) {
      stats::dnorm((y - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                   mean = 0, 
                   sd   = 1) * 
        X[[1]] * (y - xb[[x]]) / exp(parlist[[lcpar]][[x]])^3
    })
    names(dEdb) <- names(parlist[[lcoef[1]]])
    
    # Density of observed value
    dBdb <- lapply(names(parlist[[lcoef[1]]]), function (x) {
      as.numeric(y >  psi1) * dCdb[[x]] + 
        as.numeric(y <= psi2) * dDdb[[x]] + 
        as.numeric(y <= psi1 & y > psi2) * dEdb[[x]]
    })
    names(dBdb) <- names(parlist[[lcoef[1]]])
    
    # Derivative of likelihood
    dLdb <- lapply(names(parlist[[lcoef[1]]]), function (x) {
      sweep(matrix(dBdb[[x]], ncol = length(parlist[[lcoef[1]]][[x]])), 
            MARGIN = 1,
            A[[x]], 
            `*`)
    })
    names(dLdb) <- names(parlist[[lcoef[1]]])
    
    # Derivative of log-likelihood
    dlldb <- lapply(1:length(parlist[[lcoef[1]]]), function (x) {
      tmpmat <- sweep(dLdb[[x]], 
                      MARGIN = 1,
                      L, 
                      `/`)
      colnames(tmpmat) <- colnames(X[[1]])
      rownames(tmpmat) <- rownames(X[[1]])
      return(tmpmat)
    })
    names(dlldb) <- names(parlist[[lcoef[1]]])
  }
  
  # Derivative w.r.t. delta
  #------------------------
  
  # Derivative of probability of component of change variable
  dAdd <- lapply(names(parlist[[lcoef[2]]]), function (x) {
    (exp(wd[[x]]) * X[[lcoef[2]]] * sumexp - 
       exp(wd[[x]]) * exp(wd[[x]]) * X[[lcoef[2]]]) /
      sumexp^2
    
  })
  names(dAdd) <- names(parlist[[lcoef[2]]])
  
  # Derivative of probability of other components
  dAdd.inv <- lapply(names(parlist[[lcoef[2]]]), function (x) {
    tmplist <- lapply(names(parlist[[lcoef[2]]]), function (z) {
      (-exp(wd[[z]]) / sumexp^2) * exp(wd[[x]]) * X[[lcoef[2]]]
    })
    tmplist[[ncmp]] <- (-1 / sumexp^2) * exp(wd[[x]]) * X[[lcoef[2]]]
    return(tmplist)
  })
  
  names(dAdd.inv) <- names(parlist[[lcoef[2]]])
  
  dAdd.inv <- lapply(dAdd.inv, function (x) {
    names(x) <- names(parlist[[lcoef[1]]])
    return(x)
  })
  
  # Derivative of likelihood
  mat <- matrix(1, 
                ncol = length(parlist[[lcoef[1]]]),
                nrow = length(parlist[[lcoef[1]]]),
                dimnames = list(names(parlist[[lcoef[1]]]),
                                names(parlist[[lcoef[1]]])))
  diag(mat) <- 0
  
  dLdd <- lapply(names(dAdd), function (x) {
    sweep(matrix(dAdd[[x]], ncol = length(parlist[[lcoef[2]]][[x]])),
          MARGIN = 1,
          B[[x]],
          `*`) +
      Reduce("+", 
             lapply(names(dAdd.inv[[x]]), function (z) {
               mat[z, x] * sweep(matrix(dAdd.inv[[x]][[z]], 
                                        ncol = length(parlist[[lcoef[2]]][[x]])),
                                 MARGIN = 1,
                                 B[[z]],
                                 `*`)
             })
      )
  })
  names(dLdd) <- names(parlist[[lcoef[2]]])
  
  # Derivative of log-likelihood
  dlldd <- lapply(names(parlist[[lcoef[2]]]), function (x) {
    tmpmat <- sweep(dLdd[[x]],
                    MARGIN = 1,
                    L,
                    `/`)
    colnames(tmpmat) <- colnames(X[[2]])
    rownames(tmpmat) <- rownames(X[[2]])
    return(tmpmat)
  })
  names(dlldd) <- names(parlist[[lcoef[2]]])
  
  # Derivative w.r.t. sigma
  #------------------------
  
  if (dist == "normal") {
    
    # Density of values above maximum
    dCds <- lapply(names(parlist[[lcoef[1]]]), function (x) {
      stats::dnorm((psi1 - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                   mean = 0, 
                   sd   = 1) * 
        ((psi1 - xb[[x]]) / exp(parlist[[lcpar]][[x]])^2) *
        exp(parlist[[lcpar]][[x]])
    })
    names(dCds) <- names(parlist[[lcoef[1]]])
    
    # Density of values below minimum
    dDds <- lapply(names(parlist[[lcoef[1]]]), function (x) {
      stats::dnorm((psi2 - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                   mean = 0, 
                   sd   = 1) * 
        (-(psi2 - xb[[x]]) / exp(parlist[[lcpar]][[x]])^2) *
        exp(parlist[[lcpar]][[x]])
    })
    names(dDds) <- names(parlist[[lcoef[1]]])
    
    # Density of values within range
    dEds <- lapply(names(parlist[[lcoef[1]]]), function (x) {
      (stats::dnorm((y - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                    mean = 0, 
                    sd   = 1) * (y - xb[[x]])^2 / exp(parlist[[lcpar]][[x]])^4  - 
         stats::dnorm((y - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                      mean = 0, 
                      sd   = 1) / exp(parlist[[lcpar]][[x]])^2) *
        exp(parlist[[lcpar]][[x]])
    })
    names(dEds) <- names(parlist[[lcoef[1]]])
    
    # Density of observed values
    dBds <- lapply(names(parlist[[lcoef[1]]]), function (x) {
      as.numeric(y >  psi1) * dCds[[x]] + 
        as.numeric(y <= psi2) * dDds[[x]] + 
        as.numeric(y <= psi1 & y > psi2) * dEds[[x]]
    })
    names(dBds) <- names(parlist[[lcoef[1]]])
    
    # Derivative of likelihood
    dLds <- lapply(names(parlist[[lcoef[1]]]), function (x) {
      sweep(matrix(dBds[[x]], ncol = length(lcpar)), 
            MARGIN = 1,
            A[[x]], 
            `*`)
    })
    names(dLds) <- names(parlist[[lcoef[1]]])
    
    # Derivative of log-likelihood
    dllds <- lapply(1:length(parlist[[lcoef[1]]]), function (x) {
      tmpmat <- sweep(dLds[[x]], 
                      MARGIN = 1,
                      L, 
                      `/`)
      colnames(tmpmat) <- lcpar
      rownames(tmpmat) <- rownames(X[[1]])
      return(tmpmat)
    })
    names(dllds) <- names(parlist[[lcoef[1]]])
    
  }
  
  #----------------------------------------------------------------------------
  # Collect and return
  #----------------------------------------------------------------------------
  
  outmat <- -cbind(do.call("cbind", dlldb), 
                   do.call("cbind", dlldd), 
                   do.call("cbind", dllds))
  
  colnames(outmat) <- aldvmm.getnames(X,
                                      names = c(lcoef, lcpar),
                                      lcoef = lcoef,
                                      lcmp  = lcmp,
                                      lcpar = lcpar,
                                      ncmp = ncmp)
  
  # if (optim.method %in% c("L-BFGS-B", "Rcgmin")) {
  #   outmat[!is.finite(outmat)] <- 0
  # }
  
  return(outmat)
}
