aldvmm.sc <- function(par,
                      y,
                      X,
                      psi,
                      ncmp,
                      dist,
                      lcoef = lcoef,
                      lcmp  = lcmp,
                      lcpar = lcpar) {
  
  psi1 <- max(psi)
  psi2 <- min(psi)
  
  # Prepare list of parameters
  #---------------------------
  
  parlist <- aldvmm.getpar(par   = par,
                           lcoef = lcoef,
                           lcmp  = lcmp,
                           lcpar = lcpar,
                           ncmp  = ncmp)
  
  # Elements of likelihood function
  #--------------------------------
  
  if (ncmp > 1) {
    A <- lapply(1:length(parlist[[lcoef[2]]]), function (x) {
      exp(rowSums(sweep(X[[lcoef[2]]], 
                        MARGIN = 2, 
                        parlist[[lcoef[2]]][[x]], 
                        `*`))) /
        (1 + Reduce("+",
               lapply(1:length(parlist[[lcoef[2]]]), function (z) {
                 exp(rowSums(sweep(X[[lcoef[2]]], 
                                   MARGIN = 2, 
                                   parlist[[lcoef[2]]][[z]], 
                                   `*`)))
               })))
    })
    A[[ncmp]] <- 1 - Reduce("+", A)
  } else {
    A <- matrix(data = 1, 
                nrow = nrow(X[[lcoef[2]]]), 
                ncol = 1,
                dimnames = list(rownames(X[[lcoef[2]]]),
                                paste0(lcmp, 1)))
  }
  names(A)
  
  C <- lapply(1:length(parlist[[lcoef[1]]]), function (x) {
    1 - stats::pnorm((psi1 - rowSums(sweep(X[[lcoef[1]]], 
                                           MARGIN = 2, 
                                           parlist[[lcoef[1]]][[x]], 
                                           `*`))) / exp(parlist[[lcpar]][[x]]), 
                     mean = 0, 
                     sd = 1)
  })
  names(C) <- names(parlist[[lcoef[1]]])
  
  D <- lapply(1:length(parlist[[lcoef[1]]]), function (x) {
    stats::pnorm((psi2 - rowSums(sweep(X[[lcoef[1]]], 
                                       MARGIN = 2, 
                                       parlist[[lcoef[1]]][[x]], 
                                       `*`))) / exp(parlist[[lcpar]][[x]]), 
                 mean = 0, 
                 sd = 1)
  })
  names(D) <- names(parlist[[lcoef[1]]])
  
  E <- lapply(1:length(parlist[[lcoef[1]]]), function (x) {
    stats::pnorm((y - rowSums(sweep(X[[lcoef[1]]], 
                                    MARGIN = 2, 
                                    parlist[[lcoef[1]]][[x]], 
                                    `*`))) / exp(parlist[[lcpar]][[x]]), 
                 mean = 0, 
                 sd = 1) / exp(parlist[[lcpar]][[x]])
  })
  names(E) <- names(parlist[[lcoef[1]]])
  
  B <- lapply(1:length(parlist[[lcoef[1]]]), function (x) {
    as.numeric(y >  psi1) * C[[x]] + 
      as.numeric(y <= psi2) * D[[x]] + 
      as.numeric(y <= psi1 & y > psi2) * E[[x]]
  })
  
  names(B) <- names(parlist[[lcoef[1]]])
  
  L <- Reduce("+",
              lapply(1:ncmp, function (x) {
                A[[x]] * B[[x]]
              })
  )
  
  # Derivative w.r.t. beta
  #-----------------------
  
  dCdb <- lapply(1:length(parlist[[lcoef[1]]]), function (x) {
    stats::pnorm((psi1 - sweep(X[[lcoef[1]]], 
                               MARGIN = 2, 
                               parlist[[lcoef[1]]][[x]], 
                               `*`)) / exp(parlist[[lcpar]][[x]]), 
                 mean = 0, 
                 sd   = 1) * 
      (-X[[1]] * (psi1 - sweep(X[[lcoef[1]]], 
                               MARGIN = 2, 
                               parlist[[lcoef[1]]][[x]], 
                               `*`))) / exp(parlist[[lcpar]][[x]])^2
  })
  names(dCdb) <- names(parlist[[lcoef[1]]])
  
  dDdb <- lapply(1:length(parlist[[lcoef[1]]]), function (x) {
    stats::pnorm((psi2 - sweep(X[[lcoef[1]]], 
                               MARGIN = 2, 
                               parlist[[lcoef[1]]][[x]], 
                               `*`)) / exp(parlist[[lcpar]][[x]])^2, 
                 mean = 0, 
                 sd   = 1) * 
      (X[[1]] * (psi2 - sweep(X[[lcoef[1]]], 
                              MARGIN = 2, 
                              parlist[[lcoef[1]]][[x]], 
                              `*`))) / exp(parlist[[lcpar]][[x]])^2
  })
  names(dDdb) <- names(parlist[[lcoef[1]]])
  
  dEdb <- lapply(1:length(parlist[[lcoef[1]]]), function (x) {
    stats::pnorm((y - sweep(X[[lcoef[1]]], 
                            MARGIN = 2, 
                            parlist[[lcoef[1]]][[x]], 
                            `*`)) / exp(parlist[[lcpar]][[x]]), 
                 mean = 0, 
                 sd   = 1) * 
      (X[[1]] * (y - sweep(X[[lcoef[1]]], 
                           MARGIN = 2, 
                           parlist[[lcoef[1]]][[x]], 
                           `*`))) / exp(parlist[[lcpar]][[x]])^3
  })
  names(dEdb) <- names(parlist[[lcoef[1]]])
  
  dBdb <- lapply(1:length(parlist[[lcoef[1]]]), function (x) {
    as.numeric(y >  psi1) * dCdb[[x]] + 
      as.numeric(y <= psi2) * dDdb[[x]] + 
      as.numeric(y <= psi1 & y > psi2) * dEdb[[x]]
  })
  names(dBdb) <- names(parlist[[lcoef[1]]])
  
  dLdb <- lapply(1:length(parlist[[lcoef[1]]]), function (x) {
    sweep(dBdb[[x]], 
          MARGIN = 1,
          A[[x]], 
          `*`)
  })
  names(dLdb) <- names(parlist[[lcoef[1]]])
  
  dlldb <- lapply(1:length(parlist[[lcoef[1]]]), function (x) {
    sweep(dLdb[[x]], 
          MARGIN = 1,
          L, 
          `/`)
  })
  
  do.call("cbind", dlldb)
}
