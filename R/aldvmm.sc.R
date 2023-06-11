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
  
  # Multinomial logit
  if (ncmp > 1) {
    wd <- lapply(names(parlist[[lcoef[2]]]), function (x) {
      rowSums(sweep(X[[lcoef[2]]], 
                    MARGIN = 2, 
                    parlist[[lcoef[2]]][[x]], 
                    `*`))
    })
    names(wd) <- names(parlist[[lcoef[2]]])
    
    sumexp <- 1 + Reduce("+",
                         lapply(names(parlist[[lcoef[2]]]), function (z) {
                           exp(rowSums(sweep(X[[lcoef[2]]], 
                                             MARGIN = 2, 
                                             parlist[[lcoef[2]]][[z]], 
                                             `*`)))
                         }))
    
    A <- lapply(names(parlist[[lcoef[2]]]), function (x) {
      exp(wd[[x]]) / sumexp
    })
    A[[ncmp]] <- 1 - Reduce("+", A)
  } else {
    A <- matrix(data = 1, 
                nrow = nrow(X[[lcoef[2]]]), 
                ncol = 1,
                dimnames = list(rownames(X[[lcoef[2]]]),
                                paste0(lcmp, 1)))
  }
  names(A) <- names(parlist[[lcoef[1]]])
  
  # Component distributions
  
  xb <- lapply(parlist[[lcoef[1]]], function (x) {
    rowSums(sweep(X[[lcoef[1]]], 
                  MARGIN = 2, 
                  x, 
                  `*`))
  })
  names(xb) <- names(parlist[[lcoef[1]]])
  
  C <- lapply(names(parlist[[lcoef[1]]]), function (x) {
    1 - stats::pnorm((psi1 - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                     mean = 0, 
                     sd = 1)
  })
  names(C) <- names(parlist[[lcoef[1]]])
  
  D <- lapply(names(parlist[[lcoef[1]]]), function (x) {
    stats::pnorm((psi2 - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                 mean = 0, 
                 sd = 1)
  })
  names(D) <- names(parlist[[lcoef[1]]])
  
  E <- lapply(names(parlist[[lcoef[1]]]), function (x) {
    stats::dnorm((y - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                 mean = 0, 
                 sd = 1) / exp(parlist[[lcpar]][[x]])
  })
  names(E) <- names(parlist[[lcoef[1]]])
  
  B <- lapply(names(parlist[[lcoef[1]]]), function (x) {
    as.numeric(y >  psi1) * C[[x]] + 
      as.numeric(y <= psi2) * D[[x]] + 
      as.numeric(y <= psi1 & y > psi2) * E[[x]]
  })
  names(B) <- names(parlist[[lcoef[1]]])
  
  L <- Reduce("+",
              lapply(names(A), function (x) {
                A[[x]] * B[[x]]
              })
  )
  
  # Derivative w.r.t. beta
  #-----------------------
  
  dCdb <- lapply(names(parlist[[lcoef[1]]]), function (x) {
    stats::dnorm((psi1 - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                 mean = 0, 
                 sd   = 1) * X[[lcoef[1]]] / exp(parlist[[lcpar]][[x]])
  })
  names(dCdb) <- names(parlist[[lcoef[1]]])
  
  dDdb <- lapply(names(parlist[[lcoef[1]]]), function (x) {
    stats::dnorm((psi2 - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                 mean = 0, 
                 sd   = 1) * -X[[lcoef[1]]] / exp(parlist[[lcpar]][[x]])
  })
  names(dDdb) <- names(parlist[[lcoef[1]]])
  
  dEdb <- lapply(names(parlist[[lcoef[1]]]), function (x) {
    stats::dnorm((y - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                 mean = 0, 
                 sd   = 1) * 
      X[[1]] * (y - xb[[x]]) / exp(parlist[[lcpar]][[x]])^3
  })
  names(dEdb) <- names(parlist[[lcoef[1]]])
  
  dBdb <- lapply(names(parlist[[lcoef[1]]]), function (x) {
    as.numeric(y >  psi1) * dCdb[[x]] + 
      as.numeric(y <= psi2) * dDdb[[x]] + 
      as.numeric(y <= psi1 & y > psi2) * dEdb[[x]]
  })
  names(dBdb) <- names(parlist[[lcoef[1]]])
  
  dLdb <- lapply(names(parlist[[lcoef[1]]]), function (x) {
    sweep(matrix(dBdb[[x]], ncol = length(parlist[[lcoef[1]]][[x]])), 
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
  names(dlldb) <- names(parlist[[lcoef[1]]])
  
  # Derivative w.r.t. delta
  #------------------------
  
  # START EXPERIMENTAL ---------------
  
  dAdd <- lapply(names(parlist[[lcoef[2]]]), function (x) {
    (exp(wd[[x]]) * X[[lcoef[2]]] * sumexp - 
       exp(wd[[x]]) * exp(wd[[x]]) * X[[lcoef[2]]]) /
      sumexp^2
    
  })
  names(dAdd) <- names(parlist[[lcoef[2]]])
  
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
  
  mat <- matrix(1, 
                ncol = length(parlist[[lcoef[1]]]),
                nrow = length(parlist[[lcoef[1]]]),
                dimnames = list(names(parlist[[lcoef[1]]]),
                                names(parlist[[lcoef[1]]])))
  mat <- lower.tri(mat) * mat
  
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
  
  dlldd <- lapply(names(parlist[[lcoef[2]]]), function (x) {
    sweep(dLdd[[x]],
          MARGIN = 1,
          L,
          `/`)
  })
  names(dlldd) <- names(parlist[[lcoef[2]]])
  
  # END EXPERIMENTAL ---------------
  
  # dAdd <- lapply(names(parlist[[lcoef[2]]]), function (x) {
  #   (exp(wd[[x]]) * X[[lcoef[2]]] * sumexp - 
  #      exp(wd[[x]]) * exp(wd[[x]]) * X[[lcoef[2]]]) /
  #     sumexp^2
  #   
  # })
  # names(dAdd) <- names(parlist[[lcoef[2]]])
  # 
  # dLdd <- lapply(names(parlist[[lcoef[2]]]), function (x) {
  #   sweep(matrix(dAdd[[x]], ncol = length(parlist[[lcoef[2]]][[x]])),
  #         MARGIN = 1,
  #         B[[x]],
  #         `*`)
  # })
  # names(dLdd) <- names(parlist[[lcoef[2]]])
  # 
  # dlldd <- lapply(names(parlist[[lcoef[2]]]), function (x) {
  #   sweep(dLdd[[x]],
  #         MARGIN = 1,
  #         L,
  #         `/`)
  # })
  # names(dlldd) <- names(parlist[[lcoef[2]]])
  
  # Derivative w.r.t. sigma
  #------------------------
  
  dCds <- lapply(names(parlist[[lcoef[1]]]), function (x) {
    stats::dnorm((psi1 - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                 mean = 0, 
                 sd   = 1) * 
      ((psi1 - xb[[x]]) / exp(parlist[[lcpar]][[x]])^2) *
      exp(parlist[[lcpar]][[x]])
  })
  names(dCds) <- names(parlist[[lcoef[1]]])
  
  dDds <- lapply(names(parlist[[lcoef[1]]]), function (x) {
    stats::dnorm((psi2 - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                 mean = 0, 
                 sd   = 1) * 
      (-(psi2 - xb[[x]]) / exp(parlist[[lcpar]][[x]])^2) *
      exp(parlist[[lcpar]][[x]])
  })
  names(dDds) <- names(parlist[[lcoef[1]]])
  
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
  
  dBds <- lapply(names(parlist[[lcoef[1]]]), function (x) {
    as.numeric(y >  psi1) * dCds[[x]] + 
      as.numeric(y <= psi2) * dDds[[x]] + 
      as.numeric(y <= psi1 & y > psi2) * dEds[[x]]
  })
  names(dBds) <- names(parlist[[lcoef[1]]])
  
  dLds <- lapply(names(parlist[[lcoef[1]]]), function (x) {
    sweep(matrix(dBds[[x]], ncol = 1), 
          MARGIN = 1,
          A[[x]], 
          `*`)
  })
  names(dLds) <- names(parlist[[lcoef[1]]])
  
  dllds <- lapply(1:length(parlist[[lcoef[1]]]), function (x) {
    sweep(dLds[[x]], 
          MARGIN = 1,
          L, 
          `/`)
  })
  names(dllds) <- names(parlist[[lcoef[1]]])
  
  -cbind(do.call("cbind", dlldb), 
         do.call("cbind", dlldd), 
         do.call("cbind", dllds))
}
