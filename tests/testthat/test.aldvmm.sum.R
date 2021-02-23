test_that('Check generation of summary table.', {
  
  mm <- list(beta = data.frame(a = rep(1, 4), 
                               b = rep(2, 4),
                               c = rep(3, 4)),
             delta = data.frame(b = rep(4, 4), 
                                c = rep(5, 4),
                                d = rep(6, 4)))
  
  names <- c("Grp1_beta_a", "Grp1_beta_b", "Grp1_beta_c", "Grp2_beta_a", 
             "Grp2_beta_b", "Grp2_beta_c", "Grp1_delta_b", "Grp1_delta_c", 
             "Grp1_delta_d", "Grp1_sigma", "Grp2_sigma")
  
  est <- seq_along(names)
  est[c(length(est), length(est) - 1)] <- log(est[c(length(est), 
                                                    length(est) - 1)])
  
  se <- 1*length(names) + seq_along(names)
  se[c(length(se), length(se) - 1)] <- log(se[c(length(se), 
                                                length(se) - 1)])
  
  z <- 2*length(names) + seq_along(names)
  z[c(length(z), length(z) - 1)] <- log(z[c(length(z), 
                                            length(z) - 1)])
  
  p <- 3*length(names) + seq_along(names)
  p[c(length(p), length(p) - 1)] <- log(p[c(length(p), 
                                            length(p) - 1)])
  
  lower <- 4*length(names) + seq_along(names)
  lower[c(length(lower), length(lower) - 1)]<-log(lower[c(length(lower), 
                                                          length(lower) - 1)])
  
  upper <- 5*length(names) + seq_along(names)
  upper[c(length(upper), length(upper) - 1)]<-log(upper[c(length(upper), 
                                                          length(upper) - 1)])
  
  names(est)   <- names
  names(se)    <- names
  names(z)     <- names
  names(p)     <- names
  names(lower) <- names
  names(upper) <- names
  
  
  regtab <- aldvmm.sum(est = est,
                       se = se,
                       z = z,
                       p = p,
                       lower = lower,
                       upper = upper,
                       n = nrow(mm[[1]]),
                       value = 999999999,
                       aic = 888888888,
                       bic = 777777777,
                       lcoef = c('beta', 'delta'),
                       lcmp = 'Grp',
                       lcpar = c('sigma'),
                       lvar = lapply(mm, function(x) colnames(x)),
                       ncmp = 2)
  
  test <- regtab[-(1:2), -(1:2)]
  test <- test[-nrow(test), ]
  test <- test[!apply(test, 1, function(r) any(r %in% c("") | 
                                                 grepl('=', r) | 
                                                 grepl('-', r))), ]
  test <- as.numeric(unlist(test))
  test <- as.character(format(round(test, 4), 
                              nsmall = 4,
                              trim = TRUE))
  test <- test[order(test)]
  
  ref <- as.character(format(round(c(est, se, z, p, lower, upper), 4), 
                             nsmall = 4,
                             trim = TRUE))
  ref <- ref[order(ref)]
  
  testthat::expect(sum(test!=ref)==0,
                   failure_message = 
                     'Summary table does not include input data.'
  )
  testthat::expect(is.data.frame(regtab),
                   failure_message = 
                     'Summary table is not data.frame object.'
  )
  nochar <- unlist(lapply(regtab, function(x) !is.character(x)))
  testthat::expect(sum(nochar)==0,
                   failure_message = 'Summary table is not character table.'
  )
  rm(nochar)
  testthat::expect(sum(is.na(regtab))==0,
                   failure_message = 'Summary table includes missing values.'
  )
  
})
