test_that('Check prediction function.', {
  
  # Function of tests
  #------------------
  
  test_pred <- function(pred,
                        testdat,
                        psi) {
    testthat::expect(sum(unlist(lapply(pred, 
                                       function(x) sum(!is.numeric(x)) ))) == 0,
                     failure_message = 'Non-numeric elements in predictions.')
    testthat::expect(all(!is.na(pred[["yhat"]])),
                     failure_message = 'Only missing predicted outcomes.')
    testthat::expect(sum(!is.na(pred[["yhat"]])) == sum(complete.cases(testdat)),
                     failure_message = 
                       'Different number of miss. in data & predicted outcomes.')
    testthat::expect(sum(unlist(lapply(pred, 
                                       function(x) (Inf %in% x) | 
                                         (-Inf %in% x)))) == 0,
                     failure_message = 
                       'Predicted outcomes include non-finite values.')
    testthat::expect(sum(pred[["prob"]]) == 1,
                     failure_message = 
                       "Probabilities of group membership do not sum to 1.")
    
    testthat::expect(all(pred[["yhat"]] <= 1),
                     failure_message = "Predictions larger than one.")
    testthat::expect(all(pred[["yhat"]] >= min(psi)),
                     failure_message = "Predictions smaller than minimum in 
                   'psi'.")
  }
  
  # Two-component model
  #--------------------
  
  testdat <- as.data.frame(matrix(data     = runif(n = 12), 
                                  nrow     = 4, 
                                  ncol     = 4,
                                  dimnames = list(NULL,
                                                  c('dep', 
                                                    'ind1', 
                                                    'ind2', 
                                                    'ind3'))))
  testdat[2, 4] <- NA
  
  ncmp <- 2
  
  names <- c("Grp1_beta_(Intercept)", "Grp1_beta_ind1", "Grp1_beta_ind2", 
             "Grp2_beta_(Intercept)", "Grp2_beta_ind1", "Grp2_beta_ind2", 
             "Grp1_delta_(Intercept)", "Grp1_delta_ind2", "Grp1_delta_ind3", 
             "Grp1_delta_ind2:ind3", "Grp1_sigma", "Grp2_sigma")
  
  mm <- list(beta = rbind('1' = c(1, 0.05933173, 0.4921575),
                          '3' = c(1, 0.059,      0.49),
                          '4' = c(1, 0.05775388, 0.06194975)),
             delta = rbind('1' = c(1, 0.4921575,  0.9556145, 0.4703129),
                           '3' = c(1, 0.5,        0.9,       0.5),
                           '4' = c(1, 0.06194975, 0.1646918, 0.01020262)))  
  
  y <- runif(n = nrow(mm[[1]]))
  
  init <- rep(0, length(names))
  names(init) <- names
  
  psi <- c(0.883, -0.594)
  
  
  
  pred <- aldvmm.pred(par = init,
                      X = mm,
                      y = y,
                      psi = psi,
                      ncmp = 2,
                      dist = 'normal',
                      lcoef = c('beta', 'delta'),
                      lcmp = 'Grp',
                      lcpar = c('sigma'))
  
  
  test_pred(pred = pred,
            testdat = testdat,
            psi = psi)
  
  # Warnings for missing fitted outcomes in two-component model
  #------------------------------------------------------------
  
  inittmp <- rep(-Inf, length(init))
  names(inittmp) <- names(init)
  
  testthat::expect_warning(aldvmm.pred(par = inittmp,
                                       X = mm,
                                       y = y,
                                       psi = psi,
                                       ncmp = 2,
                                       dist = 'normal',
                                       lcoef = c('beta', 'delta'),
                                       lcmp = 'Grp',
                                       lcpar = c('sigma')))
  
  # Warnings for missing predicted probabilities in two-component model
  #--------------------------------------------------------------------
  
  inittmp <- init
  inittmp[grepl("delta",  names(inittmp))] <- Inf
  
  w <- testthat::capture_warnings(  aldvmm.pred(par = inittmp,
                                 X = mm,
                                 y = y,
                                 psi = psi,
                                 ncmp = 2,
                                 dist = 'normal',
                                 lcoef = c('beta', 'delta'),
                                 lcmp = 'Grp',
                                 lcpar = c('sigma'))
  )
  
  testthat::expect_match(w, "fitted probabilities of component membership include missing", all = FALSE)
  testthat::expect_match(w, "fitted values include missing values", all = FALSE)
  
  # Single-component model
  #-----------------------
  
  ncmp <- 1
  
  names <- c("Grp1_beta_(Intercept)", "Grp1_beta_ind1", "Grp1_beta_ind2", 
             "Grp2_beta_(Intercept)", "Grp2_beta_ind1", "Grp2_beta_ind2", 
             "Grp1_delta_(Intercept)", "Grp1_delta_ind2", "Grp1_delta_ind3", 
             "Grp1_delta_ind2:ind3", "Grp1_sigma", "Grp2_sigma")
  
  mm <- list(beta = rbind('1' = c(1, 0.05933173, 0.4921575),
                          '3' = c(1, 0.059,      0.49),
                          '4' = c(1, 0.05775388, 0.06194975)),
             delta = rbind('1' = c(1, 0.4921575,  0.9556145, 0.4703129),
                           '3' = c(1, 0.5,        0.9,       0.5),
                           '4' = c(1, 0.06194975, 0.1646918, 0.01020262)))  
  
  y <- runif(n = nrow(mm[[1]]))
  
  init <- rep(0, length(names))
  names(init) <- names
  
  psi <- c(0.883, -0.594)
  
  
  
  pred <- aldvmm.pred(par = init,
                      X = mm,
                      y = y,
                      psi = psi,
                      ncmp = 2,
                      dist = 'normal',
                      lcoef = c('beta', 'delta'),
                      lcmp = 'Grp',
                      lcpar = c('sigma'))
  
  test_pred(pred = pred,
            testdat = testdat,
            psi = psi)
  

})
