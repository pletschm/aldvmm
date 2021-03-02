test_that('Check likelihood function.', {
  
  mm <- list('beta' = matrix(data      = c(rep(1, 4), runif(n = 8)), 
                             nrow     = 4, 
                             ncol     = 3,
                             dimnames = list(NULL,
                                             c('(Intercept)', 
                                               'ind1', 
                                               'ind2'))),
             'delta' = matrix(data     = c(rep(1, 4), runif(n = 8)), 
                              nrow     = 4, 
                              ncol     = 3,
                              dimnames = list(NULL,
                                              c('(Intercept)', 
                                                'ind2', 
                                                'ind3'))))
  
  y <- runif(n = 4)
  
  names <- c("Grp1_beta_(Intercept)", "Grp1_beta_ind1", "Grp1_beta_ind2", 
             "Grp2_beta_(Intercept)", "Grp2_beta_ind1", "Grp2_beta_ind2", 
             "Grp1_delta_(Intercept)", "Grp1_delta_ind2", "Grp1_delta_ind3", 
             "Grp1_sigma", "Grp2_sigma")
  
  init <- rep(0, length(names))
  names(init) <- names
  
  optim.method <- "BFGS"
  
  ll <- aldvmm.ll(par = init,
                  X = mm,
                  y = y,
                  psi = c(0.883, -0.594),
                  dist = 'normal',
                  ncmp = 2,
                  lcoef = c('beta', 'delta'),
                  lcmp = 'Grp',
                  lcpar = c('sigma'),
                  optim.method = optim.method)
  
  testthat::expect(is.finite(ll),
         failure_message = 'Infinite likelihood.')
  testthat::expect(is.numeric(ll),
         failure_message = 'Infinite likelihood.')

})
