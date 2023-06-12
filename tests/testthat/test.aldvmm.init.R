test_that('Check generation of initial values.', {
  
  # Function of tests
  #------------------
  
  test_init <- function(init,
                        names) {
    testthat::expect_named(init[["est"]])
    testthat::expect(length(init[["est"]]) == length(init[["lo"]]) & 
                       length(init[["est"]]) == length(init[["hi"]]),
                     failure_message = 
                       'Limits are not of same length as initial values.'
    )
    testthat::expect(length(init[["est"]]) == length(names),
                     failure_message = 'Initial values are of unexpected length.'
    )
    testthat::expect(sum(names(init[["est"]]) != names) == 0,
                     failure_message = 'Initial values have unexpected names.'
    )
    testthat::expect(sum(!is.finite(init[["est"]])) == 0,
                     failure_message = 
                       'Initial values include non-finite values.'
    )
    testthat::expect(is.numeric(init[["est"]]),
                     failure_message = 
                       'Initial values include non-finite values.'
    )
    testthat::expect(is.numeric(init[["lo"]]) & is.numeric(init[["hi"]]),
                     failure_message = 'Limits are not numeric.'
    )
    testthat::expect(sum(init[["est"]] > init[["hi"]]) + 
                       sum(init[["est"]] < init[["lo"]]) == 0,
                     failure_message = 
                       'Initial values outside limits are generated.'
    )
  }
  
  # Create data
  #------------
  
  nr <- 100
  nc <- 3
  
  mm <- list('beta' = matrix(data     = c(rep(1, nr), runif(n = (nc - 1)*nr)), 
                             nrow     = nr, 
                             ncol     = nc,
                             dimnames = list(NULL,
                                             c('(Intercept)', 
                                               'ind1', 
                                               'ind2'))),
             'delta' = matrix(data     = c(rep(1, nr), runif(n = (nc - 1)*nr)), 
                              nrow     = nr, 
                              ncol     = nc,
                              dimnames = list(NULL,
                                              c('(Intercept)', 
                                                'ind2', 
                                                'ind3'))))
  
  y <- runif(n = nr)
  
  # Parameter names for single- (1) and multi-component (2) models
  #---------------------------------------------------------------
  
  names <- list()
  
  names[[1]] <- c("Grp1_beta_(Intercept)", "Grp1_beta_ind1", "Grp1_beta_ind2", 
                  "Grp1_sigma")
  
  names[[2]] <- c("Grp1_beta_(Intercept)", "Grp1_beta_ind1", "Grp1_beta_ind2", 
                  "Grp2_beta_(Intercept)", "Grp2_beta_ind1", "Grp2_beta_ind2", 
                  "Grp1_delta_(Intercept)", "Grp1_delta_ind2", "Grp1_delta_ind3", 
                  "Grp1_sigma", "Grp2_sigma")
  
  
  # Zero initial values
  #--------------------
  
  for (i in 1:2) { # Test single- and multi-component models
    for (j in c(TRUE, FALSE)) { # Test gradient method
      init <- aldvmm.init(X = mm,
                          y = y,
                          dist = 'normal',
                          psi = c(0.883, -0.594),
                          ncmp = i,
                          lcoef = c('beta', 'delta'),
                          lcmp = 'Grp',
                          lcpar = c('sigma'),
                          init.method = 'zero',
                          init.est = NULL,
                          init.lo = NULL,
                          init.hi = NULL,
                          optim.method = 'Nelder-Mead',
                          optim.control = list(trace = FALSE),
                          num.grad = j)
      
      testthat::expect(sum(init[["est"]]) == 0,
                       failure_message = 
                         'Non-zero initial values with init.method=="zero".'
      )
      
      test_init(init = init,
                names = names[[i]])
    }  
  }
  
  # Random initial values
  #----------------------
  
  for (i in 1:2) { # Test single- and multi-component models
    for (j in c(TRUE, FALSE)) { # Test gradient method
      init <- aldvmm.init(X = mm,
                          y = y,
                          dist = 'normal',
                          psi = c(0.883, -0.594),
                          ncmp = i,
                          lcoef = c('beta', 'delta'),
                          lcmp = 'Grp',
                          lcpar = c('sigma'),
                          init.method = 'random',
                          init.est = NULL,
                          init.lo = NULL,
                          init.hi = NULL,
                          optim.method = 'Nelder-Mead',
                          optim.control = list(trace = FALSE),
                          numm.grad = j)
      
      testthat::expect(length(unique(init[["est"]])) == length(init[["est"]]),
                       failure_message = 
                         'Random initial values include repeated values.'
      )
      
      test_init(init = init,
                names = names[[i]])
    }
  }
  
  # Initial values from constant-only model
  #----------------------------------------
  
  for (i in 1:2) { # Test single- and multi-component models
    for (j in c(TRUE, FALSE)) { # Test gradient method
      init <- aldvmm.init(X = mm,
                          y = y,
                          dist = 'normal',
                          psi = c(0.883, -0.594),
                          ncmp = i,
                          lcoef = c('beta', 'delta'),
                          lcmp = 'Grp',
                          lcpar = c('sigma'),
                          init.method = 'constant',
                          init.est = NULL,
                          init.lo = NULL,
                          init.hi = NULL,
                          optim.method = 'Nelder-Mead',
                          optim.control = list(trace = FALSE),
                          num.grad = j)
      
      testthat::expect(sum(init[["est"]][!grepl('(Intercept)|sigma', 
                                                names(init[["est"]]))]) == 0,
                       failure_message = 
                         'Modeled parameters include non-zero initial values.'
      )
      test_init(init = init,
                names = names[[i]])
      
    }
  }
  
  # Initial values from simulated annealing (SANN) model
  #-----------------------------------------------------
  
  for (i in 1:2) { # Test single- and multi-component models
    for (j in c(TRUE, FALSE)) { # Test gradient method
      init <- aldvmm.init(X = mm,
                          y = y,
                          dist = 'normal',
                          psi = c(0.883, -0.594),
                          ncmp = i,
                          lcoef = c('beta', 'delta'),
                          lcmp = 'Grp',
                          lcpar = c('sigma'),
                          init.method = 'sann',
                          init.est = NULL,
                          init.lo = NULL,
                          init.hi = NULL,
                          optim.method = 'Nelder-Mead',
                          optim.control = list(trace = FALSE),
                          num.grad = j)
      
      test_init(init = init,
                names = names[[i]])
      
    }
  }
  
  # User-defined initial values
  #----------------------------
  
  for (i in 1:2) { # Test single- and multi-component models
    est <- runif(length(names[[i]]))
      
      init <- aldvmm.init(X = mm,
                          y = y,
                          dist = 'normal',
                          psi = c(0.883, -0.594),
                          ncmp = i,
                          lcoef = c('beta', 'delta'),
                          lcmp = 'Grp',
                          lcpar = c('sigma'),
                          init.method = 'sann',
                          init.est = est,
                          init.lo = est/10,
                          init.hi = est*10,
                          optim.method = 'Nelder-Mead',
                          optim.control = list(trace = FALSE),
                          num.grad = FALSE)
      
      testthat::expect(sum(init[["est"]] != est) == 0,
                       failure_message = 
                         'Initial values are not equal to user input.'
      )
      testthat::expect(sum(init[["lo"]] != est/10) + 
                         sum(init[["hi"]] != est*10) == 0,
                       failure_message = 'Limits are not equal to user input.'
      )
      
      test_init(init = init,
                names = names[[i]])
      
    }
  rm(mm, y, est, names, init, nr, nc, i, j, test_init)
  
})
