test_that('Check calculation of gradients of log-likelihood w.r.t. parameters per observation.', {
  
  #----------------------------------------------------------------------------
  # Create auxiliary objects
  #----------------------------------------------------------------------------
  
  # Model fit
  #----------
  
  data("utility", package = "aldvmm")
  
  fit <- aldvmm(eq5d ~ age + female | 1,
                data = utility,
                psi = c(0.883, -0.594))
  
  par <- fit$coef
  object <- fit
  
  # Model matrix
  #-------------
  
  X <- model.matrix(object)
  
  # Candidate parameter vectors
  #----------------------------
  
  set.seed(101010101)
  test.par <- rbind(fit$coef,
                    rnorm(length(fit$coef), 0, 1),
                    rnorm(length(fit$coef), 0, 1),
                    rnorm(length(fit$coef), 0, 1),
                    rnorm(length(fit$coef), 0, 1),
                    rnorm(length(fit$coef), 0, 1),
                    rnorm(length(fit$coef), 0, 1),
                    rnorm(length(fit$coef), 0, 1),
                    rnorm(length(fit$coef), 0, 1),
                    rnorm(length(fit$coef), 0, 1))
  test.na <- test.inf <- test.ninf <- test.par
  test.na[matrix(rbinom(nrow(test.par) * ncol(test.par), 1, 0.5) == 1,
                 nrow = nrow(test.par),
                 ncol = ncol(test.par))] <- NA
  test.inf[matrix(rbinom(nrow(test.par) * ncol(test.par), 1, 0.5) == 1,
                  nrow = nrow(test.par),
                  ncol = ncol(test.par))] <- Inf
  test.ninf[matrix(rbinom(nrow(test.par) * ncol(test.par), 1, 0.5) == 1,
                   nrow = nrow(test.par),
                   ncol = ncol(test.par))] <- -Inf
  test.mat <- rbind(test.par, test.na, test.inf, test.ninf)
  rm(test.par, test.na, test.inf, test.ninf)
  
  #----------------------------------------------------------------------------
  # Define test function
  #----------------------------------------------------------------------------
  
  test_sc <- function(par,
                      X,
                      object,
                      tol = 0.01) {
    
    
    out <- aldvmm.sc(par = par,
                     X = X,
                     y = object$pred$y,
                     psi = object$psi,
                     ncmp = object$k,
                     dist = object$dist,
                     lcoef = object$label$lcoef,
                     lcmp  = object$label$lcmp,
                     lcpar = object$label$lcpar,
                     optim.method = object$optim.method)
    
    # Gradient matrix is of right format and dimensions
    #--------------------------------------------------
    
    testthat::expect(is.matrix(out),
                     failure_message = 'Gradient is not a matrix.')
    
    testthat::expect(nrow(out) == nrow(X[[1]]),
                     failure_message = 'Gradient matrix includes wrong number of rows.')
    
    testthat::expect(ncol(out) == length(par),
                     failure_message = 'Gradient matrix includes wrong number of columns.')
    
    # Gradients are numeric and finite
    #---------------------------------
    
    testthat::expect(sum(!is.numeric(out)) == 0,
                     failure_message = 'Gradient matrix includes non-numeric values.')
    
    # Parameters are in same order as in initial values
    #--------------------------------------------------
    
    testthat::expect(any(colnames(out) == names(par)),
                     failure_message = 'Column names not identical to parameter names.')
    
  }
  
  #----------------------------------------------------------------------------
  # Test gradient at different parameter values
  #----------------------------------------------------------------------------
  
  for (i in 1:nrow(test.mat)) {
    test_sc(par = test.mat[i, ],
            X = X,
            object = fit,
            tol = 0.01)
  }
})