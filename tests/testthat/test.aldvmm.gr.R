test_that('Check calculation of gradients of log-likelihood.', {
  
  #----------------------------------------------------------------------------
  # Create auxiliary objects
  #----------------------------------------------------------------------------
  
  # Model fit
  #----------
  
  data("utility", package = "aldvmm")
  
  fit <- aldvmm(eq5d ~ age + female | 1,
                data = utility,
                psi = c(0.883, -0.594))
  
  # Model matrix
  #-------------
  
  X <- model.matrix(fit)
  
  # List of reference values from numderiv::grad
  #---------------------------------------------
  
  reflist <- list()
  set.seed(101010101)
  i <- 1
  while(i <= 20){
    
    tryCatch({
      
      reflist[["par"]][[i]] <- rnorm(length(fit$coef), 0, 1)
      names(reflist[["par"]][[i]]) <- names(fit$coef)
      
      reflist[["grad"]][[i]] <- numDeriv::grad(func = function(z) {
        aldvmm.ll(par = z,
                  X = X,
                  y = fit$pred$y,
                  psi = fit$psi,
                  ncmp = fit$k,
                  dist = fit$dist,
                  lcoef = fit$label$lcoef,
                  lcmp  = fit$label$lcmp,
                  lcpar = fit$label$lcpar,
                  optim.method = fit$optim.method)
      },
      x = reflist[["par"]][[i]])
      
      i <- i + 1
      
    }, error = function (e) {
      
    })
    
  }
  
  #----------------------------------------------------------------------------
  # Define test function
  #----------------------------------------------------------------------------
  
  test_gr <- function(par,
                      num.grad,
                      X,
                      object,
                      reflist,
                      tol = 0.01) {
    
    out.gr <- aldvmm.gr(par = par,
                        X = X,
                        y = object$pred$y,
                        psi = object$psi,
                        ncmp = object$k,
                        dist = object$dist,
                        lcoef = object$label$lcoef,
                        lcmp  = object$label$lcmp,
                        lcpar = object$label$lcpar,
                        optim.method = "L-BFGS-B")
    
    out.sc <- aldvmm.sc(par = par,
                        X = X,
                        y = object$pred$y,
                        psi = object$psi,
                        ncmp = object$k,
                        dist = object$dist,
                        lcoef = object$label$lcoef,
                        lcmp  = object$label$lcmp,
                        lcpar = object$label$lcpar,
                        optim.method = "L-BFGS-B")
    
    # Set optim-method to L-BFGS-B for converting infinite values to zero
    
    # Output is colSums of gradient matrix
    #------------------------------------
    
    testthat::expect(all(out.gr == colSums(out.sc)),
                     failure_message = 'Gradient vector is not sum of gradient matrix.')
    
    # Numeric and finite values
    #--------------------------
    
    testthat::expect(all(is.finite(out.gr)),
                     failure_message = 'Gradient vector includes infinite values.')
    
    # Small differences from numDeriv:Jacobian results
    #-------------------------------------------------
    
    testthat::expect(all(abs(out.gr - num.grad) < abs(tol * out.gr) | abs(out.gr - num.grad) < tol),
                     failure_message = paste0('Analytical and numerical gradients differ by more than ',
                                              tol * 100, '%, or more than ', tol))
    
  }
  
  #----------------------------------------------------------------------------
  # Test gradient at different parameter values
  #----------------------------------------------------------------------------
  
  for (i in 1:length(reflist[["par"]])) {
    test_gr(par = reflist[["par"]][[i]],
            num.grad = reflist[["grad"]][[i]],
            X = X,
            object = fit,
            tol = 0.01)
  }

  #----------------------------------------------------------------------------
  # Test behavior in case of infeasible parameter values
  #----------------------------------------------------------------------------
  
  # List of infeasible values from numderiv::grad
  #----------------------------------------------
  
  errlist <- list()

  set.seed(101010101)
  i <- 1
  while(i <= 20){

    par <- rnorm(length(fit$coef), 0, 1)
    names(par) <- names(fit$coef)

    tryCatch({
      grad <- numDeriv::grad(func = function(z) {
        aldvmm.ll(par = z,
                  X = X,
                  y = fit$pred$y,
                  psi = fit$psi,
                  ncmp = fit$k,
                  dist = fit$dist,
                  lcoef = fit$label$lcoef,
                  lcmp  = fit$label$lcmp,
                  lcpar = fit$label$lcpar,
                  optim.method = fit$optim.method)
      },
      x = par)

    }, error = function (e) {
      errlist[["par"]][[i]] <<- par
      i <<- i + 1
    })

  }

  # Run tests
  #----------

  for (i in 1:length(errlist[["par"]])) {

    out.gr <- aldvmm.gr(par = errlist[["par"]][[i]],
                        X = X,
                        y = fit$pred$y,
                        psi = fit$psi,
                        ncmp = fit$k,
                        dist = fit$dist,
                        lcoef = fit$label$lcoef,
                        lcmp  = fit$label$lcmp,
                        lcpar = fit$label$lcpar,
                        optim.method = "L-BFGS-B")

    testthat::expect(sum(!is.finite(out.gr)) == 0,
                     failure_message = 'Gradient matrix includes infinite values.')

  }

})