#test if negative log-likelihood of more complex model is smaller than of less complex model.
test_that("Check covariance function.", {

  # Fit models
  #-----------
  
  data(utility)
  
  suppressWarnings({
    fit1 <- aldvmm(formula = eq5d ~ age | 1,
                   data = utility,
                   psi = c(-0.594, 0.883))
    
    fit2 <- aldvmm(eq5d ~ age | female,
                   data = utility,
                   psi = c(-0.594, 0.884))
  })
  
  gof1 <- aldvmm.gof(par = fit1$coef,
                     ll = -fit1$gof$ll,
                     res = fit1$pred$res)
  
  gof2 <- aldvmm.gof(par = fit2$coef,
                     ll = -fit2$gof$ll,
                     res = fit2$pred$res)
  
  # Correct format
  #---------------
  
  testthat::expect(is.numeric(unlist(gof1)),
                   failure_message = 
                     "Goodness of fit measures include non-numeric elements."
  )
  
  testthat::expect(all(unlist(gof1)[c("mse", "mae")] > 0),
                   failure_message = 
                     "MSE and MAE are not positive."
  )
  
  # Correct statistics
  #-------------------
  
  testthat::expect(gof1$gof$aic == 2 * length(fit1$coef) + 2 * gof1$gof$ll,
                   failure_message = 
                     "AIC not correctly calculated."
  )
  
  testthat::expect(gof1$gof$bic == length(fit1$coef) * log(nrow(utility)) + 
                     2 * gof1$gof$ll,
                   failure_message = 
                     "BIC not correctly calculated."
  )
  
  # Compare models
  #---------------
  
  testthat::expect(gof1$gof$ll > gof2$gof$ll,
                   failure_message = 
                     "More complex model does not show larger log-likelihood."
  )

  # Warnings
  #---------
  
  testthat::expect_warning(aldvmm.gof(par = fit2$coef,
                                      ll = -fit2$gof$ll,
                                      res = c(NA, 1))
  )
  
  
  })
