test_that("Check estimation of standard errors of fitted values.", {
  
  data(utility)
  
  # Model with valid covariance matrix
  #-----------------------------------
  
  formula <- eq5d ~ age + female | age + female
  
  psi <- c(0.883, -0.594)
  
  fit <- aldvmm(data = utility[1:100, ],
                formula = formula,
                psi = psi,
                ncmp = 2,
                init.method = "zero",
                optim.method = "BFGS",
                se.fit = TRUE)
  
  newdata <- utility[1:5, ]
  newdata[2, 2] <- NA
  yhat <- fit[["pred"]][["yhat"]][1:4]
  
  mm <- aldvmm.mm(data = newdata,
                  formula = formula,
                  ncmp = 2,
                  lcoef = c("beta", "delta"))
  
  invisible({pred.se <- aldvmm.sefit(par = fit[["coef"]],
                                     yhat = yhat,
                                     X = mm,
                                     type = "fit",
                                     formula = fit[["formula"]],
                                     psi = fit[["psi"]],
                                     cv = fit[["cov"]],
                                     mse = fit[["gof"]][["mse"]],
                                     ncmp = fit[["k"]],
                                     dist = fit[["dist"]],
                                     level = fit[["level"]],
                                     lcoef = fit[["label"]][["lcoef"]],
                                     lcpar = fit[["label"]][["lcpar"]],
                                     lcmp = fit[["label"]][["lcmp"]])})
  
  testthat::expect(sum(pred.se[["se.fit"]] <= 0, na.rm = TRUE) == 0,
                   failure_message = "Non-positive standard errors.")
  
  for (i in names(pred.se)) {

    testthat::expect(all(!is.na(pred.se[[i]])),
                     failure_message = "Missing standard errors or limits
                     despite valid covariance matrix.")
    
    testthat::expect(length(pred.se[[i]]) == nrow(mm[[1]]),
                     failure_message = "Length of standard errors or limits 
                     does not match size of design matrix.")
    
    testthat::expect(sum(is.na(pred.se[[i]])) != length(pred.se[[i]]),
                     failure_message = "Only missing standard errors or limits 
                     returned")
    
    testthat::expect(is.numeric(pred.se[[i]]),
                     failure_message = "Non-numeric standard errors or limits")
    
    testthat::expect(all(names(pred.se[[i]]) ==  
                           rownames(newdata)[complete.cases(newdata)]),
                     failure_message = "Missing values not at same index as in 
                   data.")
    
  }
  
  testthat::expect(all(pred.se[["upper.fit"]] <= 1),
                   failure_message = "Upper limits larger than one.")
  testthat::expect(all(pred.se[["lower.fit"]] <= 1),
                   failure_message = "Lower limits larger than one.")
  testthat::expect(all(pred.se[["upper.fit"]] >= min(psi)),
                   failure_message = "Upper limits smaller than minimum in 
                   'psi'.")
  testthat::expect(all(pred.se[["lower.fit"]] >= min(psi)),
                   failure_message = "Lower limits smaller than minimum in 
                   'psi'.")
  
  # Missing values in covariance matrix
  #------------------------------------
  
  cvtmp <- fit$cov
  cvtmp[3, 4] <- NA
  
  testthat::expect_warning({pred.se <- 
    aldvmm.sefit(par = fit[["coef"]],
                 yhat = yhat,
                 X = mm,
                 type = "fit",
                 formula = fit[["formula"]],
                 psi = fit[["psi"]],
                 cv = cvtmp,
                 mse = fit[["gof"]][["mse"]],
                 ncmp = fit[["k"]],
                 dist = fit[["dist"]],
                 level = fit[["level"]],
                 lcoef = fit[["label"]][["lcoef"]],
                 lcpar = fit[["label"]][["lcpar"]],
                 lcmp = fit[["label"]][["lcmp"]])})
  
  for (i in names(pred.se)) {
    
    testthat::expect(is.null(pred.se[[i]]),
                     failure_message = 
                       "No standard errors or limits of the fit should be 
                       returned with missing values in covariance matrix."
    )
    
  }
  
  for (i in 2:length(pred.se)) {
    
    testthat::expect(all(is.na(pred.se[[1]]) == is.na(pred.se[[i]])),
                     failure_message = 
                       'Missing standard errors do not match missing limits.')
                     
  }
  
  # Negative values in diagonal
  #----------------------------
  
  cvtmp <- fit$cov
  cvtmp[3, 3] <- -1
  
  testthat::expect_warning({pred.se <- 
    aldvmm.sefit(par = fit[["coef"]],
                 yhat = yhat,
                 X = mm,
                 type = "fit",
                 formula = fit[["formula"]],
                 psi = fit[["psi"]],
                 cv = cvtmp,
                 mse = fit[["gof"]][["mse"]],
                 ncmp = fit[["k"]],
                 dist = fit[["dist"]],
                 level = fit[["level"]],
                 lcoef = fit[["label"]][["lcoef"]],
                 lcpar = fit[["label"]][["lcpar"]],
                 lcmp = fit[["label"]][["lcmp"]])})
  
  for (i in names(pred.se)) {
    
    testthat::expect(is.null(pred.se[[i]]),
                     failure_message = 
                       "No standard errors or limits of the fit should be 
                       returned with negative diagonals in covariance matrix."
    )
    
  }
})
    