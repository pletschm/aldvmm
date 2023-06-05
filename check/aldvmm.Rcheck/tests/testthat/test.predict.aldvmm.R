test_that('Check prediction S3 method.', {
  
  # Object not coercible to data.frame
  #-----------------------------------
  
  data(utility)
  suppressWarnings({
    fit <- aldvmm(eq5d ~ age | female,
                  data = utility,
                  psi = c(-0.594, 0.883))
  })
  
  testthat::expect_error(predict(fit,
                                 newdata = plot(),
                                 se.fit = TRUE))
  
  # Predictions for different data
  #-------------------------------
  
  data(utility)
  suppressWarnings({
    fit <- aldvmm(eq5d ~ female | 1,
                  data = utility,
                  psi = c(-0.594, 0.883))
  })
  
  testthat::expect_warning(predict(fit,
                                   newdata = utility[utility$female == 1, ],
                                   se.fit = TRUE),
                           NA)
  
  
  pred <- predict(fit,
                  newdata = utility[utility$female == 1, ],
                  se.fit = TRUE)
                  
  
  for (i in 1:length(pred)) {
    
    testthat::expect(all(rownames(utility[utility$female == 1, ]) == names(pred[[i]])),
                     failure_message = 
                       'Position of standard errors do not match position in data.')
    
  }
  
  # Prediction in data with incomplete rows and missing covariance matrix
  #----------------------------------------------------------------------
  
  data(utility)
  utility[1, 3] <- NA
  utility[34, 1] <- NA
  
  suppressWarnings({
    suppressMessages({
      fit <- aldvmm(eq5d ~ age | female,
                    data = utility,
                    psi = c(-0.594, 0.883))
    })
  })
  
  pred <- predict(fit,
                  newdata = utility,
                  se.fit = TRUE)
  
  testthat::expect(all(stats::complete.cases(utility) == !is.na(pred$yhat)),
                   failure_message = 
                     'Missing predictions not same position as missing data.')
  
  for (i in 2:length(pred)) {
    
    testthat::expect(all(is.na(pred[[1]]) == is.na(pred[[i]])),
                     failure_message = 
                       'Missing standard errors and limits not same position as 
                     missing data.')
    
  }
  
})