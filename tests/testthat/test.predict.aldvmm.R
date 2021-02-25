test_that('Check prediction S3 method.', {
  
  data(utility)
  utility[1, 3] <- NA
  utility[34, 1] <- NA
  utility[77, 2] <- NA
  
  # Warning for missing or invalid values of covariance matrix
  testthat::expect_warning(
    fit <- aldvmm(eq5d ~ age | female,
                  data = utility,
                  psi = c(-0.594, 0.883))
  )
  
  # Message for incomplete values in data
  testthat::expect_message(
    suppressWarnings(fit <- aldvmm(eq5d ~ age | female,
                                   data = utility,
                                   psi = c(-0.594, 0.883)))
  )
  
  pred <- predict(fit,
                  newdata = utility)
  testthat::expect(all(stats::complete.cases(utility)==!is.na(pred$yhat)),
                   failure_message = 
                     'Missing predictions not same position as missing data.'
  )
})