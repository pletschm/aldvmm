test_that("Check estimation of standard errors of fitted values.", {
  
  data(utility)
  
  # Model with valid covariance matrix
  #-----------------------------------
  
  formula <- eq5d ~ age + female | age + female
  
  fit <- aldvmm(data = utility[1:100, ],
                formula = formula,
                psi = c(0.883, -0.594),
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
  
  testthat::expect(length(pred.se[["se.fit"]]) == nrow(mm[[1]]),
                   failure_message = "Length of standard errors does not match
                   size of design matrix."
  )
  testthat::expect(sum(is.na(pred.se[["se.fit"]])) != length(pred.se[["se.fit"]]),
                   failure_message = "Only missing standard errors returned"
  )
  testthat::expect(is.numeric(pred.se[["se.fit"]]),
                   failure_message = "Non-numeric output"
  )
  testthat::expect(sum(pred.se[["se.fit"]] <= 0, na.rm = TRUE) == 0,
                   failure_message = "Non-positive standard errors."
  )
  testthat::expect(sum(names(pred.se[["se.fit"]]) != 
                         rownames(newdata)[complete.cases(newdata)]) == 0,
                   failure_message = "Missing values not at same index as in 
                   data."
  )
  
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
  
  testthat::expect(is.null(pred.se[["se.fit"]]),
                   failure_message = 
                     "No standard errors of the fit should be returned with
                   missing values in covariance matrix."
  )
  
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
  
  testthat::expect(is.null(pred.se[["se.fit"]]),
                   failure_message = 
                     "No standard errors of the fit should be returned with
                   negative diagonals in covariance matrix."
  )
  
})
