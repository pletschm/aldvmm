test_that("Check input value checks.", {
  
  data(utility)
  
  aux <- function(formula = eq5d ~ female | age, 
                  data = utility,
                  psi = c(-0.594, 0.883),
                  ncmp = 2,
                  dist = "normal",
                  lcoef = c("beta", "delta"),
                  lcpar = c("lnsigma"),
                  lcmp = c("Comp1"),
                  init.method = "zero", 
                  optim.method = NULL,
                  optim.grad = TRUE,
                  init.est = NULL,
                  init.lo = NULL,
                  init.hi = NULL,
                  optim.control = list(trace = FALSE),
                  se.fit = FALSE,
                  level = 0.95) {
    
    aldvmm.check(formula = formula, 
                 data = data, 
                 psi = psi,
                 ncmp = ncmp,
                 dist = dist,
                 lcoef = lcoef,
                 lcpar = lcpar,
                 lcmp = lcmp,
                 init.method = init.method, 
                 optim.method = optim.method,
                 optim.grad = optim.grad,
                 init.est = init.est,
                 init.lo = init.lo,
                 init.hi = init.hi,
                 optim.control = optim.control,
                 se.fit = se.fit,
                 level = level)
    
  }
  
  # Check format of input values
  #-----------------------------
  
  testthat::expect_error(aux(data = list(NA)))
  
  testdf <- utility
  names(testdf) <- NULL
  testthat::expect_error(aux(data = testdf))
  rm(testdf)
  
  testthat::expect_error(aux(formula = "dfg"))
  testthat::expect_error(aux(psi = list(-0.594, 0.883)))
  testthat::expect_error(aux(psi = c("-0.594", "0.883")))
  testthat::expect_error(aux(psi = c(0.5, 0.5)))
  testthat::expect_error(aux(psi = c(0.3, 0.8, 0.6)))
  testthat::expect_error(aux(psi = c(0.3, 1.00000001)))
  testthat::expect_error(aux(ncmp = 3.3))
  testthat::expect_error(aux(ncmp = 0))
  testthat::expect_error(aux(ncmp = -1))
  testthat::expect_error(aux(dist = "t"))
  testthat::expect_error(aux(init.method = "cons"))
  testthat::expect_error(aux(optim.method = "nlm"))
  testthat::expect_error(aux(init.est = data.frame(init.est = c(0, 0, 0))))
  testthat::expect_error(aux(init.est = data.frame(init.lo = c(0, 0, 0))))
  testthat::expect_error(aux(init.est = data.frame(init.hi = c(0, 0, 0))))
  testthat::expect_error(aux(optim.control = data.frame(trace = FALSE)))
  testthat::expect_error(aux(optim.grad = "TRUE"))
  testthat::expect_error(aux(se.fit = "TRUE"))
  testthat::expect_error(aux(lcpar = c("lnsigma", "lambda")))
  testthat::expect_error(aux(lcoef = c("beta", "delta", "epsilon")))
  testthat::expect_error(aux(lcmp = c("Comp1", "Comp2")))
  testthat::expect_error(aux(level = c(0.025, 0.975)))
  testthat::expect_error(aux(level = "0.95"))
  testthat::expect_error(aux(level = 0))
  testthat::expect_error(aux(level = 1))
  
  # Count rows with missing values
  #-------------------------------
  
  testdf <- utility
  testdf[1, 1] <- NA
  
  testthat::expect_message(aux(data = testdf))
  rm(testdf)
  
  # Check if all variables in formula exist in data
  #------------------------------------------------
  
  testthat::expect_error(aux(formula = eq5d ~ female | age + country))
  
  # Check if user-defined initial values are the right length.
  #-----------------------------------------------------------
  
  testthat::expect_error(aux(init.est = rep(0, 1)))
  
  # Only one component but pipe separator in formula
  #-------------------------------------------------

  testthat::expect_message(aux(formula = eq5d ~ female | age,
                               ncmp = 1))
  
  # Check if model includes constants when init.method is set to "constant".
  #-------------------------------------------------------------------------
  
  testthat::expect_error(aux(formula = eq5d ~ -1 + female | age,
                             init.method = "constant"))
  testthat::expect_error(aux(formula = eq5d ~ female | -1 + age,
                             init.method = "constant"))
  
  # Ensure the term "(Intercept)" is not used in column names of data
  #--------------------------------------------------------------------
  
  testdf <- utility
  names(testdf) <- paste0("hide", "(Intercept)", names(testdf))
  
  testthat::expect_error(aux(formula = `hide(Intercept)eq5d` ~ 
                               `hide(Intercept)female` | `hide(Intercept)age`,
                             data = testdf))
  
  # Check if the data includes outcome values outside limits
  #---------------------------------------------------------
  
  formula <- eq5d ~ female | age
  
  outdat <- utility[, as.character(formula)[[2]]]
  
  # Lower bound larger than observed minimum
  minobs <- min(outdat, na.rm = TRUE) + 0.0001
  gapobs <- max(outdat[outdat < 1], na.rm = TRUE)
  
  testthat::expect_error(aux(psi = c(minobs, gapobs)))
  
  # Upper bound smaller than observed minimum
  minobs <- min(outdat, na.rm = TRUE)
  gapobs <- max(outdat[outdat < 1], na.rm = TRUE) - 0.0001
  
  testthat::expect_error(aux(psi = c(minobs, gapobs)))
  
  # Values larger than 1
  tmpdat <- utility
  tmpdat$eq5d <- tmpdat$eq5d + 0.00001
  testthat::expect_error(aux(data = tmpdat))
  rm(tmpdat)
})