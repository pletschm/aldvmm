test_that("Check covariance function.", {
  
  mm <- matrix(data     = c(rep(1, 4), runif(n = 8)), 
               nrow     = 4, 
               ncol     = 3,
               dimnames = list(NULL,
                               c("(Intercept)", "ind1", "ind2")))
  
  y <- runif(n = 4)
  
  init <- rep(1, ncol(mm))
  
  optim.method <- "BFGS"
  
  test <- function(par, 
                   X = X, 
                   y = y,
                   psi,
                   ncmp,
                   dist,
                   lcoef,
                   lcmp,
                   lcpar,
                   optim.method) {
    sum(stats::pnorm((y - X%*%par)))
  }
  
  cov <- aldvmm.cv(ll = test,
                   par = init,
                   X = mm,
                   y = y,
                   psi = c(0.883, -0.594),
                   ncmp = 2,
                   dist = "normal",
                   lcoef = c("beta", "delta"),
                   lcmp = "Grp",
                   lcpar = c("sigma"),
                   optim.method = optim.method)
  
  testthat::expect(sum(unlist(lapply(cov, function(c) !is.numeric(c))))==0,
                   failure_message = "Some elements of list are not numeric."
  )
  testthat::expect(sum(unlist(lapply(cov, function(c) !is.finite(c))))==0,
                   failure_message = "Some elements of list are not finite."
  )
  testthat::expect(is.matrix(cov[["cv"]]),
                   failure_message = "Covariance matrix is not matrix."
  )
  testthat::expect(sum(diag(cov[["cv"]])<0)==0,
                   failure_message = 
                     "Covariance matrix includes negative values in diagonal."
  )
  wlength <- unlist(lapply(cov, function(c) sum(dim(c)!=length(init))))
  testthat::expect(sum(wlength)==0,
                   failure_message = 
                     "Some elements of output are of wrong length."
  )
  rm(wlength)
  testthat::expect(sum(init < cov[["lower"]]) + sum(init > cov[["upper"]])==0,
                   failure_message = "Estimates outside confidence bands."
  )
  testthat::expect(sum(cov[["se"]]!=sqrt(diag(cov[["cv"]])))==0,
                   failure_message = 
                     "Standard errors are not square root of diagnonal CV."
  )
  
})
