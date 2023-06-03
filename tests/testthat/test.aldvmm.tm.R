test_that('Check creation of model terms objects', {
  
  # Define tests in function
  #-------------------------
  
  test_tm <- function(tm,
                      ncmp) {
    
    testthat::expect_named(tm)
    testthat::expect(length(tm) == min(3, ncmp + 1),
                     failure_message = 
                       'The list of terms is the wrong length.'
    )
    testthat::expect(is.list(tm), 
                     failure_message = 
                       'aldvmm.tm output is not list.'
    )
    
    testthat::expect(if (ncmp > 1) {length(tm) == 3} else {length(tm) == 2}, 
                     failure_message = 
                       'aldvmm.tm output is of wrong length.'
    )
    
    testthat::expect(all(unlist(lapply(tm, 
                                function(x) 
                                  any(class(x) == "terms")))),
                     failure_message = 
                       'Some elemens do not include terms objects.'
    )
    
  }
  
  # Create test data
  #-----------------
  
  testdat <- as.data.frame(matrix(data     = runif(n = 16), 
                                  nrow     = 4, 
                                  ncol     = 4,
                                  dimnames = list(NULL,
                                                  c('dep', 
                                                    'ind1', 
                                                    'ind2', 
                                                    'ind3'))))
  
  testdat[2, 4] <- NA
  testdat[1, 1] <- NA
  testdat[3, 2] <- NA
  
  ncmp <- 4
  
  # Model with two parts on the right-hand side
  #--------------------------------------------
  
  f <- dep ~ ind1 + ind2 | ind2 + ind3 + ind2:ind3
  
  tm <- aldvmm.tm(mf = stats::model.frame(Formula::Formula(f), data = testdat),
                  Formula = Formula::Formula(f),
                  ncmp = ncmp,
                  lcoef = c('beta', 'delta'))
  
  test_tm(tm = tm,
          ncmp = ncmp)
  
  # Model with one part on the right-hand side
  #-------------------------------------------
  
  f <- dep ~ ind1 + ind2
  
  tm <- aldvmm.tm(mf = stats::model.frame(Formula::Formula(f), data = testdat),
                  Formula = Formula::Formula(f),
                  ncmp = ncmp,
                  lcoef = c('beta', 'delta'))
  
  test_tm(tm = tm,
          ncmp = ncmp)
  
  # Single-component model with one part on the right-hand side
  #------------------------------------------------------------
  
  f <- dep ~ ind1 + ind2
  
  tm <- aldvmm.tm(mf = stats::model.frame(Formula::Formula(f), data = testdat),
                  Formula = Formula::Formula(f),
                  ncmp = 1,
                  lcoef = c('beta', 'delta'))
  
  test_tm(tm = tm,
          ncmp = 1)
  
  rm(testdat, ncmp, tm, f)
  
})
