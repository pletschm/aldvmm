test_that('Check creation of model matrices', {
  
  # Define tests in function
  #-------------------------
  
  test_mm <- function(mm,
                      ncmp) {
    
    testthat::expect_named(mm)
    testthat::expect(length(mm) == min(2, ncmp),
                     failure_message = 
                       'The list of model matrices is the wrong length.'
    )
    testthat::expect(is.list(mm), 
                     failure_message = 
                       'aldvmm.mm output is not list.'
    )
    testthat::expect(sum(unlist(lapply(mm, 
                                       function(x) 
                                         is.matrix(x)))) == min(2, ncmp),
                     failure_message = 
                       'Model matrices does not include matrix objects.'
    )
    testthat::expect(sum(unlist(lapply(mm, 
                                       function(x) 
                                         is.null(colnames(x)) ))) == 0,
                     failure_message = 
                       'Model matrices includes matrices wo column names.'
    )
    testthat::expect(sum(unlist(lapply(mm, 
                                       function(x) 
                                         !('(Intercept)' %in% 
                                             colnames(x))))) == 0,
                     failure_message = 
                       'Some model matrices do not inlcude Intercept column.'
    )
    testthat::expect(sum(unlist(lapply(mm, function(x) is.na(x)))) == 0,
                     failure_message = 
                       'The model matrices include missing values.'
    )
    testthat::expect(sum(unlist(lapply(mm, function(x) !is.numeric(x)))) == 0,
                     failure_message = 'The model matrices are not numeric.'
    )
    testthat::expect(sum(unlist(lapply(mm, 
                                       function(x) 
                                         rownames(mm[[1]]) != 
                                         rownames(x)))) == 0,
                     failure_message = 
                       'Model matrices include different rows from orig. data.'
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
  
  mm <- aldvmm.mm(mf = stats::model.frame(Formula::Formula(f), data = testdat),
                  Formula = Formula::Formula(f),
                  ncmp = ncmp,
                  lcoef = c('beta', 'delta'))
  
  test_mm(mm = mm,
          ncmp = ncmp)
  
  # Model with one part on the right-hand side
  #-------------------------------------------
  
  f <- dep ~ ind1 + ind2
  
  mm <- aldvmm.mm(mf = stats::model.frame(Formula::Formula(f), data = testdat),
                  Formula = Formula::Formula(f),
                  ncmp = ncmp,
                  lcoef = c('beta', 'delta'))
  
  test_mm(mm = mm,
          ncmp = ncmp)
  
  rm(testdat, ncmp, mm, f)
  
})
