test_that('Check generation of summary table.', {
  
  getval <- function(regtab) {
    test <- list()
    test <- regtab[-(1:2), -(1:2)]
    test <- test[-nrow(test), ]
    test <- test[!apply(test, 1, function(r) any(r %in% c("") | 
                                                   grepl('--', r))), ]
    test <- as.numeric(unlist(test))
    test <- as.character(format(round(test, 3), 
                                nsmall = 3,
                                trim = TRUE))
    test <- test[order(test)]
    
    return(test)
    
  }
  
  data(utility)
  
  for (i in 1:3) {
    
    suppressWarnings({
      suppressMessages({
        fit <- aldvmm(eq5d ~ age | female,
                      data = utility,
                      psi = c(-0.594, 0.883),
                      ncmp = i)
      })
    })
    
    suppressWarnings({
      suppressMessages({
        regtab <- aldvmm.sum(fit,
                             digits = max(3L, getOption("digits") - 3L),
                             level = 0.95)
      })
    })
    
    testthat::expect(is.data.frame(regtab),
                     failure_message = 
                       'Summary table is not data.frame object.'
    )
    nochar <- unlist(lapply(regtab, function(x) !is.character(x)))
    testthat::expect(sum(nochar) == 0,
                     failure_message = 'Summary table is not character table.'
    )
    rm(nochar)
    testthat::expect(all(!is.na(regtab)),
                     failure_message = 'Summary table includes missing values.'
    )
  }
})
