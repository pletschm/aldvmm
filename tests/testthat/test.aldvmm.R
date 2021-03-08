test_that('Check aldvmm fitting function.', {
  
  data(utility)
  utility[1, 3] <- NA
  utility[34, 1] <- NA
  utility[77, 2] <- NA
  
  # Message for incomplete observations in data
  suppressWarnings(
    testthat::expect_message(
      suppressWarnings({fit <- aldvmm(eq5d ~ age | female,
                                      data = utility,
                                      psi = c(-0.594, 0.883))})
    )
  )
  
})
