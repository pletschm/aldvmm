test_that('Check aldvmm fitting function.', {
  
  # Check if the model runs without error in default settings
  #----------------------------------------------------------
  
  data(utility)
  
  suppressMessages({
    suppressWarnings({
      testthat::expect_error(aldvmm(eq5d ~ age | female,
                                    data = utility,
                                    psi = c(-0.594, 0.883)),
                             NA)
    })
  })
  
  
  # Message for incomplete observations in data
  #--------------------------------------------
  
  data(utility)
  utility[1, 3] <- NA
  utility[34, 1] <- NA
  utility[77, 2] <- NA
  
  # With Analytical gradients
  suppressMessages({
    suppressWarnings({testthat::expect_message(
      aldvmm(eq5d ~ age | female,
             data = utility,
             psi = c(-0.594, 0.883))
    )
    })
  })
  
  # With numerical gradients
  suppressMessages({
    suppressWarnings({
      testthat::expect_message(
        aldvmm(eq5d ~ age | female,
               data = utility,
               psi = c(-0.594, 0.883),
               num.grad = TRUE)
      )
    })
  })
  
  # Optimization method in constraint optimization
  #-----------------------------------------------
  
  data(utility)
  
  # Auxiliary model for initial values
  suppressMessages({
    suppressWarnings({
      fit <- aldvmm(eq5d ~ age | female,
                    data = utility,
                    psi = c(-0.594, 0.883))
    })
  })
  
  # With analytical gradients
  suppressMessages({
    suppressWarnings({
      fit <- aldvmm(eq5d ~ age | female,
                    data = utility,
                    psi = c(-0.594, 0.883),
                    init.lo = rep(-99, length(fit$coef)))
    })
  })
  
  testthat::expect(fit$optim.method == "L-BFGS-B",
                   failure_message = 'optim.method should be "L-BFGS-B"')  
  
  # With numerical gradients
  suppressMessages({
    suppressWarnings({
      fit <- aldvmm(eq5d ~ age | female,
                    data = utility,
                    psi = c(-0.594, 0.883),
                    init.lo = rep(-99, length(fit$coef)),
                    num.grad = TRUE)
    })
  })
  
  testthat::expect(fit$optim.method == "L-BFGS-B",
                   failure_message = 'optim.method should be "L-BFGS-B"')  
  
  # Infeasible starting values
  #---------------------------
  
  # With analytical gradients
  suppressMessages({
    suppressWarnings({
      testthat::expect_error(aldvmm(eq5d ~ age | female,
                                    data = utility,
                                    psi = c(-0.594, 0.883),
                                    init.est = rep(-99, length(fit$coef))))
    })
  })
  
  
  # With numerical gradients
  suppressMessages({
    suppressWarnings({
      testthat::expect_error(aldvmm(eq5d ~ age | female,
                                    data = utility,
                                    psi = c(-0.594, 0.883),
                                    init.est = rep(-99, length(fit$coef)),
                                    num.grad = TRUE))
    })
  })
  
})

