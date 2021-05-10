test_that('Check aldvmm fitting function.', {
  
  # Message for incomplete observations in data
  #--------------------------------------------
  
  data(utility)
  utility[1, 3] <- NA
  utility[34, 1] <- NA
  utility[77, 2] <- NA
  
  # With numeric gradients
  suppressMessages({
    suppressWarnings({testthat::expect_message(
      aldvmm(eq5d ~ age | female,
             data = utility,
             psi = c(-0.594, 0.883))
    )
    })
  })
  
  # With gradient descent
  suppressMessages({
    suppressWarnings({
      testthat::expect_message(
        aldvmm(eq5d ~ age | female,
               data = utility,
               psi = c(-0.594, 0.883),
               optim.grad = FALSE)
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
  
  # With numeric gradients
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
  
  # With gradient descent
  suppressMessages({
    suppressWarnings({
      fit <- aldvmm(eq5d ~ age | female,
                    data = utility,
                    psi = c(-0.594, 0.883),
                    init.lo = rep(-99, length(fit$coef)),
                    optim.grad = FALSE)
    })
  })
  
  testthat::expect(fit$optim.method == "L-BFGS-B",
                   failure_message = 'optim.method should be "L-BFGS-B"')  
  
  # Unfeasible starting values
  #----------------------------
  
  # With numeric gradients
  suppressMessages({
    suppressWarnings({
      testthat::expect_error(aldvmm(eq5d ~ age | female,
                                    data = utility,
                                    psi = c(-0.594, 0.883),
                                    init.est = rep(-99, length(fit$coef))))
    })
  })
  
  
  # With gradient descent
  suppressMessages({
    suppressWarnings({
      testthat::expect_error(aldvmm(eq5d ~ age | female,
                                    data = utility,
                                    psi = c(-0.594, 0.883),
                                    init.est = rep(-99, length(fit$coef)),
                                    optim.grad = FALSE))
    })
  })
  
  
})

