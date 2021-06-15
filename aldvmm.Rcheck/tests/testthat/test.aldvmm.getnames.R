test_that('Check generation of parameter names.', {
  
  
  mm <- list(beta = data.frame(a = rep(1, 4), 
                               b = rep(2, 4),
                               c = rep(3, 4)),
             delta = data.frame(b = rep(4, 4), 
                                c = rep(5, 4),
                                d = rep(6, 4)))
  
  ref <- c("Grp1_beta_a", "Grp1_beta_b", "Grp1_beta_c", "Grp2_beta_a", 
           "Grp2_beta_b", "Grp2_beta_c", "Grp1_delta_b", "Grp1_delta_c", 
           "Grp1_delta_d", "Grp1_sigma", "Grp2_sigma")
  
  test <- aldvmm.getnames(X     = mm,
                          names = c('beta', 'delta', 'sigma'),
                          lcoef = c('beta', 'delta'),
                          lcpar = c('sigma'),
                          lcmp  = 'Grp',
                          ncmp  = 2)
  
  expect_equal(test, ref)
  
  rm(mm, ref)
  
})
