new_aldvmm <- function(dist,
                       coef,
                       se,
                       z,
                       p,
                       lower,
                       upper,
                       hessian,
                       cov,
                       n,
                       k,
                       ll,
                       aic,
                       bic,
                       mse,
                       mae,
                       yhat,
                       y,
                       res,
                       prob,
                       se.fit = NULL,
                       lower.fit = NULL,
                       upper.fit = NULL,
                       init,
                       formula,
                       psi,
                       lcoef,
                       lcpar,
                       lcmp,
                       lvar,
                       optim.method,
                       init.method,
                       level) {
  
  outlist <- list(coef    = coef,
                  se      = se,
                  z       = z,
                  p       = p,
                  lower   = lower,
                  upper   = upper,
                  hessian = hessian,
                  cov     = cov,
                  n       = n,
                  k       = k,
                  gof     = list(mse = mse,
                                 mae = mae,
                                 ll = ll,
                                 aic = aic,
                                 bic = bic),
                  pred    = list(y = y,
                                 yhat = yhat,
                                 res = res,
                                 se.fit = se.fit,
                                 lower.fit = lower.fit,
                                 upper.fit = upper.fit,
                                 prob = prob),
                  init    = init,
                  formula = formula,
                  psi     = psi,
                  dist    = dist,
                  label   = list("lcoef" = lcoef, 
                                 "lcpar" = lcpar,
                                 "lcmp" = lcmp,
                                 "lvar" = lvar),
                  optim.method = optim.method,
                  init.method = init.method,
                  level = level)
  
  structure(outlist, class = "aldvmm")

}