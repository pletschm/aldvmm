new_aldvmm <- function(fit,
                       cov,
                       y,
                       mm,
                       ncmp,
                       gof,
                       pred,
                       pred.se = NULL,
                       init,
                       call,
                       formula,
                       terms,
                       data,
                       psi,
                       dist,
                       lcoef,
                       lcpar,
                       lcmp,
                       optim.method,
                       init.method,
                       level,
                       na.action) {
  
  # Calculate degrees of freedom
  #-----------------------------
  
  df.null <- length(y) - 
    ncmp * as.integer(attr(terms[[lcoef[1]]], "intercept") > 0L) - 
    (ncmp > 1) * as.integer(attr(terms[[lcoef[2]]], "intercept") > 0L) -
    ncmp # standard errors lnsigma
  
  
  df.residual <- length(y) - length(fit$par)
  
  # Make output list
  #-----------------
  
  outlist <- list(coef    = fit[["par"]],
                  hessian = cov[["hessian"]],
                  cov     = cov[["cv"]],
                  n       = length(y),
                  k       = lapply(mm, function (x) ncol(x)),
                  ncmp    = ncmp,
                  df.null = df.null,
                  df.residual = df.residual,
                  iter    = fit[["counts"]][1],
                  convergence = fit[["convergence"]],
                  gof     = list(ll      = gof[["ll"]],
                                 aic     = gof[["aic"]],
                                 bic     = gof[["bic"]],
                                 mse     = gof[["mse"]],
                                 mae     = gof[["mae"]]),
                  pred    = list(y = pred[["y"]],
                                 yhat = pred[["yhat"]],
                                 res = pred[["res"]],
                                 se.fit = pred.se[["se.fit"]],
                                 lower.fit = pred.se[["lower.fit"]],
                                 upper.fit = pred.se[["upper.fit"]],
                                 prob = pred[["prob"]]),
                  init    = init,
                  call    = call,
                  formula = formula,
                  terms   = terms,
                  contrasts = lapply(mm, function (x) attr(x, "contrasts")),
                  data    = data,
                  psi     = psi,
                  dist    = dist,
                  label   = list("lcoef" = lcoef, 
                                 "lcpar" = lcpar,
                                 "lcmp" = lcmp,
                                 "lvar" = lapply(mm, function(x) colnames(x))),
                  optim.method = optim.method,
                  init.method = init.method,
                  level   = level,
                  na.action = na.action)
  
  structure(outlist, class = "aldvmm")
  
}