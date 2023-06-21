#' Creating Initial Values
#'
#' @description
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.init}}}{\code{aldvmm.init()}}
#' creates initial values for the minimization of the negative log-likelihood
#' returned by
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.ll}}}{\code{aldvmm.ll()}} using
#' \ifelse{html}{\code{\link[optimr]{optimr}}}{\code{optimr::optimr()}}.
#'
#' @inheritParams aldvmm
#' @inheritParams aldvmm.ll
#'
#' @details \code{'init.method'} accepts four methods for generating initial
#'   values: \code{"zero"}, \code{"random"}, \code{"constant"}, \code{"sann"}.
#'   The method \code{"zero"} sets initial values of all parameters to 0. The
#'   method \code{"random"} draws random starting values from a standard normal
#'   distribution.  The method \code{"constant"} estimates a constant-only
#'   model and uses estimates as initial values for intercepts and constant
#'   distribution parameters and 0 for all other parameters.  The method
#'   \code{"sann"} estimates the full model using the simulated annealing
#'   optimization method and uses all parameter estimates as initial values.
#'   When user-specified initial values are supplied in \code{'init.est'}, the
#'   argument \code{'init.method'} is ignored.
#'
#'   By default, \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{ \code{aldvmm()}}
#'   performs unconstrained optimization with upper and lower limits at
#'   \code{-Inf} and \code{Inf}.  When user-defined lower and upper limits are
#'   supplied to \code{'init.lo'} and/or \code{'init-hi'}, these default limits
#'   are replaced with the user-specified values, and the method
#'   \code{"L-BFGS-B"} is used for box-constrained optimization instead of the
#'   user defined \code{'optim.method'}.  It is possible to only set either
#'   maximum or minimum limits.
#'
#' @return
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.init}}}{\code{aldvmm.init()}}
#' returns a list with the following objects. \item{\code{est}}{a numeric
#' vector of initial values of parameters.}
#' \item{\code{lo}}{a numeric vector of lower limits of parameters.}
#' \item{\code{hi}}{a numeric vector of upper limits of parameters.}
#'
#' @export

aldvmm.init <- function(X,
                        y,
                        psi,
                        ncmp,
                        dist,
                        init.method,
                        init.est,
                        init.lo,
                        init.hi,
                        optim.method,
                        optim.control = list(),
                        optim.grad,
                        lcoef,
                        lcpar,
                        lcmp) {
  
  # Zero-only initial parameter values
  #-----------------------------------
  
  # We create zero initial values regardless of the user-selected method 
  # because they are being used by multiple methods for creating initial 
  # values.
  
  zero <- list()
  
  # Betas (coefficients on distribution parameters)
  zero[[lcoef[1]]] <- list(est = rep(0, times = ncmp * ncol(X[[lcoef[1]]])))
  # Adding sublists with list() is necessary when val is length 1
  names(zero[[lcoef[1]]][["est"]]) <- aldvmm.getnames(X     = X,
                                                      names = c(lcoef[1]),
                                                      lcoef = lcoef,
                                                      lcpar = lcpar,
                                                      lcmp  = lcmp,
                                                      ncmp  = ncmp)
  
  
  # Deltas (coefficients for multinomial logit for group membership)
  if (ncmp > 1) {
    # Adding sublists with list() is necessary when val is length 1
    zero[[lcoef[2]]] <- list(est = rep(0, 
                                       times = (ncmp - 1)*ncol(X[[lcoef[2]]])))
    names(zero[[lcoef[2]]][["est"]]) <- aldvmm.getnames(X     = X,
                                                        names = c(lcoef[2]),
                                                        lcoef = lcoef,
                                                        lcpar = lcpar,
                                                        lcmp  = lcmp,
                                                        ncmp  = ncmp)
  }
  
  # Constant distribution parameters
  for (i in lcpar) {
    # Adding sublists with list() is necessary when val is length 1
    zero[[i]] <- list(est = rep(0, times = ncmp))
    names(zero[[i]][["est"]]) <- aldvmm.getnames(X     = X,
                                                 names = i,
                                                 lcoef = lcoef,
                                                 lcpar = lcpar,
                                                 lcmp  = lcmp,
                                                 ncmp  = ncmp)
  }
  
  # User selected method for creating initial values
  #-------------------------------------------------
  
  if (is.null(init.est)) {
    if (init.method == "zero") {
      
      # Zero-only initial values (default)
      #-----------------------------------
      
      init <- zero  
      
    } else if (init.method == "random") {
      
      # Random initial values
      #----------------------
      
      init <- zero  
      
      for (i in names(zero)) {
        init[[i]] <- list(est = stats::rnorm(n = length(zero[[i]][["est"]])))
        names(init[[i]][["est"]]) <- names(zero[[i]][["est"]])
      }
      
    } else if (init.method == "constant") {
      
      # Estimate constant-only model
      #-----------------------------
      
      # Make list of model matrices
      if (ncmp > 1) {
        X0 <- lapply(lcoef, function(x) matrix(1, nrow = nrow(X[[x]]),
                                               dimnames = list(NULL,
                                                               "(Intercept)")))
        names(X0) <- lcoef
      } else {
        X0 <- list( matrix(1, nrow = nrow(X[[1]]),
                           dimnames = list(NULL, "(Intercept)")) )
        names(X0) <- lcoef[1]
      }

      # Initial values of constant-only model
      tmp <- list()
      for (i in names(zero)) {
        index <- grepl(paste("(Intercept)", lcpar, sep = "|"), 
                       names(zero[[i]][["est"]]))
        tmp[[i]] <- list(est = zero[[i]][["est"]][index])
      }
      tmpnames <- unlist(lapply(tmp, function(x) names(x[["est"]])), 
                         use.names = FALSE)
      tmp <- unlist(tmp)
      names(tmp) <- tmpnames
      
      # Attach gradient function
      if (optim.grad == TRUE) {
        grd <- aldvmm.gr
      } else {
        grd <- NULL
      }
      
      # Fit model
      fit <- optimr::optimr(method       = optim.method,
                            fn           = aldvmm.ll,
                            par          = tmp,
                            X            = X0,
                            y            = y,
                            psi          = psi,
                            dist         = dist,
                            ncmp         = ncmp,
                            lcoef        = lcoef,
                            lcmp         = lcmp,
                            lcpar        = lcpar,
                            optim.method = optim.method,
                            gr           = grd,
                            hessian      = FALSE,
                            control      = optim.control)
      
      # Make vector of initial values
      init <- zero
      for (i in names(zero)) {
        index <- which(names(init[[i]][["est"]]) %in% names(fit[["par"]]))
        init[[i]][["est"]][index] <- fit[["par"]][names(init[[i]][["est"]])[index]]
      }
      
      rm(X0, fit, tmp, index)
      
    } else if (init.method == "sann") {
      
      # Estimate simulated annealing (SANN) algorithm
      #----------------------------------------------
      
      # Initial values of SANN model
      tmp <- zero
      tmpnames <- unlist(lapply(tmp, function(x) names(x[["est"]])), 
                         use.names = FALSE)
      tmp <- unlist(tmp)
      names(tmp) <- tmpnames
      
      # Fit stats::optim
      fit <- stats::optim(fn           = aldvmm.ll,
                          par          = tmp,
                          X            = X,
                          y            = y,
                          psi          = psi,
                          dist         = dist,
                          ncmp         = ncmp,
                          lcoef        = lcoef,
                          lcmp         = lcmp,
                          lcpar        = lcpar,
                          optim.method = optim.method,
                          method       = "SANN",
                          hessian      = FALSE,
                          control      = optim.control)
      
      # Make vector of initial values
      init <- zero
      for (i in names(init)) {
        index <- which(names(init[[i]][["est"]]) %in% names(fit[["par"]]))
        init[[i]][["est"]][index] <- fit[["par"]][names(init[[i]][["est"]])[index]]
      }
      
      rm(fit)
    }
    
    # Convert nested list to vector of initial values
    #------------------------------------------------
    
    initnames <- unlist(lapply(init, function(x) names(x[["est"]])))
    tmplist <- init
    init <- list()
    init[["est"]] <- unlist(lapply(tmplist, function(x) x[["est"]]), 
                            use.names = FALSE)
    names(init[["est"]]) <- initnames
    rm(tmplist, initnames)
    
  } else {
    
    # User-defined initial values
    #----------------------------
    
    init <- list()
    init[["est"]] <- init.est
    
    names(init[["est"]]) <- aldvmm.getnames(X     = X,
                                            names = c(lcoef, lcpar),
                                            lcoef = lcoef,
                                            lcpar = lcpar,
                                            lcmp  = lcmp,
                                            ncmp  = ncmp)
  }
  
  # Constraints
  #------------
  
  # Lower limits
  if (!is.null(init.lo)) {
    init[["lo"]] <- init.lo
  } else { 
    init[["lo"]] <- rep(-Inf, times = length(init[["est"]]))
  }
  names(init[["lo"]]) <- names(init[["est"]])
  
  # Upper limits
  if (!is.null(init.hi)) {
    init[["hi"]] <- init.hi
  } else { 
    init[["hi"]] <- rep(Inf, times = length(init[["est"]]))
  }
  names(init[["hi"]]) <- names(init[["est"]])
  
  return(init)
  
}
