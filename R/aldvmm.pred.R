#' Predicting Expected Values from Adjusted Limited Dependent Variable Mixture
#' Models
#'
#' @description
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.pred}}}{\code{aldvmm.pred()}} makes
#' predictions of observations in design matrices in \code{'X'} using parameter
#' estimates returned by
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{\code{aldvmm()}}.
#'
#' @inheritParams aldvmm.ll
#'
#' @details
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.pred}}}{\code{aldvmm.pred()}}
#' calculates expected values for observations in design matrices in \code{'X'}
#' using the expected value function published in Hernandez Alava and Wailoo
#' (2015). Constant distribution parameters that need to be non-negative (i.e.
#' standard deviations of normal distributions) enter the expected value
#' function as log-transformed values.
#'
#' @return a list of of predicted outcomes including the following elements.
#'   \item{\code{y}}{a numeric vector of observed outcomes in \code{'data'}.}
#'   \item{\code{yhat}}{a numeric vector of fitted values.} \item{\code{res}}{a
#'   numeric vector of residuals.} \item{\code{prob}}{a numeric matrix of expected 
#'   probabilities of group membership per individual in \code{'data'}.}
#'
#' @export

aldvmm.pred <- function(par,
                         X,
                         y = NULL,
                         psi,
                         ncmp,
                         dist,
                         lcoef,
                         lcpar,
                         lcmp) {
  
  psi1 <- max(psi)
  psi2 <- min(psi)
  
  # Check if par has names
  #-----------------------
  
  checkmate::assert_numeric(par, names = "named")
  
  # Create list of parameters
  #--------------------------
  
  parlist <- aldvmm.getpar(par   = par,
                           lcoef = lcoef,
                           lcmp  = lcmp,
                           lcpar = lcpar,
                           ncmp  = ncmp)
  
  # Multinomial logit
  #------------------
  
  if (ncmp > 1) {
    
    # Linear predictor
    wd <- lapply(names(parlist[[lcoef[2]]]), function (x) {
      rowSums(sweep(X[[lcoef[2]]], 
                    MARGIN = 2, 
                    parlist[[lcoef[2]]][[x]], 
                    `*`))
    })
    names(wd) <- names(parlist[[lcoef[2]]])
    
    # Denominator
    sumexp <- 1 + Reduce("+",
                         lapply(names(parlist[[lcoef[2]]]), function (z) {
                           exp(rowSums(sweep(X[[lcoef[2]]], 
                                             MARGIN = 2, 
                                             parlist[[lcoef[2]]][[z]], 
                                             `*`)))
                         }))
    
    # Probability of component membership
    A <- lapply(names(parlist[[lcoef[2]]]), function (x) {
      exp(wd[[x]]) / sumexp
    })
    A[[ncmp]] <- 1 - Reduce("+", A)
  } else {
    A <- list(
      matrix(data = 1, 
             nrow = nrow(X[[lcoef[1]]]), 
             ncol = 1,
             dimnames = list(rownames(X[[lcoef[2]]]),
                             paste0(lcmp, 1)))
    )
  }
  names(A) <- names(parlist[[lcoef[1]]])
  
  # Component distributions
  #------------------------
  
  if (dist == "normal") {
    
    # Linear predictor
    xb <- lapply(parlist[[lcoef[1]]], function (x) {
      rowSums(sweep(X[[lcoef[1]]], 
                    MARGIN = 2, 
                    x, 
                    `*`))
    })
    names(xb) <- names(parlist[[lcoef[1]]])
    
    # Density of values above maximum
    C <- lapply(names(parlist[[lcoef[1]]]), function (x) {
      1 - stats::pnorm((psi1 - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                       mean = 0, 
                       sd = 1)
    })
    names(C) <- names(parlist[[lcoef[1]]])
    
    # Density of values below minimum
    D <- lapply(names(parlist[[lcoef[1]]]), function (x) {
      stats::pnorm((psi2 - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                   mean = 0, 
                   sd = 1) * psi2
    })
    names(D) <- names(parlist[[lcoef[1]]])
    
    # Density of value within range
    E <- lapply(names(parlist[[lcoef[1]]]), function (x) {
      
      (stats::pnorm((psi1 - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                    mean = 0, 
                    sd = 1) -
         stats::pnorm((psi2 - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                      mean = 0, 
                      sd = 1)) *
        (xb[[x]] + exp(parlist[[lcpar]][[x]]) *
                         (stats::dnorm((psi1 - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                                       mean = 0, 
                                       sd = 1) - 
                            stats::dnorm((psi2 - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                                         mean = 0, 
                                         sd = 1)) /
                         (stats::pnorm((psi2 - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                                       mean = 0, 
                                       sd = 1) - 
                            stats::pnorm((psi1 - xb[[x]]) / exp(parlist[[lcpar]][[x]]), 
                                         mean = 0, 
                                         sd = 1)))
    })
    names(E) <- names(parlist[[lcoef[1]]])
    
    # Density of observed value
    B <- lapply(names(parlist[[lcoef[1]]]), function (x) {
      C[[x]] + D[[x]] + E[[x]]
    })
    names(B) <- names(parlist[[lcoef[1]]])
    
  }
  
  # Expected value
  #---------------
  
  V <-   Reduce("+",
                lapply(names(A), function (x) {
                  A[[x]] * B[[x]]
                })
  )
  
  # Collect and return
  #-------------------
  
  pred <- list(prob = do.call("cbind", A),
               yhat = V,
               y = if (!is.null(y)) {y} else {NULL},
               res = if (!is.null(y)) {y - V} else {NULL})
  
  if (!is.null(y)){
    names(pred[["res"]]) <- rownames(X[[1]])
    names(pred[["y"]]) <- rownames(X[[1]])  
  }
  
  if (any(is.na(pred[["prob"]]))) {
    warning("fitted probabilities of component membership include missing 
            values\n",
            call. = FALSE)
  }
  
  return(pred)
  
}
