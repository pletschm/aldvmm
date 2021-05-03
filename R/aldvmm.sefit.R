#' Calculating Standard Errors of Fitted and Predicted Outcomes
#'
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.sefit}}}{\code{aldvmm::aldvmm.sefit()}}
#' calculates standard errors of fitted and predicted outcomes using the delta
#' method.
#'
#' @param yhat a numeric vector of predicted outcomes from
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm.pred}}}{\code{aldvmm::aldvmm.pred()}}.
#'
#' @param type a character value from 'fit' or 'pred' indicating whether the
#'   standard error of the fit ('fit') or the standard error of predictions in
#'   new data ('pred') are calculated.
#' @param cv a numeric matrix with covariances/variances of parameter estimates
#'   returned by
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm.cv}}}{\code{aldvmm::aldvmm.cv()}}.
#'
#'
#' @param mse a scalar a numeric value of the mean squared error of observed
#'   versus predicted outcomes \eqn{\sum{(y - \hat{y})^2}/(n_{obs} -
#'   n_{par})}{\sum{(y - \hat{y})^2}/(nobs - npar)} for all observations in
#'   \code{data} supplied to
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm.ll}}}{\code{aldvmm::aldvmm.ll()}}.
#'
#'
#'
#' @inheritParams aldvmm
#' @inheritParams aldvmm.ll
#'
#' @details
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.sefit}}}{\code{aldvmm::aldvmm.sefit()}}
#' calculates standard errors of fitted values using the delta method.  The
#' standard errors of fitted values in the estimation data set are calculated
#' as \eqn{se_{fit} = \sqrt{G^{t} \Sigma G}}{se_fit =
#' (t(grad)*\Sigma*grad)^0.5}, where \eqn{G} is the gradient of a fitted value
#' with respect to changes of parameter estimates, and \eqn{\Sigma} is the
#' estimated covariance matrix of parameters (Dowd et al., 2014).  The standard
#' errors of predicted values in new data sets using the method
#' \ifelse{html}{\code{\link[aldvmm]{predict.aldvmm}}}{
#' \code{aldvmm::predict.aldvmm()}} are calculated as \eqn{se_{pred} =
#' \sqrt{MSE + G^{t} \Sigma G}}{se_pred = (mse + t(grad)*\Sigma*grad)^0.5},
#' where \eqn{MSE}{mse} is the mean squared error of fitted versus observed
#' outcomes in the original estimation data (Whitmore, 1986).  The gradients of
#' fitted values with respect to parameter estimates are approximated
#' numerically using
#' \ifelse{html}{\code{\link[numDeriv]{jacobian}}}{\code{numDeriv::jacobian()}}.
#'
#' @references Whitmore, G. A. (1986). Prediction limits for a univariate
#'   normal observation. The American Statistician, 40(2), 141-143.
#'   https://doi.org/10.1080/00031305.1986.10475378 \cr
#'
#'   Dowd, B. E., Greene, W. H., and Norton, E. C. (2014) Computation of
#'   standard errors. \emph{Health services research}, \bold{49(2)}, 731--750.
#'   \doi{10.1111/1475-6773.12122}
#'
#' @return a named numeric vector of standard errors of fitted or predicted
#'   outcomes. The names of the elements in the vector are identical to the row
#'   names of design matrices in \code{'X'}
#'
#' @export

aldvmm.sefit <- function(par,
                         yhat,
                         X,
                         type,
                         formula,
                         psi,
                         cv,
                         mse,
                         ncmp,
                         dist,
                         level,
                         lcoef,
                         lcmp,
                         lcpar) {
  
  # Check validity of covariance matrix
  #------------------------------------
  
  if (any(is.na(cv))) {
    warning("missing values in covariance matrix: ",
            "No standard errors of the fit obtained\n",
            call. = FALSE)
    return(NULL)
  }
  
  if (any(diag(cv) <= 0)) {
    warning("non-positive diagonals in covariance matrix: ",
            "No standard errors of the fit obtained\n", 
            call. = FALSE)
    return(NULL)
  }
  
  # Initialize vector of standard errors of all observations
  #---------------------------------------------------------
  
  se.fit <- rep(NA, times = nrow(X[[1]]))
  names(se.fit) <- rownames(X[[1]])

  # Calculate jacobian matrix numerically
  #--------------------------------------
  
  jacobian <- numDeriv::jacobian(func = function(z) {
    aldvmm.pred(par   = z,
                X     = X,
                y     = rep(0, nrow(X[[1]])),
                psi   = psi,
                ncmp  = ncmp,
                dist  = dist,
                lcoef = lcoef,
                lcmp  = lcmp,
                lcpar = lcpar)[["yhat"]]
  },
  x = par)
    
  # Loop over all observations in design matrix
  #--------------------------------------------
  
  for (i in 1:nrow(X[[1]])) {
    
    # Calculate standard error
    #-------------------------
    
    if (!(type %in% c("fit", "pred"))) {
      warning("'type' ",
              'is not "fit" or "pred": "pred" is used\n',
              call. = FALSE)
    }
    
    if (type == "fit") {
      se.fit[i] <- sqrt(t(jacobian[i, ]) %*% cv %*% jacobian[i, ])
    } else {
      if (!is.null(mse) & !is.na(mse)) {
        se.fit[i] <- sqrt(mse + t(jacobian[i, ]) %*% cv %*% jacobian[i, ])
      } else {
        se.fit[i] <- sqrt(t(jacobian[i, ]) %*% cv %*% jacobian[i, ])
        warning("'mse' is missing: Standard errors of the fit are generated\n",
                call. = FALSE)
      }
    }
  }
  
  # Confidence / prediction interval
  #---------------------------------
  
  ul <- yhat + stats::qnorm((1 + level)/2) * se.fit
  ul[ul > max(psi)] <- 1
  names(ul) <- rownames(X[[1]])
  
  ll <- yhat - stats::qnorm((1 + level)/2) * se.fit
  ll[ll < min(psi)] <- min(psi)
  names(ll) <- rownames(X[[1]])
  
  return(list(se.fit    = se.fit,
              upper.fit = ul,
              lower.fit = ll))
  
}