#' Extract Coefficients of Adjusted Limited Dependent Variable Mixture 
#' Model Fits
#'
#' The method \code{coef.aldvmm} for the generic function
#' \ifelse{html}{\code{\link[stats]{coef}}}{\code{stats::coef()}} extracts the 
#' vector of coefficients from an object of class "aldvmm".
#'
#' @param object an object inheriting from class "aldvmm".
#' @param ... further arguments passed to or from other methods.
#'
#' @return a named numeric vector of parameter estimates.
#'
#' @method coef aldvmm
#' @rdname coef
#' @export coef.aldvmm
#'
#' @export

coef.aldvmm <- function (object, 
                         ...) {
  object$coef
}

#' Create Matrix of Adjusted Limited Dependent Variable Mixture Model Gradients per Observation
#'
#' The method \code{estfun.aldvmm} for the generic function 
#' \ifelse{html}{\code{\link[sandwich]{estfun}}}{\code{sandwich::estfun()}} calculates the gradient of the aldvmm 
#' log-likelihood using with respect to parameter values for each observation
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.sc}}}{\code{aldvmm::aldvmm.sc()}}. 
#'
#' @param x an object inheriting from class "aldvmm".
#' @param ... further arguments passed to or from other methods.
#'
#' @return a numeric matrix of gradients with one row per 
#' observation and one column per parameter.
#'
#' @method estfun aldvmm
#' @rdname estfun
#' @export estfun.aldvmm
#'
#' @export

estfun.aldvmm <- function(x,
                          ...) {
  
  aldvmm.sc(par = x$coef,
            X = model.matrix(x),
            y = x$pred$y,
            psi = x$psi,
            ncmp = x$k,
            dist = x$dist,
            lcoef = x$label$lcoef,
            lcmp  = x$label$lcmp,
            lcpar = x$label$lcpar)
  
}

#' Extract Adjusted Limited Dependent Variable Mixture Model Number of Observations
#'
#' The method \code{nobs.aldvmm} for the generic function
#' \ifelse{html}{\code{\link[stats]{nobs}}}{\code{stats::nobs()}} extracts the 
#' number of observations from an object of class "aldvmm".
#'
#' @param object an object inheriting from class "aldvmm".
#' @param ... further arguments passed to or from other methods.
#'
#' @return a scalar of the number of complete 
#' observations.
#'
#' @method nobs aldvmm
#' @rdname nobs
#' @export nobs.aldvmm
#'
#' @export

nobs.aldvmm <- function(object,
                        ...) {
  object$n
}

#' Extract Adjusted Limited Dependent Variable Mixture Model Formula
#'
#' The method \code{formula.aldvmm} for the generic function
#' \ifelse{html}{\code{\link[stats]{formula}}}{\code{stats::formula()}} returns the 
#' formula object from an object of class "aldvmm".
#'
#' @param object an object inheriting from class "aldvmm".
#' @param ... further arguments passed to or from other methods.
#'
#' @return an object of class "formula"
#'
#' @method formula aldvmm
#' @rdname formula
#' @export formula.aldvmm
#'
#' @export 

formula.aldvmm <- function(object,
                           ...) {
  object$formula
}

#' Extract Adjusted Limited Dependent Variable Mixture Model Model Matrices
#'
#' The method
#' \code{model.matrix.aldvmm} for the generic function \ifelse{html}{\code{\link[stats]{model.matrix}}}{\code{stats::model.matrix()}} extracts a list of model matrices from an object 
#' of class "aldvmm" using the function 
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.mm}}}{\code{aldvmm::aldvmm.mm()}}.
#'
#' @param object an object inheriting from class "aldvmm".
#' @param ... further arguments passed to or from other methods.
#'
#' @return a named list of numeric matrices. 
#' \item{\code{beta}}{a numeric design matrix for the model of component 
#' distributions.}
#' \item{\code{delta}}{a numeric design matrix of the multinomial logit model
#'   of probabilities of component membership.}
#' 
#' @method model.matrix aldvmm
#' @rdname model.matrix
#' @export model.matrix.aldvmm
#'
#' @export

model.matrix.aldvmm <- function(object,
                                ...) {
  
  aldvmm.mm(mf      = object$data, 
            Formula = Formula::Formula(object$formula), 
            ncmp    = object$k, 
            lcoef   = object$label$lcoef)
}

#' Predict Method for Adjusted Limited Dependent Variable Mixture Model Fits
#'
#' The method \code{predict.aldvmm} for the generic function
#' \ifelse{html}{\code{\link[stats]{predict}}}{\code{stats::predict()}} calls
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.pred}}}{\code{aldvmm.pred()}} and \ifelse{html}{\code{\link[aldvmm]{aldvmm.sefit}}}{\code{aldvmm.sefit()}} to
#' predict outcomes and their standard errors in new data.
#'
#' @param object an object inheriting from class 'aldvmm'.
#' @param newdata a data frame, list or environment (or object coercible to a
#'   data frame by \cr \ifelse{html}{\code{\link[base]{as.data.frame}}}{
#'   \code{base::as.data.frame()}}) including explanatory variables for
#'   prediction.
#' @inheritParams aldvmm.sefit
#' @inheritParams aldvmm
#' @param ... further arguments passed to or from other methods.
#'
#' @return a named list of numeric vectors of predicted outcomes, standard
#'   errors and confidence or prediction intervals.
#'
#' @method predict aldvmm
#' @rdname predict
#' @export predict.aldvmm
#'
#' @export

predict.aldvmm <- function(object, 
                           newdata = NULL,
                           se.fit = FALSE,
                           type = "pred",
                           level = 0.95,
                           ...) {
  
  # Extract data
  #-------------
  
  if (is.null(newdata)) {
    if (is.null(object$data)) {
      stop("No data supplied to 'newdata' or included in 'object'.",
           "\n")
    } else {
      newdata <- object$data
    }
  }
  
  # Convert newdata to data.frame object
  #-------------------------------------
  
  tryCatch({
    newdata <- as.data.frame(newdata)
  }, error = function(e) {
    #message(e)
    stop("'newdata' cannot be converted to data.frame.",
         "\n")
  })
  
  # Make list of empty outcome vectors including incomplete rows of newdata
  #------------------------------------------------------------------------
  
  pred <- list()
  
  pred[["yhat"]] <- rep(NA, times = nrow(newdata))
  names(pred[["yhat"]]) <- rownames(newdata)
  
  pred[["se.fit"]] <- rep(NA, times = nrow(newdata))
  names(pred[["se.fit"]]) <- rownames(newdata)
  
  pred[["ll"]] <- rep(NA, times = nrow(newdata))
  names(pred[["ll"]]) <- rownames(newdata)
  
  pred[["ul"]] <- rep(NA, times = nrow(newdata))
  names(pred[["ul"]]) <- rownames(newdata)
  
  # Make list of design matrices
  #-----------------------------
  
  formula <- Formula::Formula(object[["formula"]])
  newdata <- stats::model.frame(formula, data = newdata)
  
  mm <- aldvmm.mm(mf = newdata,
                  Formula = formula,
                  ncmp = object[["k"]],
                  lcoef = object[["label"]][["lcoef"]])
  
  # Predict outcomes
  #-----------------
  
  # Expected values
  tmp <- aldvmm.pred(par   = object[["coef"]],
                     X     = mm,
                     psi   = object[["psi"]],
                     ncmp  = object[["k"]],
                     dist  = object[["dist"]],
                     lcoef = object[["label"]][["lcoef"]],
                     lcpar = object[["label"]][["lcpar"]],
                     lcmp  = object[["label"]][["lcmp"]])
  
  pred[["yhat"]][names(tmp[["yhat"]])] <- tmp[['yhat']]
  
  # Estimate standard errors of predictions and prediction intervals
  #-----------------------------------------------------------------
  
  if (se.fit == TRUE) {
    # Standard errors
    tmp <- list()
    tmp <- aldvmm.sefit(par     = object[["coef"]],
                        yhat    = pred[["yhat"]],
                        X       = mm,
                        type    = type,
                        cv      = object[["cov"]],
                        mse     = object[["gof"]][["mse"]],
                        psi     = object[["psi"]],
                        ncmp    = object[["k"]],
                        dist    = object[["dist"]],
                        level   = object[["level"]],
                        lcoef   = object[["label"]][["lcoef"]],
                        lcpar   = object[["label"]][["lcpar"]],
                        lcmp    = object[["label"]][["lcmp"]])
    
    # Add output to complete observations of newdata
    pred[["se.fit"]][names(tmp[["se.fit"]])] <- tmp[['se.fit']]
    pred[["ll"]][names(tmp[["lower.fit"]])] <- tmp[['lower.fit']]
    pred[["ul"]][names(tmp[["upper.fit"]])] <- tmp[['upper.fit']]
    
  }
  
  return(pred)
  
}

#' Print Adjusted Limited Dependent Variable Mixture Model Fits
#'
#' The method \code{print.aldvmm} for the generic function
#' \ifelse{html}{\code{\link[base]{print}}}{\code{base::print()}} prints a 
#' summary of an object of class "aldvmm".
#'
#' @param x an object inheriting from class "aldvmm".
#' @param digits an integer value of the number of digits in the printed 
#' output.
#' @param ... further arguments passed to or from other methods.
#'
#' @method print aldvmm
#' @rdname print
#' @export print.aldvmm
#'
#' @export

print.aldvmm <- function(x, 
                         digits = max(3L, getOption("digits") - 3L), 
                         ...) {
  cat("\nCall:  ",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  if(length(coef(x))) {
    cat("Coefficients")
    if(is.character(co <- x$contrasts))
      cat("  [contrasts: ",
          apply(cbind(names(co),co), 1L, paste, collapse = "="), "]")
    cat(":\n")
    print.default(format(x$coef, digits = digits),
                  print.gap = 2, quote = FALSE)
  } else cat("No coefficients\n\n")
  
  cat("\nLog-likelihood:",	format(signif(x$gof$ll, digits)),
      "\tAIC:", format(signif(x$gof$aic, digits)),
      "\tBIC:", format(signif(x$gof$bic, digits)))
  
  cat("\nDegrees of Freedom (null):    ", x$df.null,
      "\nDegrees of Freedom (residual):", x$df.residual, "\n")
  if(nzchar(mess <- naprint(x$na.action))) cat("  (",mess, ")\n", sep = "")
  
  cat("\n")
  invisible(x)
}

#' Printing Adjusted Limited Dependent Variable Mixture Model Summaries
#'
#' The method \code{print.summary.aldvmm} for the generic function
#' \ifelse{html}{\code{\link[base]{print}}}{\code{base::print()}} prints a 
#' summary of an object of class "summary.aldvmm".
#'
#' @param x an object inheriting from class 'summary.aldvmm'.
#' @param digits an integer value of the number of digits in the output table.
#' @param ... further arguments passed to or from other methods.
#'
#' @method print summary.aldvmm
#' @rdname print.summary
#'
#' @export

print.summary.aldvmm <- function(x,
                                 digits = max(3L, getOption("digits") - 3L),
                                 ...) {
  
  
  # Print
  #------
  
  cat("\nCall:\n")
  print(x$call)
  cat("\n")
  
  cat("\nCoefficients:")
  print(x$summary,
        row.names = FALSE, 
        right = TRUE)
  
  cat("Log-likelihood:",	format(signif(x$ll, digits)),
      "\tAIC:", format(signif(x$aic, digits)),
      "\tBIC:", format(signif(x$bic, digits)))
  
  cat("\nDegrees of Freedom (null):    ", x$df.null,
      "\nDegrees of Freedom (residual):", x$df.residual, "\n")
  
  invisible(x)
}

#' Extract Adjusted Limited Dependent Variable Mixture Model Residuals
#'
#' The method \code{residuals.aldvmm} for the generic function
#' \ifelse{html}{\code{\link[stats]{residuals}}}{\code{stats::residuals()}} returns the 
#' covariance matrix from an object of class "aldvmm".
#'
#' @param object an object inheriting from class "aldvmm".
#' @param ... further arguments passed to or from other methods.
#'
#' @return a numeric vector of residuals.
#'
#' @method residuals aldvmm
#' @rdname residuals
#' @export residuals.aldvmm
#'
#' @export 

residuals.aldvmm <- function(object,
                             ...) {
  object$pred$res
}

#' Summarizing Adjusted Limited Dependent Variable Mixture Model Fits
#'
#' The method \code{summary.aldvmm} for the generic function
#' \ifelse{html}{\code{\link[base]{summary}}}{\code{base::summary()}} creates 
#' an object of class "summary.aldvmm" including key results from an object of 
#' class "aldvmm".
#' 
#' @param object an object inheriting from class 'aldvmm'.
#' @param digits an integer value of the number of digits in the output table.
#' @param level a numeric value of the confidence interval between 0 and 1.
#' @param ... further arguments passed to or from other methods.
#'
#' @return \code{summary.aldvmm} returns an object of class "summary.aldvmm" 
#' including the following elements. 
#'   \item{\code{call}}{a character value including the model call captured by
#'   \ifelse{html}{\code{\link[base]{match.call}}}{\code{base::match.call}}.}
#'   \item{\code{summary}}{a data frame generated by 
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm.sum}}}{\code{aldvmm::aldvmm.sum}} 
#'   including a formatted summary of model results for printing.}
#'   \item{\code{terms}}{a list of objects of class
#'   \ifelse{html}{\code{\link[stats]{terms}}}{\code{stats::terms}}.}
#'   \item{\code{contrasts}}{a nested list of character values showing 
#'   contrasts of factors used in models of component means ("beta") and 
#'   probabilities of component membership ("delta").}
#'   \item{\code{coef}}{a
#'   numeric vector of parameter estimates.}
#'   \item{\code{cov}}{a numeric matrix object with covariances of parameters.}
#'   \item{\code{n}}{a scalar representing the number of complete observations
#'   with no missing values that were used in the estimation.}
#'   \item{\code{df.residual}}{an integer value of the residual 
#'   degrees of freedom.}
#'   \item{\code{df.null}}{an integer value of the residual 
#'   degrees of freedom of a null model including intercepts and standard errors.}
#'   \item{\code{iter}}{an integer value of the number of iterations used in 
#'   optimization.}
#'   \item{\code{ll}}{a numeric value of the negative log-likelihood
#'   \eqn{-ll}.}
#'   \item{\code{aic}}{a numeric value of the Akaike information
#'   criterion \eqn{AIC = 2n_{par} - 2ll}{AIC = 2*npar - 2*ll}.}
#'   \item{\code{bic}}{a numeric value of the Bayesian information criterion
#'   \eqn{BIC = n_{par}*log(n_{obs}) - 2ll}{BIC = npar*log(nobs) - 2*ll}.}
#'   \item{\code{k}}{a numeric value of the number of components.}
#'   \item{\code{lcoef}}{a character vector of labels for objects including
#'   results on distributions (default \code{"beta"}) and the probabilities of
#'   component membership (default \code{"delta"}).} 
#'   \item{\code{lcpar}}{a
#'   character vector of labels for objects including constant distribution
#'   parameters (default \code{"sigma"} for \code{dist = "normal"}).}
#'   \item{\code{lcmp}}{a character value of the label for objects including
#'   results on different components (default "Comp")} 
#'   \item{\code{lvar}}{a
#'   list including 2 character vectors of covariate names for model parameters
#'   of distributions (\code{"beta"}) and the multinomial logit
#'   (\code{"delta"}).}
#'   
#' @method summary aldvmm
#' @rdname summary
#'
#' @export summary.aldvmm
#' 
#' @export

summary.aldvmm <- function(object, 
                           digits = max(3L, getOption("digits") - 3L),
                           level = 0.95,
                           ...) {
  
  # Create summary table
  #---------------------
  
  regtab <- aldvmm.sum(object = object,
                       digits = digits,
                       level = level)
  
  # Create output list
  #-------------------
  
  ans <- list()
  
  ans[["call"]] <- object[["call"]]
  ans[["summary"]] <- format(as.data.frame(regtab), trim = TRUE)
  ans[["terms"]] <- object[["terms"]]
  ans[["contrasts"]] <- object[["contrasts"]]
  ans[["coef"]] <- object[["coef"]]
  ans[["cov"]] <- object[["cov"]]
  ans[["n"]] <- object[["n"]]
  ans[["df.residual"]] <- object[["df.residual"]]
  ans[["df.null"]] <- object[["df.null"]]
  ans[["iter"]] <- object[["iter"]]
  ans[["ll"]] <- object[["gof"]][["ll"]]
  ans[["aic"]] <- object[["gof"]][["aic"]]
  ans[["bic"]] <- object[["gof"]][["bic"]]
  ans[["k"]] <- object[["k"]]
  ans[["lcoef"]] <- object[["label"]][["lcoef"]]
  ans[["lcpar"]] <- object[["label"]][["lcpar"]]
  ans[["lcmp"]] <- object[["label"]][["lcmp"]]
  ans[["lvar"]] <- object[["label"]][["lvar"]]
  
  class(ans) <- "summary.aldvmm"
  return(ans)
}

#' Extract Adjusted Limited Dependent Variable Mixture Model Terms
#'
#' The method \code{terms.aldvmm} for the generic function
#' \ifelse{html}{\code{\link[stats]{terms}}}{\code{stats::terms()}} returns the 
#' terms object for the combined model of component means and probabilities of
#' component membership from an object of class "aldvmm".
#'
#' @param object an object inheriting from class "aldvmm".
#' @param ... further arguments passed to or from other methods.
#'
#' @return an object of class "terms"
#'
#' @method terms aldvmm
#' @rdname terms
#' @export terms.aldvmm
#'
#' @export 

terms.aldvmm <- function(object,
                         ...) {
  object$terms$full
}

#' Update Adjusted Limited Dependent Variable Mixture Model Fit
#'
#' The method \code{update.aldvmm} for the generic function
#' \ifelse{html}{\code{\link[stats]{update}}}{\code{stats::update()}} 
#' re-estimates an object of class "aldvmm".
#'
#' @param object an object inheriting from class "aldvmm".
#' @param formula. a formula object representing the new model.
#' @param evaluate a logical value indicating if the model should be 
#' re-estimated (TRUE) or not (FALSE).
#' @param ... further arguments passed to or from other methods.
#'
#' @return an object of class "aldvmm"
#'
#' @method update aldvmm
#' @rdname update
#' @export update.aldvmm
#'
#' @export

update.aldvmm <- function (object, formula., ..., evaluate = TRUE)
{
  if(is.null(call <- getCall(object))) stop("need an object with call component")
  extras <- match.call(expand.dots = FALSE)$...
  if(!missing(formula.)) call$formula <- formula(update(Formula(Formula::Formula(formula(object))), formula.))
  if(length(extras)) {
    existing <- !is.na(match(names(extras), names(call)))
    for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
    if(any(!existing)) {
      call <- c(as.list(call), extras[!existing])
      call <- as.call(call)
    }
  }
  if(evaluate) eval(call, parent.frame())
  else call
}

#' Extract Adjusted Limited Dependent Variable Mixture Model Covariance Matrix
#'
#' The method \code{vcov.aldvmm} for the generic function
#' \ifelse{html}{\code{\link[stats]{vcov}}}{\code{stats::vcov()}} returns the 
#' covariance matrix from an object of class "aldvmm".
#'
#' @param object an object inheriting from class "aldvmm".
#' @param ... further arguments passed to or from other methods.
#'
#' @return a numeric matrix.
#'
#' @method vcov aldvmm
#' @rdname vcov
#' @export vcov.aldvmm
#'
#' @export 

vcov.aldvmm <- function(object,
                        ...) {
  object$cov
}