#' Predict Method for Adjusted Limited Dependent Variable Mixture Model Fits
#'
#' The generic function
#' \ifelse{html}{\code{\link[stats]{predict}}}{\code{stats::predict()}} calls
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.pred}}}{\code{aldvmm.pred()}} to
#' predict outcomes and their standard errors in new data using
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.pred}}}{\code{aldvmm.pred()}} and
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.sefit}}}{\code{aldvmm.sefit()}}.
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
                           newdata,
                           se.fit = FALSE,
                           type = "pred",
                           level = 0.95,
                           ...) {
  
  # Convert newdata to data.frame object
  #-------------------------------------
  
  tryCatch({
    newdata <- as.data.frame(newdata)
  }, error = function(e) {
    #message(e)
    stop("'newdata' cannot be converted to data.frame.",
         "\n")
  })
  
  # Make list of design matrices
  #-----------------------------
  
  mm <- aldvmm.mm(data    = newdata,
                  formula = object[["formula"]],
                  ncmp    = object[["k"]],
                  lcoef   = object[["label"]][["lcoef"]])
  
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
  
  # Add missing predictions for incomplete observations in newdata
  pred <- list()
  pred[["yhat"]] <- rep(NA, times = nrow(newdata))
  names(pred[["yhat"]]) <- rownames(newdata)
  
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
                        formula = object[["formula"]],
                        cv      = object[["cov"]],
                        mse     = object[["gof"]][["mse"]],
                        psi     = object[["psi"]],
                        ncmp    = object[["k"]],
                        dist    = object[["dist"]],
                        level   = object[["level"]],
                        lcoef   = object[["label"]][["lcoef"]],
                        lcpar   = object[["label"]][["lcpar"]],
                        lcmp    = object[["label"]][["lcmp"]])
    
    # Add missing standard errors and confidence bands for incomplete 
    # observations in newdata
    pred[["se.fit"]] <- rep(NA, times = nrow(newdata))
    names(pred[["se.fit"]]) <- rownames(newdata)
    pred[["se.fit"]][names(tmp[["se.fit"]])] <- tmp[['se.fit']]
    
    pred[["ll"]] <- rep(NA,times = nrow(newdata))
    names(pred[["ll"]]) <- rownames(newdata)
    pred[["ll"]][names(tmp[["lower.fit"]])] <- tmp[['lower.fit']]
    
    pred[["ul"]] <- rep(NA,times = nrow(newdata))
    names(pred[["ul"]]) <- rownames(newdata)
    pred[["ul"]][names(tmp[["upper.fit"]])] <- tmp[['upper.fit']]
    
  }
  
  return(pred)
  
}
