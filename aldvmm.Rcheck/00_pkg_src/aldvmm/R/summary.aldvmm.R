#' Summarizing Adjusted Limited Dependent Variable Mixture Model Fits
#'
#' The generic function
#' \ifelse{html}{\code{\link[base]{summary}}}{\code{base::summary()}} calls
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.sum}}}{\code{aldvmm.sum()}} to
#' print the summary table data frame returned by
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.sum}}}{\code{aldvmm.sum()}}.
#'
#' @param object an object inheriting from class 'aldvmm'.
#' @param ... further arguments passed to or from other methods.
#'
#' @method summary aldvmm
#' @rdname summary
#'
#' @export summary.aldvmm
#'
#' @export

summary.aldvmm <- function(object, ...) {
  
  # Create regression table
  #------------------------
  
  regtab <- aldvmm.sum(est    = object$coef,
                       se     = object$se,
                       z      = object$z,
                       p      = object$p,
                       lower  = object$lower,
                       upper  = object$upper,
                       n      = object$n,
                       value  = object$gof$ll,
                       aic    = object$gof$aic,
                       bic    = object$gof$bic,
                       ncmp   = object$k,
                       lcoef  = object$label$lcoef,
                       lcmp   = object$label$lcmp,
                       lcpar  = object$label$lcpar,
                       lvar   = object$label$lvar,
                       digits = 3)
  
  df <- format(as.data.frame(regtab), 
              trim = TRUE)
  
  print(df,
        row.names = FALSE, 
        right = TRUE)
  
}
