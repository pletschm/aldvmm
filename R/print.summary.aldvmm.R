#' Printing Adjusted Limited Dependent Variable Mixture Model Summaries
#'
#' The method
#' \code{print.summary.aldvmm} prints a summary of an 
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{\code{aldvmm::aldvmm()}} model 
#' fit.
#'
#' @param x an object inheriting from class 'summary.aldvmm'.
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
