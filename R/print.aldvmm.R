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