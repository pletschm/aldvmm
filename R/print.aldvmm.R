#' Print Adjusted Limited Dependent Variable Mixture Model Fits
#'
#' The generic function
#' \ifelse{html}{\code{\link[stats]{coef}}}{\code{stats::print()}} prints key 
#' attributes of an object of class "aldvmm"}.
#'
#' @param object an object inheriting from class 'aldvmm'.
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
  cat("\nDegrees of Freedom:", c(rbind(unlist(x$df.null), c(" (beta), ", " (delta), ", " (full)"))), "Total (i.e. Null);",
      "\n                   ", c(rbind(unlist(x$df.residual), c(" (beta), ", " (delta)"))), "Residual\n")
  if(nzchar(mess <- naprint(x$na.action))) cat("  (",mess, ")\n", sep = "")
  cat("Log-likelihood:	   ",	format(signif(x$ll, digits)),
      "\nResidual Deviance:", format(signif(x$deviance, digits)),
      "\tAIC:", format(signif(x$aic, digits)),
      "\tBIC:", format(signif(x$bic, digits)))
  cat("\n")
  invisible(x)
}