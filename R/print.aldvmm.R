print.aldvmm <- function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
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
  cat("\nDegrees of Freedom:", unlist(x$df.null), "Total (i.e. Null); ",
      unlist(x$df.residual), "Residual\n")
  if(nzchar(mess <- naprint(x$na.action))) cat("  (",mess, ")\n", sep = "")
  cat("Log-likelihood:	   ",	format(signif(x$ll, digits)),
      "\nResidual Deviance:", format(signif(x$deviance, digits)),
      "\tAIC:", format(signif(x$aic, digits)),
      "\tBIC:", format(signif(x$bic, digits)))
  cat("\n")
  invisible(x)
}