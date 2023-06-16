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
