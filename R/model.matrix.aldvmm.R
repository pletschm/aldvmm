#' Extract Adjusted Limited Dependent Variable Mixture Model Model Matrices
#'
#' The method
#' \code{model.matrix.aldvmm} extracts a list of model matrices from an object 
#' of class "aldvmm" using the function 
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.mm}}}{\code{aldvmm::aldvmm.mm()}}.
#'
#' @param object an object inheriting from class "aldvmm".
#' @param ... further arguments passed to or from other methods.
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
            ncmp    = object$ncmp, 
            lcoef   = object$label$lcoef)
}