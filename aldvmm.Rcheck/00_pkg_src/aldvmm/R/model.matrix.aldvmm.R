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