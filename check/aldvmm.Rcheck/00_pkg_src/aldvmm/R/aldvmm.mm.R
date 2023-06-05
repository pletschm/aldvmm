#' Creating Design Matrices
#'
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.mm}}}{\code{aldvmm.mm()}} creates
#' a list of two design matrices, one of the model of component distributions
#' (\code{"beta"}) and one of the model of probabilities of component
#' membership (\code{"delta"}).
#' 
#' @param mf a data frame created by \ifelse{html}{\code{\link[stats]{model.frame}}}{\code{stats::model.frame}} including the variables used in formula supplied to \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{\code{aldvmm()}} plus additional attributes, including "terms" for an object of class "terms" derived from formula.
#' @param Formula an object of class \code{"Formula"} created by \ifelse{html}{\code{\link[Formula]{Formula}}}{\code{Formula::Formula}} based on the \code{formula} supplied to \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{
#'   \code{aldvmm()}}.
#' @inheritParams aldvmm
#' @inheritParams aldvmm.ll
#'
#' @details \ifelse{html}{\code{\link[aldvmm]{aldvmm.mm}}}{\code{aldvmm.mm()}}
#'   uses
#'   \ifelse{html}{\code{\link[stats]{model.matrix}}}{\code{stats::model.matrix()}}
#'    to create design matrices for models of component distributions
#'   (\code{"beta"}) and probabilities of component membership (\code{"delta"})
#'   based on a \code{'Formula'} object created by \ifelse{html}{\code{\link[Formula]{Formula}}}{\code{Formula::Formula}} and a model frame created by \ifelse{html}{\code{\link[stats]{model.frame}}}{\code{stats::model.frame}}. The
#'   design matrix for probabilities of group membership is only created if
#'   more than one components are specified in \code{'ncmp'}.
#'
#' @return a named list of numeric matrices. \item{\code{beta}}{a numeric
#'   design matrix for the model of component distributions.}
#'   \item{\code{delta}}{a numeric design matrix of the multinomial logit model
#'   of probabilities of component membership.}
#'
#' @export

aldvmm.mm <- function(mf,
                      Formula,
                      ncmp,
                      lcoef) {
  
  if (ncmp > 1) {
    if (length(Formula)[2] > 1) {
      mm <- list(stats::model.matrix(Formula, data = mf, rhs = 1),
                 stats::model.matrix(Formula, data = mf, rhs = 2))
    } else {
      mm <- list(stats::model.matrix(Formula, data = mf, rhs = 1),
                 stats::model.matrix(Formula, data = mf, rhs = 1))
    }
    names(mm) <- lcoef
  } else {
    mm <- list(stats::model.matrix(Formula, data = mf, rhs = 1))
    names(mm) <- lcoef[1]
  }
  
  return(mm)  
}