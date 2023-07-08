#' Creating Terms Objects
#'
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.tm}}}{\code{aldvmm.tm()}} creates
#' a list of up to three "terms" objects, one of the model of component distributions
#' (\code{"beta"}), one of the model of probabilities of component
#' membership (\code{"delta"}) and one for the full model (\code{"full"}).
#' 
#' @param mf a data frame created by 
#' \ifelse{html}{\code{\link[stats]{model.frame}}}{\code{stats::model.frame}} 
#' including the variables used in \code{'formula'} supplied to 
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{\code{aldvmm()}} plus 
#' additional attributes derived from \code{'formula'}.
#' @param Formula an object of class "Formula" created by 
#' \ifelse{html}{\code{\link[Formula]{Formula}}}{\code{Formula::Formula}} 
#' based on the \code{'formula'} supplied to 
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{\code{aldvmm()}}.
#' @inheritParams aldvmm
#' @inheritParams aldvmm.ll
#'
#' @details \ifelse{html}{\code{\link[aldvmm]{aldvmm.tm}}}{\code{aldvmm.tm()}}
#'   uses
#'   \ifelse{html}{\code{\link[stats]{terms}}}{\code{stats::terms()}}
#'    to create "terms" objects based on a "Formula" object 
#'    created by 
#'    \ifelse{html}{\code{\link[Formula]{Formula}}}{\code{Formula::Formula}} 
#'    and a model frame created by 
#'    \ifelse{html}{\code{\link[stats]{model.frame}}}{\code{stats::model.frame}}. 
#'    The "terms" object for probabilities of group membership is only created if
#'   more than one components are specified in \code{'ncmp'}.
#'
#' @return a named list of objects of class "terms". 
#' \item{\code{beta}}{a "terms" object for the model of component 
#' distributions.}
#'   \item{\code{delta}}{a "terms" object of the multinomial logit model
#'   of probabilities of component membership.}
#'   \item{\code{full}}{a "terms" object of the full model.}
#'
#' @export

aldvmm.tm <- function(mf,
                      Formula,
                      ncmp,
                      lcoef) {
  
  if (ncmp > 1) {
    if (length(Formula)[2] > 1) {
      terms <- list(stats::delete.response(stats::terms(Formula, data = mf, rhs = 1)),
                    stats::delete.response(stats::terms(Formula, data = mf, rhs = 2)),
                    stats::terms(Formula, data = mf))
    } else {
      terms <- list(stats::delete.response(stats::terms(Formula, data = mf, rhs = 1)),
                    stats::delete.response(stats::terms(Formula, data = mf, rhs = 1)),
                    stats::terms(Formula, data = mf))
    }
    names(terms) <- c(lcoef, "full")
  } else {
    terms <- list(stats::delete.response(stats::terms(Formula, data = mf, rhs = 1)),
                  stats::terms(Formula, data = mf))
    names(terms) <- c(lcoef[1], "full")
  }
  return(terms)  
}