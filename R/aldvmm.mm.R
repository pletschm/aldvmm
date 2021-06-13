#' Creating Design Matrices
#'
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.mm}}}{\code{aldvmm.mm()}} creates
#' two design matrices, one of the model of component distributions
#' (\code{"beta"}) and one of the model of probabilities of component
#' membership (\code{"delta"}).
#'
#' @inheritParams aldvmm
#' @inheritParams aldvmm.ll
#'
#' @details \ifelse{html}{\code{\link[aldvmm]{aldvmm.mm}}}{\code{aldvmm.mm()}}
#'   uses
#'   \ifelse{html}{\code{\link[stats]{model.matrix}}}{\code{stats::model.matrix()}}
#'    to create design matrices for models of component distributions
#'   (\code{"beta"}) and probabilities of component membership (\code{"delta"})
#'   based on \code{'formula'} supplied to
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm.ll}}}{\code{aldvmm.ll()}}. The
#'   design matrix for probabilities of group membership is only created if
#'   more than one components are specified  in \code{'ncmp'}.
#'
#' @return a named list of numeric matrices. \item{\code{beta}}{a numeric
#'   design matrix for the model of component distributions.}
#'   \item{\code{delta}}{a numeric design matrix of the multinomial logit model
#'   of probabilities of component membership.}
#'
#' @export

aldvmm.mm <- function(data,
                      formula,
                      ncmp,
                      lcoef) {
  
  # Remove rows with missing values
  #--------------------------------
  
  complete <- stats::complete.cases(data[, all.vars(formula)])
  if (FALSE %in% complete) {
    data <- data[complete, ]
  }
  
  # List of formulae for distributions and multinomial logit
  #---------------------------------------------------------
  
  # If no pipe (|) delimiter is used in the formula, the same formula will be
  # used for distributions and the multinomial logit parts.
  
  if (grepl("\\|", as.character(formula)[3])) {
    formvec <- paste0(as.character(formula)[2], 
                      " ~ ", 
                      unlist(strsplit(as.character(formula)[3], 
                                      split = "\\|")))
    names(formvec) <- lcoef
    formlist <- lapply(formvec, function(x) stats::as.formula(x))
  } else {
    formlist <- lapply(lcoef, function(x) formula)
    names(formlist) <- lcoef
  }
  
  # List of Model matrices
  #-----------------------
  
  # Make list of model matrices for beta and delta
  if (ncmp > 1) {
    mm <- lapply(formlist, function(x) stats::model.matrix(x, data))
  } else {
    mm <- list(stats::model.matrix(formlist[[1]], data))
    names(mm) <- lcoef[1]
  }
  
  return(mm)  
}
