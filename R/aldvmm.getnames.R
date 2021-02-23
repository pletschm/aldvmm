#' Creating Names of Parameter Vectors
#'
#' @description
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm.getnames}}}{\code{aldvmm::aldvmm.getnames()}}
#'   creates names of parameter vectors used in
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{\code{aldvmm::aldvmm()}}. The
#'   order of the elements in \code{'lcoef'} and \code{'lcpar'} determines the
#'   order of parameters and the structure of summary tables returned by
#'   \ifelse{html}{\code{\link[aldvmm]{summary.aldvmm}}}{\code{aldvmm::summary.aldvmm()}}.
#'
#'
#' @inheritParams aldvmm.ll
#' @param names a character vector of names of considered elements (distributions
#'   \code{"beta"}, multinomial logit \code{"delta"} or constant distribution
#'   parameters, i.e. \code{"lnsigma"}). The elements in \code{'names'} are combined with covariate names in \code{'X'} and component labels in \code{'lcmp'} to create a vector of names of parameter vectors.
#'
#' @return a character vector of names of parameter vectors used in
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{\code{aldvmm::aldvmm()}}.
#'
#' @export

aldvmm.getnames <- function(X,
                            names,
                            lcoef,
                            lcpar,
                            lcmp,
                            ncmp) {
  
  # Betas (coefficients on distribution parameters)
  #------------------------------------------------
  
  if (lcoef[1] %in% names) {
    name.beta <- c(paste(paste0(lcmp, 
                                rep(1:ncmp, each = dim(X[[lcoef[1]]])[2])),
                         rep(lcoef[1], 
                             times = ncmp, 
                             each  = dim(X[[lcoef[1]]])[2]),
                         rep(colnames(X[[lcoef[1]]]), 
                             times = ncmp),
                         sep = "_"))
  } else {
    name.beta <- NULL
  }

  # Deltas (coefficients for multinomial logit for group membership)
  #-----------------------------------------------------------------
  
  if (ncmp>1 & lcoef[2] %in% names) {
    name.delta <- c(paste(paste0(lcmp, 
                                 rep(1:(ncmp - 1), 
                                     each = dim(X[[lcoef[2]]])[2])),
                          rep(lcoef[2], 
                              times = (ncmp - 1), 
                              each  = dim(X[[lcoef[2]]])[2]),
                          rep(colnames(X[[lcoef[2]]]), 
                              times = ncmp - 1),
                          sep = "_"))
  } else {
    name.delta <- NULL
  }
  
  # Constant distribution parameters
  #---------------------------------
  
  if (sum(lcpar %in% names)>0){
    name.cpar <- c()
    for (i in lcpar){
      name.cpar <- c(name.cpar, paste(paste0(lcmp, 1:ncmp),
                                      rep(lcpar, times = ncmp),
                                      sep = "_"))
      
    }
  } else {
    name.cpar <- NULL
  }
  
  return(c(name.beta, name.delta, name.cpar))
  
}
