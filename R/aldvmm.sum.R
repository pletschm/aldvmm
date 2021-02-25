#' Creating Summary Table
#'
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.sum}}}{
#' \code{aldvmm::aldvmm.sum()}} creates a summary table of regression results.
#'
#' @param est a named numeric vector of parameters with names returned by
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm.getnames}}}{
#'   \code{aldvmm::aldvmm.getnames()}}.
#'
#' @param se a named numeric vector of standard errors of parameters returned
#'   by \ifelse{html}{\code{\link[aldvmm]{aldvmm.cv}}}{
#'   \code{aldvmm::aldvmm.cv()}}.
#'
#' @param z a named numeric vector of z scores (standardized coefficients) of
#'   parameters returned by \ifelse{html}{\code{\link[aldvmm]{aldvmm.cv}}}{
#'   \code{aldvmm::aldvmm.cv()}}.
#'
#' @param p a named numeric vector of p-values of parameters returned by
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm.cv}}}{
#'   \code{aldvmm::aldvmm.cv()}}.
#'
#' @param lower a named numeric vector of 95\% lower limits of parameters
#'   returned by \ifelse{html}{\code{\link[aldvmm]{aldvmm.cv}}}{
#'   \code{aldvmm::aldvmm.cv()}}.
#'
#' @param upper a named numeric vector of 95\% upper limits of parameters
#'   returned by \ifelse{html}{\code{\link[aldvmm]{aldvmm.cv}}}{
#'   \code{aldvmm::aldvmm.cv()}}.
#'
#' @param value a numeric value of the negative log-likelihood returned by
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm.ll}}}{
#'   \code{aldvmm::aldvmm.ll()}}.
#'
#' @param aic a numeric value of the Akaike information criterion (AIC)
#'   returned by \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{
#'   \code{aldvmm::aldvmm()}}.
#'
#' @param bic a numeric value of the Bayesian information criterion (BIC)
#'   returned by \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{
#'   \code{aldvmm::aldvmm()}}.
#'
#' @param lvar a named list of character vectors with column names of design
#'   matrices returned by \ifelse{html}{\code{\link[aldvmm]{aldvmm.mm}}}{
#'   \code{aldvmm::aldvmm.mm()}}.
#'
#' @param n a numeric value of the number of complete observations in
#'   \code{'data'} supplied to
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{\code{aldvmm::aldvmm()}}.
#'
#' @inheritParams aldvmm.ll
#'
#' @return a data frame with a summary table of regression results.
#'
#' @export

aldvmm.sum <- function(est,
                       se,
                       z,
                       p,
                       lower,
                       upper,
                       n,
                       value,
                       aic,
                       bic,
                       ncmp,
                       lcoef,
                       lcpar,
                       lcmp,
                       lvar) {
  
  options(scipen = 999)
  
  # Extract parameters to list
  #---------------------------
  
  lstat <- c("est", "se", "z", "p", "lower", "upper")
  parlist <- lapply(lstat,
                    function(x) aldvmm.getpar(par   = eval(parse(text = x)),
                                              lcoef = lcoef,
                                              lcmp  = lcmp,
                                              lcpar = lcpar,
                                              ncmp  = ncmp))
  names(parlist) <- lstat
  
  # Make matrices of statistics by component and {beta, delta, constant}
  #---------------------------------------------------------------------
  
  tmp <- list()
  
  # Betas (coefficients on distribution parameters)
  for (i in paste0(lcmp, 1:ncmp)) {
    tmp[[i]][[lcoef[1]]] <- cbind(parlist[["est"]][[lcoef[1]]][[i]],
                                  parlist[["se"]][[lcoef[1]]][[i]],
                                  parlist[["z"]][[lcoef[1]]][[i]],
                                  parlist[["p"]][[lcoef[1]]][[i]],
                                  parlist[["lower"]][[lcoef[1]]][[i]],
                                  parlist[["upper"]][[lcoef[1]]][[i]])
    rownames(tmp[[i]][[lcoef[1]]]) <- lvar[[lcoef[1]]]
  }
  
  # Deltas (coefficients for multinomial logit for group membership)
  if (ncmp > 1) {
    for (i in paste0(lcmp, 1:(ncmp - 1))) {
      tmp[[i]][[lcoef[2]]] <- cbind(parlist[["est"]][[lcoef[2]]][[i]],
                                    parlist[["se"]][[lcoef[2]]][[i]],
                                    parlist[["z"]][[lcoef[2]]][[i]],
                                    parlist[["p"]][[lcoef[2]]][[i]],
                                    parlist[["lower"]][[lcoef[2]]][[i]],
                                    parlist[["upper"]][[lcoef[2]]][[i]])
      rownames(tmp[[i]][[lcoef[2]]]) <- lvar[[lcoef[2]]]
    }
  }
  
  # Constant distribution parameters
  for (i in paste0(lcmp, 1:ncmp)) {
    for (j in lcpar) {
      tmp[[i]][[j]] <- cbind(parlist[["est"]][[j]][[i]],
                             parlist[["se"]][[j]][[i]],
                             parlist[["z"]][[j]][[i]],
                             parlist[["p"]][[j]][[i]],
                             parlist[["lower"]][[j]][[i]],
                             parlist[["upper"]][[j]][[i]])
      rownames(tmp[[i]][[j]]) <- j
    }
  }
  
  # Make list of data frames by component
  #--------------------------------------
  
  nc <- ncol(tmp[[i]][[lcoef[1]]]) + 2
  rn <- lapply(tmp[[1]], function(x) rownames(x))
  
  lines <- rep("-", times = nc)
  
  reptab <- list()
  
  # Distribution parameters (modeled and constant)
  for (i in paste0(lcmp, 1:ncmp)) {
    cparmat <- lapply(lcpar, function(x) cbind("", 
                                               rownames(tmp[[i]][[x]]), 
                                               format(round(tmp[[i]][[x]], 4),
                                                      nsmall = 4) ))
    
    mat <- rbind(cbind(c(i, rep("", times = length(rn[[lcoef[1]]]) - 1)), 
                       rn[[lcoef[1]]], 
                       format(round(tmp[[i]][[lcoef[1]]], 4),
                              nsmall = 4) ),
                 do.call("rbind", cparmat))
    
    reptab[[i]] <- as.data.frame(mat, check.names = FALSE) 
  }
  
  # Multinomial logit parameters
  if (ncmp==2) {
    mlres <- list()
    i <- paste0(lcmp, 1)
    mlres[[i]] <- rbind(lines,
                        c('P[c|X]', rep("", times = nc - 1)),
                        lines,
                        cbind(c(i, rep("", 
                                       times = length(rn[[lcoef[2]]]) - 1)), 
                              rn[[lcoef[2]]], 
                              format(round(tmp[[i]][[lcoef[2]]], 4),
                                     nsmall = 4)))
    
  } else if (ncmp > 2) {
    mlres <- list()
    i <- paste0(lcmp, 1)
    mlres[[i]] <- rbind(lines,
                        c('P[c|X]', rep("", times = nc - 1)),
                        lines,
                        cbind(c(i, rep("", 
                                       times = length(rn[[lcoef[2]]]) - 1)), 
                              rn[[lcoef[2]]], 
                              format(round(tmp[[i]][[lcoef[2]]], 4),
                                     nsmall = 4)))
    
    for (i in paste0(lcmp, 2:(ncmp - 1))) {
      mlres[[i]] <- cbind(c(i, rep("", times = length(rn[[lcoef[2]]]) - 1)), 
                          rn[[lcoef[2]]], 
                          format(round(tmp[[i]][[lcoef[2]]], 4),
                                 nsmall = 4))
    }
  }
  
  if (ncmp>1) {
    reptab[[lcoef[[2]]]] <- as.data.frame(do.call('rbind', mlres))
  }
  
  # Combine outputs from different components
  #------------------------------------------
  
  # Make data frame
  outdat <- do.call("rbind", reptab)
  outdat <- rbind(lines,
                  c("", "", "Estimate", "Std. Err.", "z", "P>|z|", 
                    "[95% Conf. ", "Interval]"),
                  lines,
                  c("E[y|c, X]", rep("", times = nc - 1)),
                  lines,
                  outdat,
                  lines,
                  c(paste0("N = ", n), 
                    paste0("ll = ", format(round(-value, digits = 2), 
                                           nsmall = 2)),
                    paste0("AIC = ", format(round(aic, digits = 2), 
                                            nsmall = 2)),
                    paste0("BIC = ", format(round(bic, digits = 2), 
                                            nsmall = 2)),
                    rep("", times = nc - 4)))
  
  # Expand lines to column widths
  tmpdat <- outdat
  tmpdat[is.na(tmpdat)] <- "NA"
  width <- apply(apply(tmpdat, 2, nchar), 2, max)
  rm(tmpdat)
  
  for (i in seq_len(nrow(outdat))) {
    for (j in seq_along(outdat)) {
      outdat[i, j] <- ifelse(outdat[i, j] %in% c("-", "="), 
                             paste0(rep(outdat[i, j], times = width[j]), 
                                    collapse = ""), 
                             outdat[i, j])
    }
  }
  
  # Remove row and column names
  names(outdat) <- NULL
  rownames(outdat) <- NULL
  
  return(outdat)
  
}
