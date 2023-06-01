#' Creating Summary Table
#'
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.sum}}}{ \code{aldvmm.sum()}}
#' creates a summary table of regression results.
#'
#' @param est a named numeric vector of point estimates.
#'
#' @param se a named numeric vector of standard errors of parameters returned
#'   by \ifelse{html}{\code{\link[aldvmm]{aldvmm.cv}}}{ \code{aldvmm.cv()}}.
#'
#' @param z a named numeric vector of standardized coefficients of parameters
#'   returned by \ifelse{html}{\code{\link[aldvmm]{aldvmm.cv}}}{
#'   \code{aldvmm.cv()}}.
#'
#' @param p a named numeric vector of p-values of parameters returned by
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm.cv}}}{ \code{aldvmm.cv()}}.
#'
#' @param lower a named numeric vector of 95\% lower limits of parameters
#'   returned by \ifelse{html}{\code{\link[aldvmm]{aldvmm.cv}}}{
#'   \code{aldvmm.cv()}}.
#'
#' @param upper a named numeric vector of 95\% upper limits of parameters
#'   returned by \ifelse{html}{\code{\link[aldvmm]{aldvmm.cv}}}{
#'   \code{aldvmm.cv()}}.
#'
#' @param value a numeric value of the negative log-likelihood returned by
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm.ll}}}{ \code{aldvmm.ll()}}.
#'
#' @param aic a numeric value of the Akaike information criterion (AIC)
#'   returned by \ifelse{html}{\code{\link[aldvmm]{aldvmm.gof}}}{
#'   \code{aldvmm.gof()}}.
#'
#' @param bic a numeric value of the Bayesian information criterion (BIC)
#'   returned by \ifelse{html}{\code{\link[aldvmm]{aldvmm.gof}}}{
#'   \code{aldvmm.gof()}}.
#'
#' @param lvar a named list of character vectors with column names of design
#'   matrices returned by \ifelse{html}{\code{\link[aldvmm]{aldvmm.mm}}}{
#'   \code{aldvmm.mm()}}.
#'
#' @param n a numeric value of the number of complete observations in
#'   \code{'data'} supplied to
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{\code{aldvmm()}}.
#'
#' @param digits a numeric value of the number of digits in the reporting
#'   table.
#'
#' @inheritParams aldvmm.ll
#'
#' @return a \code{data.frame} object with a summary table of regression results.
#'
#' @export

aldvmm.sum <- function(est,
                       se,
                       z,
                       p,
                       lower,
                       upper,
                       n,
                       ncmp,
                       lcoef,
                       lcpar,
                       lcmp,
                       lvar,
                       digits = 3) {
  
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
  
  # Round results
  #--------------
  
  for (i in 1:length(parlist)) {
    for (j in 1:length(parlist[[i]])) {
      for (k in 1:length(parlist[[i]][[j]])) {
        val <- parlist[[i]][[j]][[k]]
        parlist[[i]][[j]][[k]] <- format(round(as.numeric(val),
                                               digits = digits), 
                                         nsmall = digits)
      }
    }
  }
  
  # Make matrices of statistics by component and type of parameters
  #----------------------------------------------------------------
  
  tmp <- list()
  
  # Betas (coefficients on distribution parameters)
  for (i in paste0(lcmp, 1:ncmp)) {
    tmp[[i]][[lcoef[1]]] <- cbind(c(i, rep("", length(lvar[[lcoef[1]]]) - 1)),
                                  lvar[[lcoef[1]]],
                                  parlist[["est"]][[lcoef[1]]][[i]],
                                  parlist[["se"]][[lcoef[1]]][[i]],
                                  parlist[["z"]][[lcoef[1]]][[i]],
                                  parlist[["p"]][[lcoef[1]]][[i]],
                                  parlist[["lower"]][[lcoef[1]]][[i]],
                                  parlist[["upper"]][[lcoef[1]]][[i]])
  }
  
  # Deltas (coefficients for multinomial logit for group membership)
  if (ncmp > 1) {
    for (i in paste0(lcmp, 1:(ncmp - 1))) {
      tmp[[i]][[lcoef[2]]] <- cbind(c(i,rep("", length(lvar[[lcoef[2]]]) - 1)),
                                    lvar[[lcoef[2]]],
                                    parlist[["est"]][[lcoef[2]]][[i]],
                                    parlist[["se"]][[lcoef[2]]][[i]],
                                    parlist[["z"]][[lcoef[2]]][[i]],
                                    parlist[["p"]][[lcoef[2]]][[i]],
                                    parlist[["lower"]][[lcoef[2]]][[i]],
                                    parlist[["upper"]][[lcoef[2]]][[i]])
    }
  }
  
  # Constant distribution parameters
  for (i in paste0(lcmp, 1:ncmp)) {
    for (j in lcpar) {
      tmp[[i]][[j]] <- cbind("",
                             paste0(j),
                             parlist[["est"]][[j]][[i]],
                             parlist[["se"]][[j]][[i]],
                             parlist[["z"]][[j]][[i]],
                             parlist[["p"]][[j]][[i]],
                             parlist[["lower"]][[j]][[i]],
                             parlist[["upper"]][[j]][[i]])
    }
  }
  
  # Make list of elements of reporting table
  #-----------------------------------------
  
  nc <- ncol(tmp[[i]][[lcoef[1]]])
  lines <- rep("-", times = nc)
  
  reptab <- list()
  
  reptab[["head0"]] <- rbind(lines,
                             c("", "", "Estimate", "Std. Err.", "z", "P>|z|", 
                               "[95% Conf. ", "Interval]"))
  
  reptab[["head1"]] <- rbind(lines,
                             c('E[y|X, c]', rep("", times = nc - 1)),
                             lines)
  
  for (c in paste0(lcmp, 1:ncmp)) {
    reptab[[paste(c, lcoef[1], sep = "_")]] <- tmp[[c]][[lcoef[1]]]
    for (j in lcpar) {
      reptab[[paste(c, j, sep = "_")]] <- tmp[[c]][[j]]
    }
  }
  
  if (ncmp > 1) {
    reptab[["head2"]] <- rbind(lines,
                               c('P[c|X]',    rep("", times = nc - 1)),
                               lines)
    
    reptab[[lcoef[2]]] <- do.call("rbind", 
                                         lapply(tmp, function(k) k[[lcoef[2]]]))
  }
  
  reptab[["end"]] <- lines
  
  # Convert list to data.frame
  #---------------------------
  
  reptab <- do.call("rbind", reptab)
  
  # Expand lines to column widths
  #------------------------------
  
  width <- apply(apply(reptab, 2, nchar), 2, max)
  
  for (i in seq_len(nrow(reptab))) {
    for (j in seq_len(ncol(reptab))) {
      if (reptab[i, j] == lines[1]) {
        reptab[i, j] <- paste0(rep(reptab[i, j],times = width[j]), 
                               collapse = "")
      }
    }
  }
  
  # Remove row and column names
  #----------------------------
  
  reptab <- as.data.frame(reptab, stringsAsFactors = FALSE)
  names(reptab) <- NULL
  rownames(reptab) <- NULL
  
  return(reptab)
  
}
