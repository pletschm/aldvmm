#' Creating Summary Table
#'
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.sum}}}{ \code{aldvmm.sum()}}
#' creates a summary table of regression results.
#'
#' @param object an \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{
#'   \code{aldvmm()}} model fit object of class "aldvmm".
#' @param digits a numeric value of the number of digits in the reporting
#'   table.
#' @param level a numeric value of the confidence level.
#'
#' @return a \code{data.frame} object with a summary table of regression results.
#'
#' @export

aldvmm.sum <- function(object,
                       digits = max(3L, getOption("digits") - 3L),
                       level = 0.95) {
  
  ncmp   <- object$ncmp
  lcoef  <- object$label$lcoef
  lcmp   <- object$label$lcmp
  lcpar  <- object$label$lcpar
  lvar   <- object$label$lvar
  
  # Calculate test statistics
  #---------------------------
  
  mat <- cbind(lmtest::coeftest(object), 
               lmtest::coefci(object, level = level))
  mat <- round(mat, digits)
  
  # Extract parameters to list
  #---------------------------
  
  parlist <- lapply(colnames(mat),
                    function(x) {
                      aldvmm.getpar(par   = mat[, x],
                                    lcoef = lcoef,
                                    lcmp  = lcmp,
                                    lcpar = lcpar,
                                    ncmp  = ncmp)
                    })
  
  names(parlist) <- colnames(mat)
  
  # Make matrices of statistics by component and type of parameters
  #----------------------------------------------------------------
  
  tmp <- list()
  
  # Betas (coefficients on distribution parameters)
  for (i in paste0(lcmp, 1:ncmp)) {
    tmp[[i]][[lcoef[1]]] <- cbind(c(i, rep("", length(lvar[[lcoef[1]]]) - 1)),
                                  lvar[[lcoef[1]]],
                                  parlist[[1]][[lcoef[1]]][[i]],
                                  parlist[[2]][[lcoef[1]]][[i]],
                                  parlist[[3]][[lcoef[1]]][[i]],
                                  parlist[[4]][[lcoef[1]]][[i]],
                                  parlist[[5]][[lcoef[1]]][[i]],
                                  parlist[[6]][[lcoef[1]]][[i]])
  }
  
  # Deltas (coefficients for multinomial logit for group membership)
  if (ncmp > 1) {
    for (i in paste0(lcmp, 1:(ncmp - 1))) {
      tmp[[i]][[lcoef[2]]] <- cbind(c(i,rep("", length(lvar[[lcoef[2]]]) - 1)),
                                    lvar[[lcoef[2]]],
                                    parlist[[1]][[lcoef[2]]][[i]],
                                    parlist[[2]][[lcoef[2]]][[i]],
                                    parlist[[3]][[lcoef[2]]][[i]],
                                    parlist[[4]][[lcoef[2]]][[i]],
                                    parlist[[5]][[lcoef[2]]][[i]],
                                    parlist[[6]][[lcoef[2]]][[i]])
    }
  }
  
  # Constant distribution parameters
  for (i in paste0(lcmp, 1:ncmp)) {
    for (j in lcpar) {
      tmp[[i]][[j]] <- cbind("",
                             paste0(j),
                             parlist[[1]][[j]][[i]],
                             parlist[[2]][[j]][[i]],
                             parlist[[3]][[j]][[i]],
                             parlist[[4]][[j]][[i]],
                             parlist[[5]][[j]][[i]],
                             parlist[[6]][[j]][[i]])
    }
  }
  
  # Make list of elements of reporting table
  #-----------------------------------------
  
  nc <- ncol(tmp[[i]][[lcoef[1]]])
  lines <- rep("-", times = nc)
  
  reptab <- list()
  
  reptab[["head0"]] <- rbind(lines,
                             c("", "", colnames(mat)))
  
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
