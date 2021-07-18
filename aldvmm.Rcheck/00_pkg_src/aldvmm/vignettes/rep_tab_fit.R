rep_tab_fit <- function(fit) {
  
  tmp <- summary(fit)
  
  valindex <- apply(tmp, 1, function(x)
    sum("-" != strsplit(paste(x, collapse = ""), "")[[1]]) != 0
  )
  tmp <- tmp[valindex, ]
  
  colnames(tmp) <- tmp[1, ]
  tmp <- tmp[-1, ]
  #tmp[nrow(tmp), 3] <- ""
  #tmp[nrow(tmp), 4] <- ""
  
  eindex <- match("E[y|X, c]", tmp[, 1])
  pindex <- match("P[c|X]", tmp[, 1])
  
  lindex <- c(-1, eindex - 1, eindex, pindex - 1, pindex, nrow(tmp) - 1)
  
  list(table = tmp,
       lindex = lindex[!is.na(lindex)])
}
