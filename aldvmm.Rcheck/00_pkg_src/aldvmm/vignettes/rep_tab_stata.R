rep_tab_stata <- function(file) {
  
  stata <- read.table(file, 
                      header = FALSE, 
                      sep = ";", 
                      dec = ".")
  
  stata[1, 1] <- ""
  names(stata) <- stata[1, ]
  stata <- stata[-1, ]
  
  eindex <- match("E[y|c, X]", stata[, 1])
  pindex <- match("P[c|X]", stata[, 1])
  
  lindex <- c(-1, eindex - 1, eindex, pindex - 1, pindex, nrow(stata) - 1)
  
  list(table = stata,
       lindex = lindex[!is.na(lindex)])
}