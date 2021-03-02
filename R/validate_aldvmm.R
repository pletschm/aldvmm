validate_aldvmm <- function(object) {
  
  # Vectors
  #--------
  
  names(object)
  vecnames <- c("coef", "se", "z", "p", "lower", "upper", "psi")
  novec <- unlist(lapply(vecnames, function(x) !is.vector(object[[x]])))
  novec <- c(novec, !is.vector(unlist(object[["label"]])))
  if (sum(novec) != 0) {
    stop('Some elements in object of class "aldvmm" are not of type "vector": ',
         paste(vecnames[novec], collapse = ", "),
         "\n")
  }

  # Lists
  #------
  
  listnames <- c("gof", "pred", "init", "label")
  nolist <- unlist(lapply(listnames, function(x) !is.list(object[[x]])))
  if (sum(nolist) != 0) {
    stop('Some elements in object of class "aldvmm" are not of type "list": ',
         paste(listnames[nolist], collapse = ", "),
         "\n")
    }

  # Matrices
  #---------
  
  matnames <- c("hessian", "cov")
  nomat <- unlist(lapply(matnames, function(x) !is.matrix(object[[x]])))
  nomat <- c(nomat, 
             unlist((lapply(object[["pred"]][c("yhat", "y", "res")], 
                                function(x) !is.matrix(x)))))
  
  if (sum(nomat) != 0) {
    stop('Some elements in object of class "aldvmm" are not of type "matrix": ',
         paste(matnames[nomat], collapse = ", "),
         "\n")
  }

  # Numeric objects
  #----------------
  
  numnames <- c("coef", "se", "z", "p", "lower", "upper", "hessian", "cov",
                "n", "k", "psi")
  nonum <- unlist(lapply(numnames, function(x) !is.numeric(object[[x]])))
  nonum <- c(nonum, 
             unlist((lapply(object[["pred"]][c("yhat", "y", "res")], 
                            function(x) !is.numeric(x)))))
  
  
  if (sum(nonum) != 0) {
    stop('Some elements in object of class "aldvmm" are not "numeric": ',
         paste(numnames[nonum], collapse = ", "),
         "\n")
  }

  # Character objects
  #------------------
  
  chrnames <- c("dist", "optim.method")
  nochr <- unlist(lapply(chrnames, function(x) !is.character(object[[x]])))
  nochr <- c(nochr, 
             unlist((lapply(unlist(object[["label"]]), 
                            function(x) !is.character(x)))))
  if (sum(nochr) != 0) {
    stop('Some elements in object of class "aldvmm" are not "character": ',
         paste(chrnames[nochr], collapse = ", "),
         "\n")
  }
  
  # Formula objects
  #----------------
  
  if (class(object$formula) != "formula") {
    stop("'formula' ",
         'in object of class "aldvmm" is not of type "formula"',
         "\n")
  }
  
  # Length of vectors
  #------------------
  
  veclen <- unlist(lapply(object[c("coef", "se", "z", "p", "lower", "upper")],
                function(x) length(x)))
  veclen <- c(veclen, unlist(lapply(object[c("hessian", "cov")],
                   function(x) dim(x)[1])))
  if (sum(veclen != veclen[1]) != 0) {
    stop('Some elements in object of class "aldvmm" are of wrong length: ',
         paste(veclen[veclen != veclen[1]], collapse = ", "),
         "\n")
  }
  
}