#' Numerical Approximation of Covariance Matrix
#'
#' @description
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.cv}}}{\code{aldvmm.cv()}} performs
#' a numerical approximation of the covariance matrix of parameter estimates.
#'
#' @inheritParams aldvmm.ll
#' @inheritParams aldvmm
#'
#' @param ll a function returning the negative log-likelihood of the adjusted
#'   limited dependent variable mixture model as a scalar result
#'   (\ifelse{html}{\code{\link[aldvmm]{aldvmm.ll}}}{\code{aldvmm.ll()}}).
#'
#' @details \ifelse{html}{\code{\link[aldvmm]{aldvmm.cv}}}{\code{aldvmm.cv()}}
#'   uses
#'   \ifelse{html}{\code{\link[numDeriv]{hessian}}}{\code{numDeriv::hessian()}}
#'   to calculate the hessian matrix of the log-likelihood function supplied to
#'   \code{'ll'} at parameter values supplied to \code{'par'}.
#'
#'   In case the Hessian matrix is near-singular, and
#'   \ifelse{base}{\code{\link[base]{solve}}}{\code{solve()}} is an \code{NA}
#'   matrix, \ifelse{html}{\code{\link[aldvmm]{aldvmm.cv}}}{\code{aldvmm.cv()}}
#'   falls back to Moore-Penrose regularization (Penrose, 1955). Moore-Penrose
#'   regularization is implemented by singular value decomposition using
#'   \ifelse{base}{\code{\link[base]{svd}}}{\code{svd()}}. If the Moore-Penrose
#'   regularization also fails, an \code{NA} matrix is returned.
#'
#' @return \code{aldvmm.cv} returns a list with the following objects.
#'   \item{\code{hessian}}{a numeric matrix with second-order partial
#'   derivatives of the likelihood function \code{'ll'}.} \item{\code{cv}}{a
#'   numeric matrix with covariances/variances of parameters in \code{'par'}.}
#'   \item{\code{se}}{a numeric vector of standard errors of parameters in
#'   \code{'par'}.} \item{\code{z}}{a numeric vector of z-values of parameters
#'   in \code{'par'}.} \item{\code{p}}{a numeric vector of p-values of
#'   parameter estimates.} \item{\code{upper}}{a numeric vector of upper 95\%
#'   confidence limits of parameter estimates in \code{'par'}.}
#'   \item{\code{lower}}{a numeric vector of lower 95\% confidence limits of
#'   parameter estimates in \code{'par'}.}
#'
#' @references Penrose, R. (1955, July). A generalized inverse for matrices. In
#'   Mathematical proceedings of the Cambridge philosophical society (Vol. 51,
#'   No. 3, pp. 406-413). Cambridge University Press.
#'
#' @export

aldvmm.cv <- function(ll,
                      par,
                      X,
                      y,
                      dist,
                      psi,
                      ncmp,
                      lcoef,
                      lcpar,
                      lcmp,
                      optim.method) {
  
  outlist <- list()
  
  # Hessian
  #--------
  
  outlist[["hessian"]] <- numDeriv::hessian(func = ll,
                                            x = par,
                                            X = X,
                                            y = y,
                                            psi = psi,
                                            ncmp = ncmp,
                                            dist = dist,
                                            lcoef = lcoef,
                                            lcmp = lcmp,
                                            lcpar = lcpar,
                                            optim.method = optim.method)
  
  rownames(outlist[["hessian"]]) <- names(par)
  colnames(outlist[["hessian"]]) <- names(par)
  
  # Covariance matrix
  #------------------
  
  # Covariance matrix
  #------------------
  
  outlist[["cv"]] <- tryCatch({
    
    cv <- solve(outlist[["hessian"]])
    
    if (any(diag(cv) < 0, na.rm = TRUE)) {
      stop("Inverted Hessian has negative diagonal entries (non-PD).")
    }
    
    cv
    
  }, error = function(e) {
    
    message("solve(Hessian) failed: ", conditionMessage(e),
            "\nFalling back to Moore-Penrose pseudo-inverse via singular value decomposition. ",
            "\nStandard errors for non-identified parameters will be NA.")
    
    sv  <- tryCatch(svd(outlist[["hessian"]]), error = function(e2) NULL)
    
    if (is.null(sv)) {
      
      # Singular value decomposition also failed — last resort: all-NA matrix.
      message("Singular value decomposition also failed. Returning all-NA covariance matrix.")
      matrix(data = NA,
             nrow = nrow(outlist[["hessian"]]),
             ncol = ncol(outlist[["hessian"]]))
      
    } else {
      
      # Singular value decomposition succeeded.
      # Compute pseudo-inverse, masking near-zero singular
      # values so that non-identified parameters get NA standard errors.
      tol <- max(dim(outlist[["hessian"]])) * .Machine$double.eps * max(abs(sv$d))
      rank_deficient_idx <- which(abs(sv$d) < tol)
      
      d_inv <- 1 / sv$d
      d_inv[rank_deficient_idx] <- 0
      
      cv_mp <- sv$v %*% diag(d_inv) %*% t(sv$u)
      cv_mp[rank_deficient_idx, ] <- NA
      cv_mp[, rank_deficient_idx] <- NA
      cv_mp
      
    }
  })
  
  rownames(outlist[["cv"]]) <- names(par)
  colnames(outlist[["cv"]]) <- names(par)
  
  # Warnings
  #---------

  if (all(is.na(outlist[["cv"]]))) {

    base::warning("no covariance matrix is obtained\n",
                  call. = FALSE)
    outlist[["se"]] <- rep(NA, times = length(par))

  } else {

    suppressWarnings(
      outlist[["se"]] <- sqrt(diag(outlist[["cv"]]))
    )

    if (all(is.na(diag(outlist[["cv"]])))) {
      base::warning("covariance matrix includes only missing diagonals\n",
                    call. = FALSE)
    } else if (any(is.na(diag(outlist[["cv"]])))) {
      base::warning("covariance matrix includes missing diagonals\n",
                    call. = FALSE)
    }

    if (any(diag(outlist[["cv"]])[!is.na(diag(outlist[["cv"]]))] <= 0)) {
      base::warning("covariance matrix includes non-positive diagnoals\n",
                    call. = FALSE)
    }

  }

  return(outlist)
}
