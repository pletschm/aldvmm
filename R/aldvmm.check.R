#' Checking the Validity of Objects Supplied to
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{\code{aldvmm()}}
#'
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.check}}}{\code{aldvmm.check()}}
#' runs validity checks of objects supplied to
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{\code{aldvmm()}}.
#'
#' @inheritParams aldvmm
#' @inheritParams aldvmm.ll
#'
#' @details
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.check}}}{\code{aldvmm.check()}}
#' checks the validity of arguments of
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{\code{aldvmm()}}.
#'
#' @return \code{aldvmm.check} returns warnings or stops the execution of
#'   \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{\code{aldvmm()}} if validity
#'   checks fail.
#'
#' @export

aldvmm.check <- function(formula, 
                         data, 
                         psi, 
                         ncmp, 
                         dist,
                         optim.method,
                         optim.control,
                         optim.grad,
                         init.method, 
                         init.est,
                         init.lo,
                         init.hi,
                         se.fit,
                         model,
                         level,
                         na.action,
                         lcoef,
                         lcpar,
                         lcmp) {
  
  # Check format of input values
  #-----------------------------
  
  checkmate::assertFormula(formula)  
  checkmate::assertDataFrame(data)
  if (is.null(names(data))) {
    stop("'data' has no column names",
         "\n")
  }
  checkmate::assertVector(psi, strict = TRUE)
  checkmate::assertNumeric(psi)
  checkmate::assert(psi[1] != psi[2])
  checkmate::assert(length(psi) == 2)
  checkmate::assert(max(psi) <= 1)
  checkmate::assertCount(ncmp, positive = TRUE)
  checkmate::assertChoice(dist, c("normal"))
  if (!is.null(optim.method)) {
    checkmate::assert(optim.method %in% c("Nelder-Mead", "BFGS", "CG", 
                                          "L-BFGS-B", "nlminb", 
                                          "Rcgmin", "Rvmmin", "hjn"))
  }
  checkmate::assertList(optim.control)
  
  if (!is.null(optim.grad)) {
    checkmate::assertLogical(optim.grad)
  }
  
  checkmate::assertChoice(init.method, c("zero", "random", "constant", "sann"))
  
  if (!is.null(init.est)) {
    checkmate::assertVector(init.est, strict = TRUE)
    checkmate::assertNumeric(init.est)
  }
  
  if (!is.null(init.lo)) {
    checkmate::assertVector(init.lo, strict = TRUE)
    checkmate::assertNumeric(init.lo)
  }
  
  if (!is.null(init.hi)) {
    checkmate::assertVector(init.hi, strict = TRUE)
    checkmate::assertNumeric(init.hi)
  }
  
  
  checkmate::assertLogical(se.fit)
  checkmate::assertLogical(model)
  
  checkmate::assert(length(level) == 1)
  checkmate::assertNumeric(level)
  checkmate::assert(level > 0)
  checkmate::assert(level < 1)
  
  checkmate::assertChoice(na.action, c("na.pass", "na.omit", "na.fail", "na.exclude"))
  
  if (dist == "normal" & length(lcpar) != 1) {
    stop("'lcpar' is wrong length for ",
         '"',
         dist, 
         '" distribution.\n')
  }
  
  if (length(lcoef) != 2) {
    stop("The length of 'lcoef' is ",
         length(lcoef),
         " instead of 2.",
         "\n")
  }
  
  if (length(lcmp) != 1) {
    stop("The length of 'lcmp' is ",
         length(lcmp),
         " instead of 1.",
         "\n")
  }
  
  # Check if all variables in formula exist in data
  #------------------------------------------------
  
  # This check needs to occur before the use of other aldvmm functions
  
  if (sum(!(all.vars(formula) %in% names(data))) > 0) {
    stop("The variables ", 
         paste(all.vars(formula)[!(all.vars(formula) %in% names(data))], 
               collapse = ", "), 
         " from 'formula' do not exist in 'data'.",
         "\n")
  }
  
  # Count rows with missing values
  #-------------------------------
  
  complete <- stats::complete.cases(data[, all.vars(formula)])
  if (FALSE %in% complete) {
    message("the data includes ", 
            sum(complete == FALSE), 
            " rows with missing values\n")
  }
  
  # Check if user-defined initial values are the right length.
  #-----------------------------------------------------------
  
  mm <- aldvmm.mm(mf = stats::model.frame(Formula::Formula(formula), data = data),
                  Formula = Formula::Formula(formula),
                  ncmp = ncmp,
                  lcoef = lcoef)
  
  parnames <- aldvmm.getnames(X = mm,
                              names = c(lcoef, lcpar),
                              lcoef = lcoef,
                              lcpar = lcpar,
                              lcmp = lcmp,
                              ncmp = ncmp)
  
  if (!is.null(init.est)) {
    if (length(init.est) != length(parnames)) {
      stop("The length of 'init.est' is ",
           length(init.est),
           " but should be ",
           length(parnames),
           "\n")
    }
  }
  
  if (!is.null(init.lo)) {
    if (length(init.lo) != length(parnames)) {
      stop("The length of 'init.lo' is ",
           length(init.lo),
           " but should be ",
           length(parnames),
           "\n")
    }
  }
  
  if (!is.null(init.hi)) {
    if (length(init.hi) != length(parnames)) {
      stop("The length of 'init.hi' is ",
           length(init.hi),
           " but should be ",
           length(parnames),
           "\n")
    }
  }
  
  rm(mm, parnames)
  
  # Only one component but pipe separator in formula
  #-------------------------------------------------
  
  if (ncmp == 1 & grepl("\\|", as.character(formula)[[3]])) {
    message("'ncmp' is set to 1, ",
            "and the second part of the formula will be ignored",
            "\n")
  }
  
  # Check if model includes constants when init.method is set to "constant".
  #-------------------------------------------------------------------------
  
  mm <- aldvmm.mm(mf = stats::model.frame(Formula::Formula(formula), data = data),
                  Formula = Formula::Formula(formula),
                  ncmp = ncmp,
                  lcoef = lcoef)
  
  if (ncmp > 1) {
    checkcons <- unlist(lapply(lcoef, 
                               function(x) "(Intercept)" %in% 
                                 colnames(mm[[x]])))
  } else {
    checkcons <- "(Intercept)" %in% colnames(mm[[1]])
  }
  
  if (init.method == "constant" & FALSE %in% checkcons) {
    stop("At least one equation in 'formula' does not include a constant, ",
         "but 'init.method' is set to ",
         '"constant"',
         "\n")
  }
  
  rm(checkcons) 
  
  # Ensure the term "(Intercept)" is not used in column names of data
  #--------------------------------------------------------------------
  
  if (TRUE %in% grepl("(Intercept)", names(data))) {
    stop('"(Intercept)" is not allowed in column names of ',
         "'data'",
         "\n")
  }

  # Check if the data includes outcome values outside limits
  #---------------------------------------------------------
  
  outdat <- data[, as.character(formula)[[2]]]
  
  minobs <- min(outdat, na.rm = TRUE)
  gapobs <- max(outdat[outdat < 1], na.rm = TRUE)
  maxobs <- max(outdat, na.rm = TRUE)
  
  if (minobs < min(psi) | gapobs > max(psi) | maxobs > 1) {
    stop("Observed values of ",
         as.character(formula)[[2]],
         " include values outside the limits supplied to 'psi' or larger than 1",
         "\n")
  }
}
