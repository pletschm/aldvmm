#' Calculating goodness of fit measures
#'
#' \ifelse{html}{\code{\link[aldvmm]{aldvmm.gof}}}{\code{aldvmm::aldvmm.gof()}}
#' calculates residual- and likelihood-based goodness of fit measures.
#'
#' @param res a numeric vector of residuals of all observations in the
#'   estimation data.
#' @param ll a numeric value of the log-likelihood.
#' @param par a named numeric vector of parameter estimates.
#'
#' @details \ifelse{html}{\code{\link[aldvmm]{aldvmm.gof}}}{\code{aldvmm::aldvmm.gof()}}
#' calculates mean squared errors as \eqn{MSE = \frac{\sum_{i = 1}^{n} (y_{i} - \hat{y}_{i})^2}{n - k}}{(sum(y - yhat)^2)/(n - k)}, and mean absolute errors as \eqn{MAE = \frac{\sum_{i = 1}^{n} y_{i} - \hat{y}_{i}}{n - k}}{sum(y - yhat)/(n - k)}, where \eqn{y_{i}}{y} denotes observed outcomes, \eqn{\hat{y}_{i}}{yhat} denotes fitted values, \eqn{n}{n} denotes the sample size, and \eqn{k}{k} denotes the number of parameters. The Akaike information criterion is calculated as \eqn{2k - 2ll}{2*k - 2*ll}, and the Bayesian information criterion is calculated as \eqn{k\log(n) - 2ll}{k*log(n) - 2*ll}, where \eqn{ll}{ll} denotes the log-likelihood.
#' 
#' @return \ifelse{html}{\code{\link[aldvmm]{aldvmm.gof}}}{\code{aldvmm::aldvmm.gof()}} returns a list including the following objects. \item{\code{mse}}{a numeric value of the mean squared error of observed versus fitted outcomes.}
#' \item{\code{mae}}{a numeric value of the mean absolute error of observed versus fitted outcomes.}
#' \item{\code{ll}}{a numeric value of the negative log-likelihood.}
#' \item{\code{aic}}{a numeric value of the Akaike information criterion.}
#' \item{\code{bic}}{a numeric value of the Bayesian information criterion.}
#' 
#' @export

aldvmm.gof <- function(res,
                       par,
                       ll) {
  
  gof <- list()
  
  gof[["mse"]] <- sum(res^2) / (length(res) - length(par))
  gof[["mae"]] <- sum(abs(res)) / (length(res) - length(par))
  
  if (is.na(gof[['mse']])) {
    base::warning("no mse or mae were obtained\n", 
                  call. = FALSE)
  }
  
  gof[["ll"]] <- -ll
  gof[["aic"]] <- 2 * length(par) - 2 * ll
  gof[["bic"]] <- length(par) * log(length(res)) - 2 * ll
  
  return(gof)
  
}