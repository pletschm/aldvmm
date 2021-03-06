% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aldvmm.mm.R
\name{aldvmm.mm}
\alias{aldvmm.mm}
\title{Creating Design Matrices}
\usage{
aldvmm.mm(data, formula, ncmp, lcoef)
}
\arguments{
\item{data}{a data frame, list or environment (or object coercible to a data
frame by \cr \ifelse{html}{\code{\link[base]{as.data.frame}}}{
\code{base::as.data.frame()}}) including data on outcomes and explanatory
variables in \code{'formula'}.}

\item{formula}{an object of class \code{"formula"} with a symbolic
description of the model to be fitted. The model formula takes the form
\code{y ~ x1 + x2 | x1 + x4}, where the \code{|} delimiter separates the
model for expected values of normal distributions (left) and the
multinomial logit model of probabilities of component membership (right).}

\item{ncmp}{a numeric value of the number of components that are mixed. The
default value is 2. A value of 1 represents a tobit model with a gap
between 1 and the maximum value in \code{'psi'}.}

\item{lcoef}{a character vector of length 2 with labels of objects including
regression coefficients of component distributions (default \code{"beta"})
and coefficients of probabilities of component membership (default
\code{"delta"}).}
}
\value{
a named list of numeric matrices. \item{\code{beta}}{a numeric
  design matrix for the model of component distributions.}
  \item{\code{delta}}{a numeric design matrix of the multinomial logit model
  of probabilities of component membership.}
}
\description{
\ifelse{html}{\code{\link[aldvmm]{aldvmm.mm}}}{\code{aldvmm.mm()}} creates
two design matrices, one of the model of component distributions
(\code{"beta"}) and one of the model of probabilities of component
membership (\code{"delta"}).
}
\details{
\ifelse{html}{\code{\link[aldvmm]{aldvmm.mm}}}{\code{aldvmm.mm()}}
  uses
  \ifelse{html}{\code{\link[stats]{model.matrix}}}{\code{stats::model.matrix()}}
   to create design matrices for models of component distributions
  (\code{"beta"}) and probabilities of component membership (\code{"delta"})
  based on \code{'formula'} supplied to
  \ifelse{html}{\code{\link[aldvmm]{aldvmm.ll}}}{\code{aldvmm.ll()}}. The
  design matrix for probabilities of group membership is only created if
  more than one components are specified  in \code{'ncmp'}.
}
