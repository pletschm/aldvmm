% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aldvmm.sum.R
\name{aldvmm.sum}
\alias{aldvmm.sum}
\title{Creating Summary Table}
\usage{
aldvmm.sum(object, digits = max(3L, getOption("digits") - 3L), level = 0.95)
}
\arguments{
\item{object}{an \ifelse{html}{\code{\link[aldvmm]{aldvmm}}}{
\code{aldvmm()}} model fit object of class "aldvmm".}

\item{digits}{a numeric value of the number of digits in the reporting
table.}

\item{level}{a numeric value of the confidence level.}
}
\value{
a \code{data.frame} object with a summary table of regression results.
}
\description{
\ifelse{html}{\code{\link[aldvmm]{aldvmm.sum}}}{ \code{aldvmm.sum()}}
creates a summary table of regression results.
}
