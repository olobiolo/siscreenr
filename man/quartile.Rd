% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun-quartile.R
\name{quartile}
\alias{quartile}
\title{quartile method of hit selection}
\usage{
quartile(x, subset = TRUE, cFac = 1.7239)
}
\arguments{
\item{x}{numeric vector or a data frame}

\item{subset}{logical vector specifying the subset to be included in the calculation;}

\item{cFac}{stringency factor \emph{c}}
}
\value{
Named numeric vector of length 2, giving the calculated cut-off thresholds,
        given the specified \emph{c} factor.
}
\description{
Determine cut-off thresholds for hit selection using the quartile method.
}
\details{
A less error-prone method of identifying outliers in skewed distributions, see reference for details.

\emph{c} factor of 1.7239 corresponds to a \emph{z} score threshold of 3 and an error rate of 0.0027.
\emph{c} factor of 0.9826 corresponds to a \emph{z} score threshold of 2 and an error rate of 0.0455.
}
\references{
Robust statistical methods for hit selection in RNA interference high-throughput screening experiments. \cr
Xiaohua Douglas Zhang \emph{et al.}, \emph{Pharmacogenetics} 2006, Apr 7 \cr
doi: 10.2217/14622416.7.3.229 \cr
PubMed ID: 16610941 \cr
}
