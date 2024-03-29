% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun-ddCt.R
\name{ddCt}
\alias{ddCt}
\title{ddCt calculation}
\usage{
ddCt(x, ref, types, output = c("ddCt", "relative"), eff = c(2, 2))
}
\arguments{
\item{x}{Ct values of measured gene; numeric vector}

\item{ref}{Ct values of reference gene; numeric vector}

\item{types}{sample types; character vector or factor}

\item{output}{type of output, either ddCt or relative template amount (2 ^ -ddCt)}

\item{eff}{primer efficiencies; numeric vector of length 2}
}
\value{
Named numeric vector of either exponents or relative template amounts.
        Names are taken from \code{types}.
}
\description{
Calculate gene expression levels in a qPCR reaction.
}
