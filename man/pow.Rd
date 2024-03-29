% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun-power.R
\name{pow}
\alias{pow}
\alias{pow2}
\alias{pow10}
\title{power functions}
\usage{
pow(x, base = exp(1))

pow2(x)

pow10(x)
}
\arguments{
\item{x}{a numeric vector}

\item{exponent}{a single number}
}
\value{
A vector of the same length as \code{x} containing transformed values.
}
\description{
\code{pow} computes \code{base} to the power of \code{x},\cr
\code{pow10} computes 10 to the power of \code{x} (inverse of \code{log10}),\cr
and \code{pow10} computes 2 to the power of \code{x} (inverse of \code{log2})
}
\examples{
pow(2, 3)
pow10(log10(2))

}
