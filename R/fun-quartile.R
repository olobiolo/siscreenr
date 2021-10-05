
#' quartile method of hit selection
#'
#' Determine cut-off thresholds for hit selection using the quartile method.
#'
#' A less error-prone method of identifying outliers in skewed distributions, see reference for details.
#'
#' \emph{c} factor of 1.7239 corresponds to a \emph{z} score threshold of 3 and an error rate of 0.0027.
#' \emph{c} factor of 0.9826 corresponds to a \emph{z} score threshold of 2 and an error rate of 0.0455.
#'
#'
#' @param x numeric vector or a data frame
#' @param cFac stringency factor \emph{c}
#' @param subset logical vector specifying the subset to be included in the calculation;
#' @param variable character string specifying the name of variable to compute from
#' @param ... arguments passed to methods
#'
#' @return Named numeric vector of length 2, giving the calculated cut-off thresholds,
#'         given the specified \emph{c} factor.
#'
#' @references
#' Robust statistical methods for hit selection in RNA interference high-throughput screening experiments. \cr
#' Xiaohua Douglas Zhang \emph{et al.}, \emph{Pharmacogenetics} 2006, Apr 7 \cr
#' doi: 10.2217/14622416.7.3.229 \cr
#' PubMed ID: 16610941 \cr
#'
#' @export

quartile <- function(x, subset = TRUE, cFac = 1.7239) {

  quantiles <- stas::quantile(x[subset], type = 7)
  q1 <- quantiles[2]
  q2 <- quantiles[3]
  q3 <- quantiles[4]

  ans <- c("lower" = q1 - 2 * cFac * (q2 - q1),
           "upper" = q3 + 2 * cFac * (q3 - q2))

  return(ans)

}
