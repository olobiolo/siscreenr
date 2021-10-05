#' assign hit scores
#'
#' Assign hit status to single wells in a screen based on their z scores.
#'
#' A data point is considered a hit if its hitting variable (usually a z score)
#' exceeds a threshold. A point can have one of three hitscores:
#' \itemize{
#'   \item{
#'   1 (positive hit) if its value is equal to or higher than the upper cut-off threshold
#'   }
#'   \item{
#'   -1 (negative hit) if its value is equal to or lower than the lower cut-off threshold
#'   }
#'   \item{
#'   0 (non-hit) otherwise
#'   }
#' }
#'
#' @param x a numeric vector
#' @param threshold numeric vector specifying the lower- and upper cut-off threshold;
#'                  normally of length 2, but a symmetric (e.g. -3 and 3) threshold
#'                  can be specified with a single numeric;
#'
#' @return a vector of hit statuses represented as integers: 1, -1 or 0
#'
#' @export

hitscore <- function(x, threshold = 2) {
  if (!is.numeric(x)) stop('"x" must be numeric')
  if (length(threshold) > 2L) stop("\"threshold\" must be of length 1 or 2")

  if (length(threshold) == 1L) {
    threshold <- c(ithreshold, threshold)
  }
  x1 <- ifelse(x > threshold[1] & x < threshold[2], 0L, x)
  x2 <- ifelse(x1 <= threshold[1], -1L, x1)
  x3 <- ifelse(x2 >= threshold[2], 1L, x2)
  return(x3)
}
