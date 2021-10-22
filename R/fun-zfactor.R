
#' calculate Z factor
#'
#' Calculate the Z factor (also known as Z'factor) to estimate assay quality.
#'
#' Z factor is used to estimate measurement accuracy in relation to the
#' dynamic range of the assay.
#'
#' Z factor of 1 is perfect.  \cr
#' 0.5 - 1 is a mean a good assay. \cr
#' 0 - 0.5 means a weak assay. \cr
#' Z factor below 0 means the assay cannot be trusted.
#' This can be justifiable in some cases.
#'
#' @param x numeric vector
#' @param types grouping factor (e.g. well types) to be passed to \code{tapply}
#'              values \code{strong} must include "positive" and "negative"
#'
#' @return A single numeric.
#'
#'
zfactor <- function(x, types) {

  checkmate::assert_numeric(x)
  # types is a factor of observation types, which includes: negative, positive
  checkmate::assert_factor(types)
  checkmate::assert_names(levels(x), must.include = c("positive", "negative"))

  # drop unnecessary data
  ind <- types %in% c("negative", "positive")
  x <- x[ind]
  f <- types[ind]
  f <- droplevels(f)
  # calculate means and sds
  means <- tapply(x, f, mean, na.rm = TRUE)
  sds <- tapply(x, f, sd, na.rm = TRUE)

  ans <- 1 - (3 * (sds["positive"] + sds["negative"])) / abs(means["positive"] - means["negative"])

  return(ans)

}
