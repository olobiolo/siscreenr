
#' flag hits
#'
#' Flag well as hit, depending on stringency.
#'
#' If the screen involves multiple replicates, hitscores should be aggregated
#' and compared to a stringency threshold to determine whether a well is a hit.
#' Stringency can be applied summarily (add all hit scores) or fractionally
#' (divide the absolute sum of hit scores by the number of replicates).
#' Each well receives a logical flag, marking the plate/well as a hit or not.
#'
#' Thus, samples that receive positive as well as negative hitscores
#' may not be flagged during aggregation.
#'
#' Note that for the fractional method missing values are not counted
#' towards the number of replicates.
#'
#' @section Grouping:
#' This function should be run on subsets of the screen object, i.e. either
#' on a grouped data frame or on a data table with the \code{by} argument,
#' such that replicates are aggregated over plate/well combinations.
#'
#' @param x numeric vector or data frame
#' @param stringency numeric vector of length 1 or the same as \code{vars};
#'                   if 1 or larger, the summary criterion will be used;
#'                   if lower that 1, the fractional criterion will be used,
#' @param vars variables to flag
#' @param ... arguments passed to methods
#'
#' @return
#' For vector method, a named list containing the aggregated hitscore and the hit status.
#' For data frame method, a copy of \code{x} with added columns.
#'
#' @inheritSection zscore Grouped data frames (\code{dplyr} package)
#'
#' @export
#'
flag_hits <- function(x, ...) {
  UseMethod("flag_hits")
}

#' @export
#' @describeIn flag_hits vector method
flag_hits.numeric <- function(x, stringency, ...) {

  # check arguments
  if (!is.numeric(x)) stop("x must be numeric")
  if (!is.numeric(stringency)) stop("stringency must be numeric")

  # define methods
  foo_sum <- function(x, stringency) {
    sum(x, na.rm = TRUE)
  }
  foo_frac <- function(x, stringency) {
    abs(mean(x, n.rm = TRUE))
  }

  # choose and apply method
  meth <- if (stringency >= 1) sum else if (stringency > 0) mean else stop("wrong!")
  aggr <- meth(x, na.rm = TRUE)
  status <- abs(aggr) >= stringency

  ans <- list("aggregated" = aggr, "hit_status" = status)

  return(ans)
}

#' @export
#' @describeIn flag_hits data frame method \cr
#'             Aggregates hitscores for each variable named in \code{vars}. \cr
#'             Ads 2 columns to \code{x} for each element of \code{vars}:
#'             one for the aggregate (numeric) and one for the hit status (logical).
flag_hits.data.frame <- function(x, stringency, vars, ...) {

  if (!inherits(x, "data.frame")) stop("\"x\" must be a data.frame")
  if (!is.character(vars)) stop("\"vars\" must be a character vector")
  if (!all(is.element(vars, names(x)))) stop("some variables not found")
  if (!is.numeric(stringency)) stop("\"stringency\" must be a numeric vector")
  if (length(stringency) != 1L && length(stringency) != length(vars))
    stop("\"stringency\" must have length 1 or the same as \"vars\" (", length(vars), ")")

  L <- vector(mode = "list", length = length(vars))

  if (length(stringency) == 1L && length(vars) > 1L) {
    stringency <- rep(stringency, length(vars))
  }

  # choose and apply method
  # if (stringency >= 1) {
  #   meth <- sum
  #   varss <- paste(vars, "sum", sep = "_")
  #   varsss <- paste(varss, "HIT", sep = "_")
  # } else if (stringency > 0) {
  #   meth <- mean
  #   varss <- paste(vars, "frac", sep = "_")
  #   varsss <- paste(varss, "HIT", sep = "_")
  # } else stop("wrong!")

  for (v in seq_along(vars)) {
    if (stringency[v] >= 1) {
      meth <- sum
      varss <- paste(vars, "sum", sep = "_")
      varsss <- paste(varss, "HIT", sep = "_")
    } else if (stringency[v] > 0) {
      meth <- mean
      varss <- paste(vars, "frac", sep = "_")
      varsss <- paste(varss, "HIT", sep = "_")
    } else stop("wrong!")

    x[[varss[v]]] <- meth(x[[vars[v]]], na.rm = TRUE)
  }
  for (v in seq_along(vars)) {
    x[[varsss[v]]] <- abs(x[[varss[v]]]) >= stringency[v]
  }

  return(x)
}

#' @export
#' @describeIn flag_hits see \code{\link[metamethods]{data.frame__to__grouped_df}}
flag_hits.grouped_df <- metamethods::data.frame__to__grouped_df(flag_hits.data.frame)
