
#' meaningful factors
#'
#' Check for categorical variables with more than one unique value.
#'
#' @param x a \code{data.frame}
#'
#' @return A character vector of column names.
#'
#' @export
#'
uniques <- function(x) {

  checkmate::assert_data_frame(x)

  # function that checks if vector is character or factor with > 1 unique value
  foo <- function(x) {
    (is.character(x) || is.factor(x)) && length(unique(x)) > 1L
  }
  # use foo on data frame and get names of result
  ans <- names(Filter(foo, x))

  return(ans)

}
