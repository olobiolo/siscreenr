
#' geometric mean
#'
#' Compute the geometric mean.
#'
#' @param x numeric vector
#' @param na.rm logical flag indicating whether to drop missing values
#'
#' @return Single numeric representing the geometric mean.
#'
#' @export
#'
meanG <- function(x, na.rm = FALSE) {

  checkmate::assert_numeric(x)
  checkmate::assert_flag(na.rm)

  if (na.rm) x <- x[!is.na(x)]

  ans <- prod(x) ^ (1 / length(x))

  return(ans)

}
