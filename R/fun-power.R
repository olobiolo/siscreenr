
#' power functions
#'
#' \code{pow} computes \code{base} to the power of \code{x},\cr
#' \code{pow10} computes 10 to the power of \code{x} (inverse of \code{log10}),\cr
#' and \code{pow10} computes 2 to the power of \code{x} (inverse of \code{log2})
#'
#' @param x a numeric vector
#' @param exponent a single number
#'
#' @return
#' A vector of the same length as \code{x} containing transformed values.
#'
#' @examples
#' pow(2, 3)
#' pow10(log10(2))
#'
#' @export
#' @rdname pow
#'
pow <- function(x, base = exp(1)) {

  checkmate::assert_numeric(x)
  checkmate::assert_number(base)

  base ^ x
}

#' @export
#' @rdname pow
pow2 <- function(x) {

  checkmate::assert_numeric(x)

  2 ^ x
}

#' @export
#' @rdname pow
pow10 <- function(x) {

  checkmate::assert_numeric(x)

  10 ^ x
}
