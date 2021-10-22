
#' ddCt calculation
#'
#' Calculate gene expression levels in a qPCR reaction.
#'
#' @param x Ct values of measured gene; numeric vector
#' @param ref Ct values of reference gene; numeric vector
#' @param types sample types; character vector or factor
#' @param output type of output, either ddCt or relative template amount (2 ^ -ddCt)
#' @param eff primer efficiencies; numeric vector of length 2
#'
#' @return Named numeric vector of either exponents or relative template amounts.
#'         Names are taken from \code{types}.
#'
#' @export
#'
ddCt <- function(x, ref, types, output = c("ddCt", "relative"), eff = c(2, 2)) {

  checkmate::assert_numeric(x)
  checkmate::assert_numeric(ref)
  output <- match.arg(output)
  checkmate::assert_numeric(eff, len = 2L)


  dCt <- log2(eff[1] ^ x / eff[2] ^ ref)
  ddCt <- dCt - meanG(dCt[types == "dmso"])

  names(ddCt) <- f

  if (output == "ddCt") {
    return(ddCt)
  } else if (output == "relative") {
    return(2 ^ (-ddCt))
  }

}
