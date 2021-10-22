#' add items to hover label
#'
#' Recursively extend highlight text by adding more factors.
#'
#' This function takes a character vector \code{label} and pastes
#' each of the columns of \code{data} specified by \code{factors}.
#' It works recursively, so if \code{factors} is an empty vector,
#' it will returned the unchanged \code{label}.
#'
#' @param data a \code{data.frame}
#' @param label character vector of hover labels to be extended
#' @param factors names of columns of \code{data}
#'                to add to \code{label}; character vector
#'
#' @return A character vector of extended hover labels.
#'
#' @export
#'
extend_label <- function(data, label, factors) {

  checkmate::assert_data_frame(data)
  checkmate::assert_character(label)
  checkmate::assert_character(factors)
  lapply(factors, function(x) checkmate::assert_choice(x, choices = names(data)))

  if (length(factors) == 0) return(label)
  label <- sprintf("%s\n%s: %s",
                   label, factors[1], data[[factors[1]]])
  factors <- factors[-1]
  extend_label(data, label, factors)
}
