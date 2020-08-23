#' normalized percent inhibition/activation
#'
#' Calculate normalized percent inhibition (NPI) or activation (NPA)
#' for variable(s) in a data frame.
#'
#' NPI is the extent to which the signal of interest is diminished compared to a positive control:
#'
#' \code{NPI = (Neg - x) / (Neg - Pos) * 100\%}
#'
#' NPA is the extent to which the signal is activated compared to a positive control:
#'
#' \code{NPA = (x - Neg) / (Pos - Neg) * 100\%}
#'
#' This is a method of data normalization in a high throughput screening campaign.
#'
#' @inheritSection normalize Grouped data frames (\code{dplyr} package)
#'
# #' @section Grouped data frames (\code{dplyr} package):
# #' Since I don't know how to properly program for class \code{grouped_df},
# #' its method is involves a workaround.
#'
#' @section Grouping variables:
#' Grouped data frames (class \code{grouped_df}) may cause problems
#' if grouping variables are used to define positive and negative controls.
#' It is best to create a separate variable to base the
#' \code{positive} and \code{negative} predicates on.
#'
#' @param x a data frame
#' @param variables column to process
#' @param positive logical predicate to define positive control observations
#'                 (max inhibition); bare or string
#' @param negative logical predicate to define negative control observations
#'                 (no inhibition); bare or string
#' @param mode character that decides whether to calculate
#'               normalized percent inhibition or normalized percent activation
#'
#' @return a modified \code{data.frame}
#'
#' @export
#'

percentage <- function(x, variables, positive, negative,
                       mode = c('inhibition', 'activation')) {
  UseMethod('percentage')
}

#' @export
#' @describeIn percentage
#' uses \code{positive} and \code{negative} to calculate mean reference values,
#' runs function for NPI or NPA over desired variables with \code{lapply},
#' then \code{cbind}s the result to \code{x}
percentage.data.frame <- function(x, variables, positive, negative,
                       mode = c('inhibition', 'activation')) {
  # check arguments
  if (!is.data.frame(x)) stop('x must be a data frame')
  if (missing(variables)) {
    message('no variables selected; taking all numeric variables except "well" and "column"')
    variables <- setdiff(names(Filter(is.numeric, x)), c('well', 'column'))
  } else {
    if (!is.character(variables)) stop('varaibles must be a character vector')
    if (!all(variables %in% names(x))) stop('invalid variables selected')
    if (!all(vapply(x[variables], is.numeric, logical(1)))) stop('non-numeric variables selected')
  }
  # capture subset specifications
  pos <- substitute(positive)
  neg <- substitute(negative)
  # more robust version, so that predicates can be given as strings
  pos <- if (is.call(pos)) pos else if (is.character(pos)) substitute(eval(parse(text = pos)))
  neg <- if (is.call(neg)) neg else if (is.character(neg)) substitute(eval(parse(text = neg)))
  # capture inhibition/activation
  method <- match.arg(mode)
  # determine positive/negative controls
  negatives <- eval(neg, x)
  positives <- eval(pos, x)

  # select and apply method
  fff <- switch(method,
                'inhibition' = function(x) {
                  P <- mean(x[positives], na.rm = T)
                  N <- mean(x[negatives], na.rm = T)
                  (N - x) / (N - P)
                },
                'activation' = function(x) {
                  P <- mean(x[positives], na.rm = T)
                  N <- mean(x[negatives], na.rm = T)
                  (x  - N) / (P - N)
                },
                stop('invalid value of "mode"'))
  y <- lapply(x[variables], fff)
  # rename columns accordingly
  names(y) <- switch(method,
                     'inhibition' = paste0(variables, '_npi'),
                     'activation' = paste0(variables, '_npa'))
  # add new variables and return
  cbind(x, as.data.frame(y))
}

#' @export
#' @describeIn percentage see \code{\link[metamethods]{data.frame__to__grouped_df}}
percentage.grouped_df <- metamethods::data.frame__to__grouped_df(percentage.data.frame)

# x <- data.frame(well = rep(1:300, 2),
#                 class = c(rep('low', 100), rep('high', 100), rep('mid', 100)),
#                 int1 = c(rnorm(100, 10, 1), rnorm(100, 100, 1), rnorm(100, 65, 5)),
#                 int2 = c(rnorm(100, 25, 1), rnorm(100, 125, 1), rnorm(100, 50, 5)),
#                 plate = 'plate 1')
# y <- x %>% dplyr::mutate_at(c('int1', 'int2'), function(x) x * 1.25) %>% dplyr::mutate(plate = 'plate 2')
# xy <-  rbind(x,y)
# percentage(xy, c('int1', 'int2'), class == 'low', class == 'high')
# percentage(dplyr::group_by(xy, plate), c('int1', 'int2'), class == 'low', class == 'high')

