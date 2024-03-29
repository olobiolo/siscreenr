#' @title normalize screen data
#'
#' @description
#' Normalize data, i.e. express each point as its deviation from a center.
#'
#' @details
#' There are three normalization methods available at the moment:
#' \itemize{
#'   \item{
#'   \code{mean}: subtract mean of reference from each data point
#'   }
#'   \item{
#'   \code{median}: subtract median of reference from each data point
#'   }
#'   \item{
#'   \code{medpolish}: run Tukey's median polish and return residuals;
#'                     calls \code{stats::medpolish}
#'   }
#' }
#'
#' @param x screen object, i.e. a \code{data.frame}, possibly \code{grouped}
#' @param variables variables to normalize;
#'                  character vector of column names or numeric vector of column indices
#' @param method normalization method, see \code{Details}
#' @param reference logical predicate that defines reference observations, bare or character
#'
#' @return an invisible \code{data.frame}
#'
#' @section Warnings:
#' If you are using the medpolish method, variables will be temporarily converted
#' from vectors to matrices. Make sure your data frames are ordered by column
#' (the default way matrices are filled) rather than by row (the default ScanR format).
#' Also, row and column specifications are necessary to read the matrix dimensions,
#' so \code{x} must contain either "row" and "column" variables or a "position" variable.
#'
#' For other methods a reference subset can be specified. Any logical predicate will do.
#' If no reference is declared, normalization will be done against the whole population.
#'
#' @inheritSection zscore Grouped data frames (\code{dplyr} package)
#'
#' @export
#'
normalize <- function(x, variables, method = c('median', 'mean', 'medpolish'), reference) {
  UseMethod('normalize')
}

#' @export
#' @describeIn normalize
#' establishes normalization method, with possible \code{reference},
#' and runs it on desired variables with \code{lapply},
#' then \code{cbind}s the result to \code{x}
normalize.data.frame <- function(x,
                                 variables,
                                 method = c('median', 'mean', 'medpolish'),
                                 reference) {
  # check arguments
  missing.columns <- setdiff(variables, names(x))
  if (length(missing.columns > 0))
    stop('\n',
         'missing variables selected: ', paste(missing.columns, collapse = ', '), '\n',
         'avaiable variables: ', paste(names(x), collapse = ', '))
  method <- match.arg(method)
  if (method == 'medpolish' &
      (!is.element('position', names(x)) |
        any(!is.element(c('row', 'column'), names(x)))))
    stop('"medpolish" method requires well coordinates, see help')
  if (method == 'medpolish' & !missing(reference))
    message('running median polish, "reference" will be ignored')
  if (method != 'medpolish') {
    # get reference as logical vector
    if (!missing(reference)) {
      reference <- substitute(reference)
      if (is.character(reference)) {
        # character string is parsed and evaluated within x
        Reference <- eval(parse(text = reference), x)
      } else if (is.call(reference)) {
        # call is evaluated within x
        Reference <- eval(reference, x)
      } else if (is.logical(reference)) {
        # logical vector is taken as is
        Reference <- reference
      }
    } else {
      message('no reference; data will be normalized to the whole of the population')
      Reference <- TRUE
    }
  }

  # define normalization methods
  meth.mean <- function(x) {
    R <- mean(x[Reference], na.rm = TRUE)
    x - R
  }
  meth.median <- function(x) {
    R <- stats::median(x[Reference], na.rm = TRUE)
    x - R
  }
  meth.medpolish <- function(x) {
    if (any(is.infinite(x)))
      stop('infinite values will derail the running median procedure', call. = FALSE)
    X <- get('x', parent.frame(2))
    nr <- length(unique(as.character(X$row)))
    nc <- length(unique(as.character(X$column)))
    x_mat <- matrix(x, nrow = nr, ncol = nc)
    polished <- stats::medpolish(x_mat, trace.iter = FALSE, na.rm = TRUE)
    return(as.vector(polished$residuals))
  }

  # assign normalization method (methods are defined as separate functions)
  meth <- switch(method,
                 mean = meth.mean,
                 median = meth.median,
                 medpolish = meth.medpolish)

  # do the deed
  x_normalized <- lapply(x[variables], meth)
  names(x_normalized) <- paste0(variables, '_normalized_', method)
  x_result <- cbind(x, as.data.frame(x_normalized))
  invisible(x_result)
}

#' @export
#' @describeIn normalize see \code{\link[metamethods]{data.frame__to__grouped_df}}
normalize.grouped_df <- metamethods::data.frame__to__grouped_df(normalize.data.frame)

