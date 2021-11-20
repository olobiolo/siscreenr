
#' mask values
#'
#' Mask certain values in the screen object to be omitted in downstream analysis.
#'
#' Selected values (observation-variable fields) can be replaced with NAs,
#' so that they are omitted from normalization, standardization, and hit scoring.
#'
#' @param scr a \code{data.frame}
#' @param cols character vector of column names to be masked
#' @param preds logical predicate that will determine masking
#'              or a list of such predicates of the same length as \code{cols}:
#'              a single predicate will be applied to all \code{cols},
#'              multiple predicates will be applied to the respective \code{cols};
#'              predicates can be given as bare expression or character strings
#'
#' @return A modified \code{data.frame} where the masked values are set to \code{NA}.
#'
#' @export
#'
#' @examples
#' Iris <- rbind(head(iris), tail(iris))
#' mask_values(Iris, "Sepal.Length", Species == "setosa")
#' mask_values(Iris, "Sepal.Length", "Species == \"setosa\"")
#' mask_values(Iris, c("Sepal.Length", "Sepal.Width"), Species == "setosa")
#' mask_values(Iris, c("Sepal.Length", "Sepal.Width"), list(Species == "setosa", Species == "virginica"))
mask_values <- function(scr, cols, preds) {

  if (!inherits(scr, "data.frame")) stop("\"scr\" must be a data.frame")
  if (!is.character(cols)) stop("\"cols\" must be a character vector")

  # validate columns
  columns <- intersect(cols, names(scr))
  if (length(columns) == 0L) stop("no valid columns selected")
  if (length(columns) < length(cols)) {
    msg <- paste("invalid columns were ignored:",
                 paste(setdiff(cols, columns), collapse = ", "))
    warning(msg, immediate. = TRUE)
  }

  # capture predicates
  preds <- eval(substitute(preds), scr)
  # if single predicate, convert to list for lapply
  if (!is.list(preds)) preds <- list(preds)
  # define function that
  eval_pred <- function(x) {
    if (is.character(x)) {
      return(eval(parse(text = x), scr))
    } else if (is.logical(x)) {
      return(x)
    } else {
      stop("internal error in mask_values: don't know how to handle type", typeof(x))
    }
  }
  logicals <- lapply(preds, eval_pred)

  # validate number of predicates
  if (is.list(logicals)) {
    if (length(logicals) != 1L && length(logicals) != length(columns)) {
      stop("supply either one logical predicate or one for each valid column (", length(columns), ")")
    }
  }

  # define workhorse function
  horse <- function(x, l) {
    x[l] <- NA
    return(x)
  }

  # do the deed
  scr[columns] <- mapply(horse, x = scr[columns], l = logicals, SIMPLIFY = FALSE)

  return(scr)
}
