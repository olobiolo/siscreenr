#' collate layout file for a screen
#'
#' Take any number of layout files and create a single layout file.
#' There must be exactly one file per plate type but each file may contain
#' layouts for different plating dates and these may be different.
#'
#' This is a utility function for neatly putting together many layouts.
#' Sometimes a layout changes over the course of the screen.
#' In that case it is easy (if a little tedious) to prepare a composite layout file
#' with a separate column for every plating date.
#' This function will load all such files and convert them into a single file
#' that can be loaded with \code{\link{build_screen}}.
#' If your screen maintains the same layout throughout, running this function is redundant.
#'
#' The output file is a tab delimited text file that contains a long-form table
#' with the following columns:
#' \code{well}, \code{well_type}, \code{plate_type}, and possibly \code{plated}.
#'
#' Input files \strong{must} follow these rules:
#'
#' 1. must be tab delimited text files with a header
#'
#' 2. must have a "well" column with integer well numbers
#'
#' 3. must have the plate type encoded either as a "plate_type" column
#' \strong{or} in the file name as the last element before the extension, e.g. "layout_S14_test.txt"
#'
#' 4. if there is a single layout, it must be in a column called "well_type"
#'
#' 5. if there is more than one layout, each must be placed in a separate column,
#' the name of which must be the plating date in yyyymmdd format (legal separators are allowed)
#'
#' There should also be a "position" column or "row" and "column" columns but this is optional.
#'
#' @param ... files or a character vector of files to load and collate
#' @param outfile file to save the layout in
#'
#' @return If \code{outfile} is not specified, the collated layout table, otherwise nothing.
#'
#' @export
#'
# #' @examples
# #' L <- layouts('layout_S14_test.txt',
# #'              'layout_S14_control.txt')
# #' head(L)
# #' table(L$plated, L$well_type, L$plate_type)
# #'
# #' layouts('layout_S14_test.txt',
# #'         'layout_S14_control.txt',
# #'         outfile = 'temp_layout_S14.txt')
# #' head(read.delim('temp_layout_S14.txt'))

layouts <- function(..., outfile = NULL) {
  # capture files
  files <- list(...)
  if (length(files) == 0) stop('no files specified')
  if (!all(vapply(files, is.character, logical(1)))) stop('all input files must be specified as character vectors')
  files <- unique(unlist(files))
  if (!all(file.exists(files))) {
    files <- files[file.exists(files)]
    if (length(files) == 0) stop('no existing files specified')
    warning('some files are missing and will be omitted')
  }

  # validate files, condition 2
  check_column_well <- function(file) {
    X <- utils::read.delim(file)
    if (is.element("well", names(X)) && is.integer(X$well)) return(TRUE) else return(FALSE)
  }
  if (!all(vapply(files, check_column_well, logical(1)))) stop("all files must contain column \"well\"")
  # validate files, condition 4
  check_column_well_type <- function(file) {
    X <- utils::read.delim(file)
    if (is.element("well_type", names(X))) return(TRUE) else return(FALSE)
  }
  if (length(files) == 1L && !check_column_well_type(files))
    stop("single alyout file must contain column \"well_type\"")

  # apply processing function over all files
  L <- data.table::rbindlist(lapply(files, process_layout))

  if (is.null(outfile)) {
    return(L)
  } else {
    utils::write.table(L, outfile, quote = FALSE, sep = '\t', row.names = FALSE)
  }
}

# internal function that will load single file and process it accordingly
process_layout <- function(x) {
  # load file
  X <- utils::read.delim(x)
  # attempt to extract plate type from file name
  if (!is.element('plate_type', names(X))) {
    X_name_split <- unlist(strsplit(x, '_|\\.'))
    X$plate_type <- X_name_split[length(X_name_split) - 1]
  }
  # transform to long-form
  data.table::setDT(X)
  y <- data.table::melt(X,
                        measure.vars = grep("[0-9]{8}", names(X)),
                        variable.name = "plated", value.name = "well_type",
                        variable.factor = FALSE, value.factor = FALSE)
  # fix dates
  if (is.element("plated", names(y))) y$plated <- gsub('^X', '', y$plated)

  return(y)
}
