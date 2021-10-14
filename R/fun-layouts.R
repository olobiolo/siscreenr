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
#' @param ... files to load and collate, given as one or more character vectors
#' @param file file to save the layout in
#'
#' @return if \code{file} is missing, the collated layout table, otherwise nothing
#'
#' @export
#'

layouts <- function(..., file) {
  # change global option and clean up afterwards
  options(stringsAsFactors = FALSE)
  on.exit(options(stringsAsFactors = TRUE))
  # capture files
  files <- unique(unlist(list(...)))
  if (length(files) == 0) stop('no files defined')
  if (!all(file.exists(files))) {
    warning('some files are missing and will be omitted')
    files <- files[file.exists(files)]
  }
  if (length(files) == 0) stop('no existing files defined')

  # define function that will load single file and process it accordingly
  process_layout <- function(x) {
    X <- utils::read.delim(x)
    if (!is.element('plate_type', names(X))) {
      X_name_split <- unlist(strsplit(x, '_|\\.'))
      X$plate_type <- X_name_split[length(X_name_split) - 1]
    }
    y <- tidyr::gather(X, 'plated', 'well_type', dplyr::matches('[0-9]{8}'))
    y$plated <- gsub('^X', '', y$plated)
    return(y)
  }

  # apply the function over all requested files
  L <- do.call(rbind, lapply(files, process_layout))

  if (missing(file)) return(L) else utils::write.table(L, file, quote = F, sep = '\t', row.names = F)
}

#' @examples
#' L <- layouts('layout_S14_test.txt',
#'              'layout_S14_control.txt')
#' head(L)
#' table(L$plated, L$well_type, L$plate_type)
#'
#' layouts('layout_S14_test.txt',
#'         'layout_S14_control.txt',
#'         file = 'temp_layout_S14.txt')
#' head(read.delim('temp_layout_S14.txt'))
