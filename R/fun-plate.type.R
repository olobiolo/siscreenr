#' @name plate.type
#'
#' @title plate type conversion
#'
#' @description
#' Manage the translation of plate type codes during building a screen object.
#'
#' @details
#' During building a screen object \code{build_screen} calls the internal function
#' \code{plate.type.converter} to read the plate type
#' encoded in field 4 of the plate name and create the columns
#' \code{plate_type} and \code{replica} accordingly.
#' This is done according to a dictionary file stored in the package directory.
#'
#' @aliases plate.type.converter plate.type.conversion plate.type.converter.key
#'          plate_type plate_type_conversion
#'          show_conversion_key edit_conversion_key recover_conversion_key
#'
NULL

#' @param x a screen object
#'
#' @describeIn plate.type
#' Takes a screen object and returns one in which columns "plate_type" and "replica" are altered.
#'
#' @keywords internal
#'
plate.type.converter <- function(x) {
  thefile <- system.file("extdata", "plate.type.converter.key.txt", package = "siscreenr")
  key <- utils::read.delim(thefile, stringsAsFactors = FALSE)
  if (!is.data.frame(key)) stop('the dictionary file must a data frame', '\n\t',
                                'see: ?plate.type')
  # convert all non-factor columns to factor
  ind <- vapply(key, is.factor, logical(1))
  if (any(ind)) key[ind] <- lapply(key[ind], factor)
  # convert codes descriptors
  ## for each column a converting function is created that calls switch
  ## alternatives for switch
  dots.replica <- c(structure(as.list(key$replica), names = key$code), 'unknown')
  dots.type <- c(structure(as.list(key$plate_type), names = key$code), 'unknown')
  ## create functions
  f_replica <- function(x) do.call(switch, c(x, dots.replica))
  f_plate_type <- function(x) do.call(switch, c(x, dots.type))
  ## apply functions
  x$replica <- vapply(x$plate_type, f_replica, character(1))
  x$plate_type <- vapply(x$plate_type, f_plate_type, character(1))

  # unfortunately, 'unknown' types will crash table merging with the layout file
  if (any(startsWith(x$plate_type, 'unknown'))) {
    stop('from plate.type.converter:\n\t',
         'plate types in layout file are not found in the converter key',
         call. = FALSE)
  }

  return(x)
}

#' @export
#' @describeIn plate.type
#' Reads and prints the conversion key as a data frame with all character columns

show_conversion_key <- function() {
  thefile <- system.file('extdata/plate.type.converter.key.txt', package = 'siscreenr')
  utils::read.delim(thefile, stringsAsFactors = FALSE)
}

#' @param key a data frame with all character columns
#'
#' @export
#' @describeIn plate.type
#' The new key will replace the file in the package directory.
#' When this function is run, it creates a backup copy of the original key.
#' The key can be recovered with \code{recover_conversion_key}.
edit_conversion_key <- function(key) {
  nms <- names(key)
  nms.req <- c('code', 'plate_type', 'replica')
  if (!identical(nms, nms.req))
    stop('key must consist of the following columns: ', paste(nms.req, collapse = ', '))
  if (anyNA(key))
    stop('missing values in key')
  char.cols <- sapply(key, is.character)
  if (!all(char.cols))
    stop('the following columns in key are not character vectors: ',
         paste(names(char.cols[!char.cols]), collapse = ', '), '\n\t',
         'it is recommended that all columns be of type character')

  thefile <- system.file('extdata/plate.type.converter.key.txt', package = 'siscreenr')
  theoldfile <- sub('converter.key.txt', 'converter.key.original.txt', thefile, fixed = TRUE)
  file.copy(thefile, theoldfile)
  utils::write.table(key, thefile, sep = '\t', row.names = FALSE, quote = FALSE)
}

#' @export
#' @describeIn plate.type
#' When the file that contains the key for plate type conversion is modified
#' by \code{edit_conversion_key}, a backup copy is created in the package directory.
#' This function will remove the current key file and restore the original.
recover_conversion_key <- function() {
  thefile <- system.file('extdata/plate.type.converter.key.txt', package = 'siscreenr')
  theoldfile <- system.file('extdata/plate.type.converter.key.original.txt', package = 'siscreenr')
  if (theoldfile == '') stop('Backup of conversion key was not found. Perhaps you are using the default?')
  cat('WARNING: you are about to delete the current conversion key.')
  proceed <- readline('Proceed? [Yes]/[No] ')
  if (proceed == 'Yes') {
    file.rename(from = theoldfile, to = thefile)
    message('Recovery complete.')
  } else if (proceed == 'No') {
    message('Recovery halted.')
  } else message('cancelled')
}
