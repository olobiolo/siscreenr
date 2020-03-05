#' check scan completion
#'
#' Tally image files after a scan andreport missing images.
#'
#' The function firt lists all directories in a given location
#' and identifies recent scans by their numeric postfixes.
#' Thereafter, it lists all .tif files in the appropriate data directories
#' and prints the well names, for which the number of collected images
#' is different to the maximum of all wells.
#'
#' @param path path to directory where scans are stored
#' @param output type of output:
#'               print/console for printing to the console,
#'               return/list for returning a named list;
#'               determined by partial match
#'
#' @export
#'

check_scan <- function(path, output = 'print') {

  output <- match.arg(arg = output, choices = c('print', 'console', 'return', 'list'))

  # get files names with postfixes
  files <- list.files(path = path, pattern = '_[0-9]{3}$', full.names = TRUE, recursive = FALSE)
  # pick out directories
  dirs <- files[file_test('-d', files)]


  # do the thing
  if (output == 'print' || output == 'console') {
    for (d in dirs) {
      # list image files
      images <- list.files(path = paste(d, 'data', sep = '/'), pattern = 'tif$')
      # isolate well names
      wells <- vapply(strsplit(images, '--'), function(x) x[1], character(1), USE.NAMES = FALSE)
      # get number of ocurrences
      freqs <- table(wells)
      # get faulty wells
      faulty <- freqs[freqs != max(freqs)]
      # print
      cat(basename(d), '\n')
      if (length(freqs) != 0) {
        cat('missing images in well(s):', '\t', paste(names(faulty), collapse = ', '), '\n\n')
      } else {
        cat('scan complete', '\n\n')
      }
    }
  } else if (output == 'return' || output == 'list') {
    check_one <- function(path) {
      images <- list.files(path = paste(path, 'data', sep = '/'), pattern = 'tif$')
      wells <- vapply(strsplit(images, '--'), function(x) x[1], character(1), USE.NAMES = FALSE)
      freqs <- table(wells)
      faulty <- freqs[freqs != max(freqs)]
      if (length(faulty) > 0) {
        return(paste(names(faulty), collapse = ', '))
      } else {
        return("scan complete")
      }
    }

    ans <- lapply(dirs, check_one)
    names(ans) <- basename(dirs)
    return(ans)
  } else {
    stop('invalid \"output\" parameter')
  }
}
