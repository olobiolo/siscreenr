#' check scan completion
#'
#' Report wells with missing images or logged events (e.g. errors).
#'
#' This is a reporting utility specific to ScanR systems.
#'
#' \code{check_scan} first lists all directories in a given location
#' and identifies recent scans by their numeric suffixes
#' Then, it lists all .tif files in the appropriate data directories
#' and checks the well names, for which the number of collected images
#' is different to the maximum of all wells.
#'
#' \code{scan_status} also checks which wells (if any) had any events logged
#' in the ScanR acquisition log and (in console mode) informs whether the scan
#' is complete or still ongoing.
#'
#' \code{scan_status} is the new, superior version. \code{check_scan} will be deprecated in time.
#'
#'
#' @param path path to directory where scans are stored
#' @param output type of output:
#'               print/console for printing to the console,
#'               return/list for returning a named list;
#'               determined by partial match
#' @return In console/print mode - nothing. In list/return mode, a named list.
#' Information on scan completion by \code{scan_status} only appears in console mode.
#'
#' @export
#'
#' @aliases scan_status

check_scan <- function(path, output = 'print') {

  if (!dir.exists(path)) stop('no such directory')

  output <- match.arg(arg = output, choices = c('print', 'console', 'return', 'list'))

  # get files names with suffixes
  files <- list.files(path = path, pattern = '_[0-9]{3}$', full.names = TRUE, recursive = FALSE)
  # pick out directories
  dirs <- files[utils::file_test('-d', files)]

  # do the thing
  if (output == 'print' || output == 'console') {
    if (length(dirs) == 0) {
      cat("no active scans", '\n')
    }
    for (d in dirs) {
      # list image files
      images <- list.files(path = paste(d, 'data', sep = '/'), pattern = 'tif$')
      # split image file names
      images_split <- strsplit(images, '--')
      # isolate well indices
      indices <- vapply(images_split, function(x) x[2], character(1), USE.NAMES = FALSE)
      # isolate well names
      wells <- vapply(images_split, function(x) x[1], character(1), USE.NAMES = FALSE)
      # order wells according to indices
      wells <- wells[order(indices)]
      # convert wells to factor so that they are not sorted by table
      wells <- factor(wells, levels = unique(wells))
      # get number of occurrences
      freqs <- table(wells)
      # get faulty wells
      faulty <- freqs[freqs != max(freqs)]
      # print
      cat(basename(d), '\n')
      if (length(faulty) != 0) {
        cat('missing images in well(s):', '\t', paste(names(faulty), collapse = ', '), '\n\n')
      } else {
        cat('scan complete', '\n\n')
      }
    }
  } else if (output == 'return' || output == 'list') {
    if (length(dirs) == 0) {
      cat("no active scans", '\n')
      return(list())
    }

    check_one <- function(path) {
      images <- list.files(path = paste(path, 'data', sep = '/'), pattern = 'tif$')
      images_split <- strsplit(images, '--')
      indices <- vapply(images_split, function(x) x[2], character(1), USE.NAMES = FALSE)
      wells <- vapply(images_split, function(x) x[1], character(1), USE.NAMES = FALSE)
      wells <- wells[order(indices)]
      wells <- factor(wells, levels = unique(wells))
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


#' @export
#' @describeIn check_scan
#' upgrade:
#'
#' 1. Also reports wells in which ScanR has logged events.
#'
#' 2. In \code{console} mode prints scan completion.
#'
#' @section Warning:
#' \code{scan_status} is a work in progress. Some errors may escape notice.
#' Due to ScanR log formats the reported events may be assigned to a neighboring well.

# scan_status <- function(path, output = c('print', 'return')) {
#
#   # messages:
#   message.noscans <- 'no active scans'
#   message.scanactive <- 'scan is running'
#   message.scanfinished <- 'scan has finished' # scan complete
#   message.noissues <- 'scan complete' # scan complete
#   message.issues <- 'issues in well(s)' # missing images in well(s)
#
#
#   if (!dir.exists(path)) stop('no such directory')
#
#   output <- match.arg(arg = output)
#
#   # get file names with suffixes
#   files <- list.files(path = path, pattern = '_[0-9]{3}$', full.names = TRUE, recursive = FALSE)
#   # pick out directories
#   dirs <- files[utils::file_test('-d', files)]
#
#   # if no directories found
#   if (length(dirs) == 0) {
#     cat(message.noscans, '\n')
#     if (output == 'return' || output == 'list') return(list())
#   }
#
#
#   # function that extracts wells with logged events
#   wells.logged <- function(logfile) {
#     # does log contain events
#     events <- !identical(levels(logfile[ ,1]), c('ENDACQUISITION', 'IMAGEPOS'))
#     if (events) {
#       # these are the rows with events
#       #logfile[which(logfile[, 1] == 'LOG'), ]
#       # next row after event
#       wells <- logfile[which(logfile[, 1] == 'LOG') +2, 2]
#       wells <- sort(as.numeric(sub('^W=', '', wells)))
#       # translate well numbers to positions
#       row <- function(w) LETTERS[(w - 1) %/% 24 + 1]
#       col <- function(w) (w - 1) %% 24 + 1
#       positions <- paste0(row(wells), col(wells))
#
#       return(unique(positions))
#     } else {
#       return(NULL)
#     }
#   }
#
#   # do the thing
#   if (output == 'print') {
#
#     for (d in dirs) {
#       # check acquisition log to see if scan has finished
#       aclog <- utils::read.delim(paste(d, 'AcquisitionLog.dat', sep = '/'), skip = 1)
#       # print scan name and status
#       cat(basename(d), ':', '\t', sep = '')
#       if (aclog[nrow(aclog), 1] == 'ENDACQUISITION') {
#         cat(message.scanfinished, '\n')
#       } else {
#         cat(message.scanactive, '\n')
#       }
#       # list image files
#       images <- list.files(path = paste(d, 'data', sep = '/'), pattern = 'tif$')
#       # isolate well indices
#       indices <- vapply(strsplit(images, '--'), function(x) x[2], character(1), USE.NAMES = FALSE)
#       # isolate well names
#       wells <- vapply(strsplit(images, '--'), function(x) x[1], character(1), USE.NAMES = FALSE)
#       # order wells according to indices
#       wells <- wells[order(indices)]
#       # convert wells to factor so that they are not sorted by table
#       wells <- factor(wells, levels = unique(wells))
#       # get number of occurrences
#       freqs <- table(wells)
#       # get faulty wells
#       faulty <- freqs[freqs != max(freqs)]
#       # get eventful wells
#       eventful <- wells.logged(aclog)
#       # combine
#       badwells <- unique(c(names(faulty), eventful))
#       # print scan completeness
#       if (length(badwells) > 0) {
#         cat(message.issues, ': ', '\t', paste(badwells, collapse = ', '), '\n\n', sep ='')
#       } else {
#         cat(message.noissues, '\n\n')
#       }
#     }
#   } else if (output == 'return') {
#
#   check_one <- function(path) {
#       images <- list.files(path = paste(path, 'data', sep = '/'), pattern = 'tif$')
#       indices <- vapply(strsplit(images, '--'), function(x) x[2], character(1), USE.NAMES = FALSE)
#       wells <- vapply(strsplit(images, '--'), function(x) x[1], character(1), USE.NAMES = FALSE)
#       wells <- wells[order(indices)]
#       wells <- factor(wells, levels = unique(wells))
#       freqs <- table(wells)
#       faulty <- freqs[freqs != max(freqs)]
#       aclog <- utils::read.delim(paste(path, 'AcquisitionLog.dat', sep = '/'), skip = 1)
#       eventful <- wells.logged(aclog)
#       badwells <- c(names(faulty), eventful)
#       scan.status <- if (aclog[nrow(aclog), 1] == 'ENDACQUISITION') message.scanfinished else message.scanactive
#       scan.status <- paste(basename(path), ": ", scan.status, sep = "")
#       attr(badwells, 'scan.status') <- scan.status
#       if (length(badwells) > 0) {
#         return(paste(message.issues, paste(badwells, collapse = ', ')), sep = ": ")
#       } else {
#         return(message.noissues)
#       }
#     }
#
#     ans <- lapply(dirs, check_one)
#     names(ans) <- basename(dirs)
#     return(ans)
#   } else {
#     stop('invalid \"output\" parameter')
#   }
# }



scan_status <- function(path, output = c('print', 'return')) {

  # messages:
  message.noscans <- 'no active scans'
  message.scanactive <- 'scan is running'
  message.scanfinished <- 'scan has finished' # scan complete
  message.noissues <- 'scan complete' # scan complete
  message.issues <- 'issues in well(s)' # missing images in well(s)


  if (!dir.exists(path)) stop('no such directory')

  output <- match.arg(arg = output)

  # get file names with suffixes
  files <- list.files(path = path, pattern = '_[0-9]{3}$', full.names = TRUE, recursive = FALSE)
  # pick out directories
  dirs <- files[utils::file_test('-d', files)]

  # if no directories found
  if (length(dirs) == 0) {
    cat(message.noscans, '\n')
    if (output == 'return' || output == 'list') return(list())
  }


  badwells <- vapply(dirs, check_one, character(1), USE.NAMES = TRUE, SIMPLIFY = FALSE)

  browser()
  meth <- switch(output,
                 'return' = return(badwells),
                 'print' = {
                   for (i in seq_along(badwells)) {
                     cat()
                   }
                 })


}

# TODO
# use check_one in all cases
#
# badwells <- lapply(dirs, check_one)
# names(badwells) <- basename(dirs)
# meth <- switch(output,
#                'return' = , 'list' = return(badwells),
#                'print' = , 'console' = {
#                  for (i in seq_along(badwells)) {
#                    cat(attr(badwells[[i]], 'scan.status'), '\n')
#                    cat(badwells[[i]], '\n')
#                  }
#                },
#                stop('invalid \"output\" parameter'))


# function for checking one scan directory
check_one <- function(path) {
  images <- list.files(path = paste(path, 'data', sep = '/'), pattern = 'tif$')
  indices <- vapply(strsplit(images, '--'), function(x) x[2], character(1), USE.NAMES = FALSE)
  wells <- vapply(strsplit(images, '--'), function(x) x[1], character(1), USE.NAMES = FALSE)
  wells <- wells[order(indices)]
  wells <- factor(wells, levels = unique(wells))
  freqs <- table(wells)
  faulty <- freqs[freqs != max(freqs)]
  aclog <- utils::read.delim(paste(path, 'AcquisitionLog.dat', sep = '/'), skip = 1)
  eventful <- wells.logged(aclog)
  badwells <- c(names(faulty), eventful)
  scan.status <- if (aclog[nrow(aclog), 1] == 'ENDACQUISITION') message.scanfinished else message.scanactive
  scan.status <- paste(basename(path), ": ", scan.status, sep = "")
  attr(badwells, 'scan.status') <- scan.status
  if (length(badwells) > 0) {
    return(paste(message.issues, paste(badwells, collapse = ', ')), sep = ": ")
  } else {
    return(message.noissues)
  }
}



# function that extracts wells with logged events
wells.logged <- function(logfile) {
  # does log contain events
  events <- !identical(levels(logfile[ ,1]), c('ENDACQUISITION', 'IMAGEPOS'))
  if (events) {
    # these are the rows with events
    #logfile[which(logfile[, 1] == 'LOG'), ]
    # next row after event
    wells <- logfile[which(logfile[, 1] == 'LOG') +2, 2]
    wells <- sort(as.numeric(sub('^W=', '', wells)))
    # translate well numbers to positions
    row <- function(w) LETTERS[(w - 1) %/% 24 + 1]
    col <- function(w) (w - 1) %% 24 + 1
    positions <- paste0(row(wells), col(wells))

    return(unique(positions))
  } else {
    return(NULL)
  }
}
