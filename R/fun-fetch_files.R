#' fetch ScanR result/export files
#'
#' Copy all ScanR result files, chosen parameter data files, and the screen log
#' into a single location.
#'
#' The master directory specified by \code{where.from} will contain scan directories,
#' which may contain exported result files stored in "Population Data" subdirectories.
#' They may also contain parameter data files, which are named
#' "ParameterData_\code{object}.txt".
#' All result files and parameter data files where object name matches \code{object}
#' will be copied to the \code{where.to} directory. Hidden files will be omitted.
#'
#' Names of scan directories, which are usually plate numbers,
#' are added to all respective file names.
#'
#' If the master directory contains files whose names match regular expressions
#' "screenlog" and "layout", they will also be copied.
#'
#' @param where.from directory where all screen data is stored;
#'                   defaults to current working directory,
#'                   otherwise must be an existing directory
#' @param where.to directory where all files will be copied;
#'                 defaults to current working directory,
#'                 otherwise may be any directory;
#'                 a non-existing one will be created
#' @param object regular expression that defines which ParameterData files to copy;
#'               defaults to Main; set to NULL to skip ParameterData files
#' @param verbose logical flag; specifies whether to report progress
#'
#' @export

fetch_files <- function(where.from, where.to, object = 'Main', verbose = TRUE) {

  if (verbose) cat("FETCHING SCREEN FILES", "\n")

  # obtain source directory
  if (missing(where.from)) where.from <- getwd()
  where.from <- paste0(where.from, '/')
  where.from <- gsub("//", "/", where.from, fixed = TRUE)
  if (verbose) cat("source directory:", where.from, "\n")
  if (!dir.exists(where.from)) stop("source directory not found")

  # obtain target directory
  if (missing(where.to)) where.to <- getwd()
  where.to <- normalizePath(paste0(where.to, '/'), winslash = '/', mustWork = FALSE)
  where.to <- paste0(where.to, '/')
  where.to <- gsub("//", "/", where.to, fixed = TRUE)
  if (verbose) cat("target directory:", where.to)
  # create directory if necessary
  if (!dir.exists(where.to)) {
    if (verbose) cat(" (creating)", "\n\n")
    failed <- dir.create(where.to)
    if (failed) stop("could not create directory ", where.to)
  } else {
    cat("\n\n")
  }
  # prepare subdir names
  where.to.data <- paste0(where.to, 'data/')
  where.to.paramdata <- paste0(where.to, 'parameter data/')

  # copy log file
  if (verbose) cat("screen log file(s)")
  logfile <- list.files(path = where.from, pattern = 'screenlog', full.names = TRUE)
  if (length(logfile) > 0) {
    if (verbose) cat(" located")
    if (identical(where.from, where.to)) {
      if (verbose) cat("\n\n")
    } else {
      if (verbose) cat(", copying", "\n\n")
      newpath <- paste0(where.to, basename(logfile))
      file.copy(from = logfile, to = newpath, overwrite = TRUE)
    }
  } else {
    if (verbose) cat(" not found", "\n\n")
  }

  # copy layout file
  if (verbose) cat("layout file(s)")
  layoutfile <- list.files(path = where.from, pattern = 'layout', full.names = TRUE)
  if (length(layoutfile) > 0) {
    if (verbose) cat(" located")
    if (identical(where.from, where.to)) {
      if (verbose) cat("\n\n")
    } else {
      newpath <- paste0(where.to, basename(layoutfile))
      success <- file.copy(from = layoutfile, to = newpath, overwrite = TRUE)
      if (verbose && all(success)) cat(", copying", "\n\n")
    }
  } else {
    if (verbose) cat(" not found", "\n\n")
  }

  # get all scan directories
  if (verbose) cat("scan directories")
  dirs <- list.dirs(path = where.from, full.names = FALSE, recursive = FALSE)
  # omit known additional directories
  dirs <- setdiff(dirs, c(basename(where.to), "data", "parameter data"))
  if (length(dirs) > 0) {
    if (verbose) cat(" located", "\n\n")
    # do the deed
    lapply(dirs, get_from_one, where.from, where.to.data, where.to.paramdata, object, verbose)
  } else {
    if (verbose) cat(" not found", "\n\n")
  }

  if (verbose) cat("FINISHED", "\n\n")

}

get_from_one <- function(dir, where.from, where.to.data, where.to.paramdata, object, verbose) {

  if (verbose) cat("processing directory:", dir, "\n")

  # copy parameter data files
  if (!is.null(object)) {
    if (verbose) cat("\t", "parameter file(s)")
    paramfiles <- list.files(path = paste(where.from, dir, sep = '/'), pattern = object, full.names = TRUE)
    if (length(paramfiles) > 0) {
      if (!dir.exists(where.to.paramdata)) {
        failed <- dir.create(where.to.paramdata)
        if (failed) stop("could not create directory ", where.to.paramdata)
      }
      if (verbose) cat(" located")
      newpaths <- sub('(ParameterData_)(.*)(.txt)',
                      paste0(where.to.paramdata, '\\1\\2_', dir, '\\3'),
                      basename(paramfiles))
      success <- file.copy(from = paramfiles, to = newpaths, overwrite = TRUE)
      if (verbose && all(success)) cat(" and copied", "\n")
    } else {
      if (verbose) cat(" not found", "\n")
    }
  }

  # copy population result files
  popdir <- paste(where.from, dir, 'Population Results', sep = '/')
  if (verbose) cat("\t", "population result files")
  if (dir.exists(popdir)) {
    datafiles <- list.files(path = popdir, full.names = TRUE)
    if (length(datafiles) > 0) {
      if (!dir.exists(where.to.data)) {
        failed <- dir.create(where.to.data)
        if (failed) stop("could not create directory ", where.to.paramdata)
      }
      if (verbose) cat(" located")
      newpaths <- paste0(where.to.data, dir,'_', basename(datafiles))
      success <- file.copy(from = datafiles, to = newpaths, overwrite = TRUE)
      if (verbose && all(success)) cat(" and copied", "\n")
    } else {
      if (verbose) cat(" not found", "\n")
    }
  } else {
    if (verbose) cat("\t\t", "not found", "\n")
  }
  if (verbose) cat("\t", "done", "\n\n")
}
