#' @title build screen object
#'
#' @description
#' Read screen log and layout, and collate all data into a single object.
#' This is the first stage of the analysis. Restrictions on log/layout files are discussed here.
#' The function tries to cope with some cases and also runs checks on data completion.
#' There are many communications that the function will normally print; these can be silenced.
#' The final object is returned invisibly as printing it all would be counterproductive.
#'
#' @details
#' The function first checks the data files present in the data directory against
#' the plates logged in the screen log.
#' Data file names must begin with the full plate names as given in the screen log.
#' Suffixes are allowed but must be separated by \code{_}. They will be discarded.
#' Unexpected and missing plates are reported.
#'
#' Once the plate list has been compiled, the layout file is loaded and all present
#' data files that correspond to logged plates are read and wrapped into a single data frame.
#'
#' The plate number and screen log data are used to obtain dates of the plates'
#' preparation, plating, and imaging.
#'
#' There is quite a lot of printed communication the function does, hence the \code{verbose} argument.
#'
#' @param logfile path to screen log file; see \code{Log file}
#' @param layout path to layout file that describes well types; see \code{Layout file}
#' @param datadir path to directory where data files are stored
#' @param rem.col columns to remove, given either as character vector or as vector of numeric indices;
#'                input 0 to remove last column;
#' @param zero.to.NA logical flag whether to convert all 0 values to NAs
#' @param wells name of column that stores well numbers; will be renamed to "wells"
#' @param verbose logical flag whether to print all communications
#'
#' @return an invisible \code{data.frame}
#'
#' @section Log file:
#' The screen log this must be a tab delimited file and contain columns:
#' \code{plateno}, \code{plated}, \code{imaged}. Any number of other columns is acceptable.
#' The plate number must be contained verbatim within the corresponding result file name.
#' Any suffixes following an underscore will be dropped.
#'
#' @section Layout file:
#' The layout file must be a tab delimited file and contain columns:
#' (numeric) \code{well} and \code{well.type} or \code{well_type}.
#' Other columns may be necessary if there are multiple layouts,
#' e.g. for different plating days or different plate types.
#' Maintain a \href{https://en.wikipedia.org/wiki/Wide_and_narrow_data}{narrow data format}.
#'
#' The file should also specify a well's coordinates on a plate, either as
#' \code{position}, e.g. "E02", or \code{row} and \code{column}.
#' By default ScanR sorts and numbers wells by row,
#' in contrast to the default filling of matrices in R.
#' Thus, \code{\link{normalize}} sorts the data frame by column and row before
#' running the median polish and if these columns are absent it throws an error.
#'
#' @section Dropping columns:
#' Columns are removed upon request. This option exists for two reasons.
#' First, ScanR always adds an empty column at the end called "X". Second, the
#' second column in every exported file,
#' called "Description", contains the names of groups of wells.
#' Since we only ever export data by well and not by group,
#' this may cause problems if not all wells are scanned.
#' Also we load layout and annotation separately, the column is redundant anyway.
#'
#' @section Dependencies:
#' Data files are loaded with \code{utils::read.delim}.
#' Some operations are done with \code{tidyr}.
#' A number of internal functions are used.
#'
#' @export
#'

build_screen <- function(logfile, layout, datadir = './data/', rem.col,
                         zero.to.NA = FALSE, wells = 'Index', verbose = TRUE) {
  # check arguments
  if (!file.exists(logfile)) stop('logfile not found')
  if (!file.exists(layout)) stop('layout file not found')
  if (!dir.exists(datadir)) stop('data directory not found')
  if (!missing(rem.col)) {
    if (!is.numeric(rem.col) & !is.character(rem.col)) {
      stop ('"rem.col" must be either a numeric or a character vector')
    }
  }

  # load log and layout and compare logged vs filed plates
  if (verbose) cat('tallying plates... \n')
  screenlog <- utils::read.delim(logfile, stringsAsFactors = FALSE)
  plates.logged <- screenlog[, 1]
  if (length(plates.logged) == 0) stop('no plates logged(?); check screen log file')
  data.files <- list.files(path = datadir)
  plates.filed <- sapply(data.files, function(x) strsplit(x, split = '_')[[1]][1])
  if (length(plates.filed) == 0) stop('no result files')
  # load a random file to test "wells" argument
  test_file <- utils::read.delim(sample(list.files(datadir, full.names = TRUE), 1), stringsAsFactors = FALSE)
  if (!is.element(wells, names(test_file))) stop('column "', wells, '" not found; check "wells" argument')
  if (verbose) {
    cat(length(plates.filed), 'result files found: \n')
    print(cbind(sort(as.vector(plates.filed))))
  }

  # check for missing/excess result files
  if (verbose && !setequal(plates.logged, plates.filed)) {
    cat('\ndetected result files do not match the screen log\n')
    plates.missing <- setdiff(plates.logged, plates.filed)
    plates.excess <- setdiff(plates.filed, plates.logged)
    cat(length(plates.missing), 'result files missing: \n')
    print(cbind(sort(plates.missing)))
    cat(length(plates.excess), 'excess result files: \n')
    print(cbind(sort(plates.excess)))
  }

  # load layout
  if (verbose) cat('loading layout(s)... \n')
  lay <- build_screen.layout(layout)

  # build screen as data frame
  if (verbose) cat('building screen... \n')
  plates <- names(plates.filed[plates.filed %in% plates.logged])
  if (length(plates) == 0) stop('no result files to collate')
  prescr <- lapply(plates, build_screen.load_plates, datadir)
  if (verbose) cat('collating', length(plates), 'plates', '\n')
  scr <- do.call(rbind, prescr)

  # removing columns if desired
  if (!missing(rem.col)) {
    if (verbose) cat('removing columns... \n')
    scr <- build_screen.remove_columns(scr, rem.col)
  }

  # read and reformat additional information
  names(scr)[which(names(scr) == wells)] <- 'well'
  scr <- tidyr::separate(scr, 'filename', c('plateno','extension'), sep = '_')
  scr <- merge(screenlog, scr, by = 'plateno', all.x = FALSE, all.y = TRUE)
  scr <- scr %>%
    tidyr::separate('plateno', c('plate', 'prepared', 'screen', 'replica'), sep = '\\.') %>%
    tidyr::separate('replica', c('plate_type', 'number'), sep = 1)
  scr$plate <- as.numeric(gsub('[A-Z]', '', scr$plate))
  scr <- plate.type.converter(scr)
  scr <- tidyr::unite(scr, 'replica', 'replica', 'number', sep = '')
  scr <- merge(lay, scr, all.x = TRUE, all.y = FALSE)
  scr[c('plated', 'prepared', 'imaged')] <- lapply(scr[c('plated', 'prepared', 'imaged')], lubridate::ymd)
  scr$extension <- NULL

  # change zeros to NAs if required
  if (zero.to.NA) {
    if (verbose) cat('replacing zeros... \n')
    scr <- build_screen.replace_zeros(scr)
  }

  # reorder by plate number, replica number and well number
  if (verbose) cat('reordering... \n')
  scr <- scr[order(scr$plate, scr$replica, scr$plated, scr$imaged, scr$column, scr$row), ]

  if (verbose) cat('\nready! \n')
  invisible(scr)
}


### internal functions


# load layout file and check format
build_screen.layout <- function(layout) {
  if (!file.exists(layout)) stop('from build_screen: layout file not found')
  lay <- utils::read.delim(file = layout, stringsAsFactors = FALSE)
  # check adjust format of the layout file
  if (!is.numeric(lay$well)) stop('from build_screen: column "well" in layout file must be numeric')
  # check column names for position specification
  lay.colnames <- names(lay)
  if (!all(is.element(c('row', 'column'), lay.colnames))) {
    if (!is.element('position', lay.colnames)) {
      warning('from build_screen:
              it seems well position (row and column) is not defined in the layout file
              this may cause problems down the line')
    } else {
      lay <- tidyr::separate(lay, col = 'position', sep = 1, into = c('row', 'column'),
                             remove = F, convert = F)
    }
  }
  # if there is a column called "date" but not one called "plated", it will be renamed to such
  if (is.element('date', lay.colnames) & !is.element('plated', lay.colnames)) {
    names(lay)[lay.colnames == 'date'] <- 'plated'
  }
  return(lay)
}

# load a file, add file name to the resulting data frame and return
build_screen.load_plates <- function(x, datadir) {
  filename <- paste0(datadir, '/', x)
  plate.loaded <- utils::read.delim(filename, stringsAsFactors = FALSE)
  plate.loaded$filename <- x
  return(plate.loaded)
}

# remove columns
build_screen.remove_columns <- function(x, rem.col) {
  cols <- colnames(x)
  if (is.factor(rem.col)) rem.col <- as.character(rem.col)
  if (is.character(rem.col)) {
    # columns to remove that are indeed present
    rem.present <- rem.col[is.element(rem.col, cols)]
    # coumns to remove that are absent
    rem.absent <- rem.col[!is.element(rem.col, cols)]
    # throw message if some requests absent
    if (length(rem.absent) > 0) {
      message('from build_screen: the following columns were not found: ',
              paste(rem.absent, collapse = ', '))
    }
    # remove column by setting them to NULL
    if (length(rem.present) > 0) {
      x[rem.present] <- NULL
    }
  } else if (is.numeric(rem.col)) {
    # convert 0 to number of penultimate column
    if (0 %in% rem.col) rem.col[rem.col == 0] <- length(cols) - 1
    x[rem.col] <- NULL
  }
  return(x)
}

# change zeros to NAs
build_screen.replace_zeros <- function(x) {
  verbose <- get('verbose', envir = parent.frame(1))
  na.count.before <- sum(is.na(x))
  zero.count <- sum(x == 0, na.rm = TRUE)
  x[x == 0] <- NA
  na.count.after <- sum(is.na(x))
  if (verbose) {
    cat(' ', na.count.before, 'NAs identified \n')
    cat(' ', zero.count, 'zeros found and replaced in total \n')
    cat(' ', na.count.after, 'NAs now present \n')
  }
  return(x)
}
