#' plot campaign progress over time
#'
#' Create a barplot that shows how many plates were created and imaged
#' over the course of the screening campaign.
#'
#' This function will search \code{directory} for a file that matches the pattern
#' "screenlog". Having found the file it will read plating and imaging dates
#' and plot the numbers of plates corresponding to those dates.
#' A barplot will be created with \code{ggplot}, which can be saved to a png file.
#'
#' A subdirectory called "results" will be created, if absent, to save the plot.
#'
#' @param directory analysis master directory, where screen log file is located;
#'                  defaults to current working directory
#' @param file optional path to print the plot to;
#'             will be created in the "results" directory;
#'             set to NULL to send plot to current graphics device
#'
#' @return A \code{ggplot} object if \code{plot} is NULL or the file path otherwise.
#'
#' @export
#'

plot_screen_progress <- function(directory, file) {
  # check for directory
  if (missing(directory)) directory <- getwd() else
    if (!dir.exists(directory)) stop('directory not found')
  # go to directory and check for "results" subdirectory
  .home <- setwd(directory)
  on.exit(setwd(.home))
  if (missing(file) & !dir.exists('results')) dir.create('./results')
  if (!missing(file) && is.null(file) && !dir.exists('results')) dir.create('./results')

  # check for screen log file
  logfiles <- list.files(pattern = 'screenlog')
  if (length(logfiles) == 0L) {
    stop('no screen log found')
  } else if (length(logfiles) == 1L) {
    logfile <- logfiles
  } else {
    cat('choose a file:\n')
    for (i in seq_along(logfiles)){
      cat("[", i, "]: ", logfiles[i], "\n", sep = "")
    }
    logfile <- logfiles[as.integer(readline('> '))]
  }

  # read screen log file and prepare data
  S <- utils::read.delim(logfile)
  S <- S[c('plated', 'imaged')]
  S <- S[stats::complete.cases(S), ]

  # count number of occurrences of plating dates
  Sp <- structure(as.data.frame(table(S$plated), stringsAsFactors = FALSE),
                  names = c('plated', 'n'))
  Sp$plated. <- cumsum(Sp$n)
  Sp['n'] <- NULL
  # convert dates to dates
  Sp$plated <- lubridate::ymd(Sp$plated)

  # repeat for imaging dates
  Si <- structure(as.data.frame(table(S$imaged), stringsAsFactors = FALSE),
                  names = c('imaged', 'n'))
  Si$imaged. <- cumsum(Si$n)
  Si['n'] <- NULL
  Si$imaged <- lubridate::ymd(Si$imaged)

  # find beginning and ending dates and prepare list of all days in between
  span <- lubridate::ymd(range(c(S$plated, S$imaged)))
  days <- data.frame(day = seq(from = min(span), to = max(span), by = 1))
  days$plated <- days$day
  days$imaged <- days$day
  # expand the cumsums to missing days and gather to long format
  SS <- merge(merge(days, Sp, all = TRUE), Si, all = TRUE)
  cols <- grep('\\.$', names(SS), value = TRUE)
  # fill missing values
  SS[cols] <- lapply(SS[cols], fill_NAs)

  # reformat data
  SS <- SS[c('day', 'plated.', 'imaged.')]
  names(SS) <- c('day', 'plated', 'imaged')
  SS <- data.table::melt(data.table::as.data.table(SS),
                         id.vars = "day", measure.vars = c('plated', 'imaged'),
                         variable.name = "plates", value.name = "number_of_plates")
  # build plot
  P <-
    ggplot2::ggplot(SS, ggplot2::aes_string(x = 'day', y = 'number_of_plates', fill = 'plates')) +
    ggplot2::geom_bar(stat = 'identity', position = ggplot2::position_identity(), width = 1) +
    ggplot2::scale_fill_manual(values = c('limegreen', 'cornflowerblue')) +
    ggplot2::ggtitle('material accumulation over the course of the screen') +
    ggplot2::ylab('number of plates')

  if (!missing(file) && is.null(file)) {
    print(P)
  } else {
    if (missing(file)) {
      # extract suffix from log file name to append to result file name
      # i.e. drop the word "screenlog"
      suffix <- sub('txt', 'png', sub('screenlog', '', logfile))
      plot_path <- paste0('results/screen_progress', suffix)
    } else {
      plot_path <- file
    }
    ggplot2::ggsave(filename = plot_path, plot = P, device = "png", width = 8, height = 6)
  }
}

# internal function to fill missing values in vector
fill_NAs <- function(x) {
  ind <- match(NA, x)
  if (is.na(ind)) return(x)
  if (ind == 1) {
    x[ind] <- 0L
  } else {
    x[ind] <- x[ind-1]
  }
  fill_NAs(x)
}

