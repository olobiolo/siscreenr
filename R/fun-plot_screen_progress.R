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
#' @return Nothing. A plot is printed if \code{file} is missing,
#' otherwise a 600x800 px png file is created in \code{directory}.
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
  if (length(logfiles) == 0) {
    stop('no screen log found')
  } else if (length(logfiles) == 1) {
    logfile <- logfiles
  } else {
    cat('choose a file:\n')
    for (i in seq_along(logfiles)){
      cat(i, '\t', logfiles[i])
    }
    logfile <- logfiles[readline('> ')]
  }

  # read screen log file and prepare data
  S <- utils::read.delim(logfile)
  S <- S[c('plated', 'imaged')]
  S <- S[stats::complete.cases(S), ]
  Sp <- stats::setNames(as.data.frame(table(S$plated), stringsAsFactors = F), c('plated', 'n'))
  Sp$plated <- lubridate::ymd(Sp$plated)
  Sp$plated. <- cumsum(Sp$n)
  Sp <- Sp[-which(names(Sp) == 'n')]
  Si <- stats::setNames(as.data.frame(table(S$imaged), stringsAsFactors = F), c('imaged', 'n'))
  Si$imaged <- lubridate::ymd(Si$imaged)
  Si$imaged. <- cumsum(Si$n)
  Si <- Si[-which(names(Si) == 'n')]
  # find beginning and ending dates and prepare list of all days in between
  span <- lubridate::ymd(range(c(S$plated, S$imaged)))
  days <- data.frame(day = seq(from = min(span), to = max(span), by = 1))
  days$plated <- days$day
  days$imaged <- days$day
  # expand the cumsums to missing days and gather to long format
  SS <- merge(merge(days, Sp, all = TRUE), Si, all = TRUE)
  SS <- tidyr::fill(SS, dplyr::matches('\\.$'))
  SS <- SS[c('day', 'plated.', 'imaged.')]
  names(SS) <- c('day', 'plated', 'imaged')
  SS <- data.table::melt(SS, 1, 2:3)
  names(SS) <- c('day', 'plates', 'number_of_plates')
  #
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
    grDevices::png(plot_path, 800, 600)
    suppressWarnings(print(P))
    grDevices::dev.off()
  }
}
