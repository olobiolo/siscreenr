
#' exploration plot
#'
#' Create scatter plot with the ability to highlight subsets of data.
#'
#' All points are drawn in the same \code{color}.
#' Data subsets can be created based on values of \code{varCol}
#' and highlighted with a different (selected) color.
#' This is done by clicking on a point or selecting a value of \code{varCol}
#' with the selectize tool. Hold \code{Shift} to add more values to the subset.
#'
#' @param data a \code{data.frame}
#' @param varX,varY character strings; variables to assign to axes
#' @param varCol character string; variable to highlight by, typically gene symbol
#' @param color character string; default color for unhighlighted points
#'
#' @return A \code{plotly} object.
#'
#' @export
#'
plotExplore <- function(data, varX, varY, varCol, color = "lightyellow") {

  checkmate::assert_data_frame(data)
  checkmate::assert_string(varX)
  checkmate::assert_string(varY)
  checkmate::assert_string(varCol)
  lapply(c(varX, varY, varCol),
         function(v) checkmate::assert_choice(v, names(data)))
  checkmate::assert_string(color)

  # drop data.table class
  if (inherits(data, "data.table")) data <- as.data.frame(data)

  # build base label
  data[["hoverlabel"]] <- sprintf("%s: %.2g \n%s: %.2g",
                               varX, data[[varX]], varY, data[[varY]])
  # add factors to label
  ## wishlist
  toLabel <- c("plate", "replica", "well", "position", "gene_symbol")
  ## extend label recursively
  data[["hoverlabel"]] <- extend_label(data, data[["hoverlabel"]], intersect(toLabel, names(data)))
  ## replace underscores with spaces
  data[["hoverlabel"]] <- gsub("_", " ", data[["hoverlabel"]], fixed = TRUE)

  xHigh <- plotly::highlight_key(data,
                                 key = stats::reformulate(varCol),
                                 group = stringi::stri_trans_totitle(gsub("_", " ", varCol, fixed = TRUE)))

  plotBase <- plotly::plot_ly(type = "scatter", mode = "markers",
                              data = xHigh,
                              x = stats::reformulate(varX), y = stats::reformulate(varY),
                              color = I(color),
                              hoverinfo = "text", text = stats::reformulate("hoverlabel"),
                              showlegend = FALSE)

  plotLaidout <- plotly::layout(plotBase,
                                paper_bgcolor = "#e0e0e0", plot_bgcolor = "#e0e0e0",
                                xaxis = list(type = "linear"),
                                yaxis = list(type = "linear"))

  plotFinal <- plotly::config(plotLaidout,
                              toImageButtonOptions = list(format = "svg"),
                              modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d"),
                              scrollZoom = TRUE, doubleClick = TRUE)

  plotHighlight <- plotly::highlight(plotFinal,
                                     on = "plotly_click", off = "plotly_doubleclick",
                                     opacityDim = 1, color = RColorBrewer::brewer.pal(7, "Accent"),
                                     selectize = TRUE, dynamic = TRUE,
                                     selected = plotly::attrs_selected(showlegend = TRUE))

  return(plotHighlight)

}
# a %>% dplyr::sample_frac(0.2) %>% dplyr::filter(well_type == "sample") %>%
#   plotExplore(., varX = 'nuclei', varY = 'total_intensity_BrdU_in_BrdU', varCol = "gene_symbol")






#' plot hit frequencies
#'
#' Plot number of hits (and hit tiers, if applicable) per well type.
#'
#' Creates a barplot that shows the frequencies of aggregated hitscores
#' separated by well types.
#'
#' @param data a data frame
#' @param varScore character string; name of variable that contains aggregated hitscores
#' @param varCol character string; name of variable that encodes well types
#' @param varGrp character vector; set of variables that uniquely identify wells;
#'               can use grouping from call to \code{\link{flag_hits}}
#' @param freq character string specifying whether the Y axis should list
#'             the absolute number of wells or a fraction of wells
#'             that fall into particular bins
#'
#' @return A \code{plotly} object.
#'
#' @export
#'
plotHits <- function(data, varScore, varCol = "well_type", varGrp = c("plate", "well"),
                     freq = c("absolute", "relative")) {

  checkmate::assert_data_frame(data)
  checkmate::assert_string(varScore)
  checkmate::assert_string(varCol)
  checkmate::assert_character(varGrp)
  lapply(c(varScore, varCol, varGrp),
         function(v) checkmate::assert_choice(v, names(data)))
  freq <- match.arg(freq)

  # drop data.table class
  if (inherits(data, "data.table")) data <- as.data.frame(data)

  # drop unnecessary information
  vars <- c(varGrp, varCol, varScore)
  data <- unique(data[vars])

  axisOptionsY = NULL

  # calculate hit number
  hn <- aggregate(data[varCol], by = data[c(varCol, varScore)], FUN = length)
  names(hn)[length(names(hn))] <- "hit_number"

  # convert hit number to fraction of the varCol class
  if (freq == "relative") {
    tot <- aggregate(data[varScore], by = data[varCol], FUN = length)
    names(tot)[length(names(tot))] <- "total"
    hn <- merge(hn, tot)
    hn$hit_number <- hn$hit_number / hn$total

    axisOptionsY <- list(range = 0, 1)
  }

  # build hover label (relative frequencies will show as percentage)
  hn[["hoverlabel"]] <- switch(freq,
                               "absolute" = sprintf("%s: %s \n%i wells",
                                                    varCol, hn[[varCol]],
                                                    hn[["hit_number"]]),
                               "relative" = sprintf("%s: %s \n%1.2g%% wells",
                                                    varCol, hn[[varCol]],
                                                    hn[["hit_number"]] * 100))

  plotBase <- plotly::plot_ly(type = "bar",
                              data = hn,
                              x = stats::reformulate(varScore), y = stats::reformulate("hit_number"),
                              color = stats::reformulate(varCol), colors = "Set3",
                              hoverinfo = "text", text = stats::reformulate("hoverlabel"))

  plotLaidout <- plotly::layout(plotBase,
                                yaxis = axisOptionsY)

  return(plotLaidout)

}

# plotHits(hss[plate_type == "test"], varScore = paste(varss[1], "sum", sep = "_"))








#' plot plate as heatmap
#'
#' Plate view of a variable.
#'
#' Creates a plate view: a heatmap where tiles correspond to wells.
#' Works with continuous as well as discrete variables, meaning
#' \code{varZ} can be numeric, character or factor.
#'
#' @param data a \code{data.frame}
#' @param varZ,varX,varY character strings specifying variables to plot
#'
#' @return A \code{plotly} object.
#'
#' @export
#'
plotPlate <- function(data, varZ, varX = "column", varY = "row") {

  checkmate::assert_data_frame(data)
  checkmate::assert_string(varX)
  checkmate::assert_string(varY)
  checkmate::assert_string(varZ)
  lapply(c(varX, varY, varZ),
         function(x) checkmate::assert_choice(x, names(data)))

  # drop data.table class
  if (inherits(data, "data.table")) data <- as.data.frame(data)

  # convert axis variables to factors
  data[[varX]] <- as.factor(data[[varX]])
  data[[varY]] <- factor(data[[varY]], levels = rev(unique(data[[varY]])))

  # prepare base label
  data[["hoverlabel"]] <- sprintf("%s: %s \n%s: %s \n",
                                  varY, data[[varY]], varX, data[[varX]])


  if (is.character(data[[varZ]])) data[[varZ]] <- as.factor(data[[varZ]])

  # depending on data type
  if (is.numeric(data[[varZ]])) {
    colorScale <- NULL
    colorBar <- NULL

    gapX <- 0
    gapY <- 0
    plotColor <- "#FFFFFF"

    data[["hoverlabel"]] <- sprintf("%s%s: %.2g", data[["hoverlabel"]], varZ, data[[varZ]])

  }
  else if (is.factor(data[[varZ]])) {
    # internals for constructing discrete color scale
    ## prepare vector of breaks for colorbar
    f1 <- function(x) {
      ans <- rep(seq(from = 0, to = 1, by = 1/length(x)), each = 2)
      ans <- ans[-length(ans)][-1]
      return(ans)
    }
    ## prepare vector of colors
    f2 <- function(x) {
      rep(RColorBrewer::brewer.pal(length(x), "RdYlBu"), each = 2)
    }
    ## prepare vector of tick positions
    f3 <- function(x) {
      s <- seq_along(x)
      offsets <- rep(1/length(x), length(x))
      signs <- vector('integer', length(offsets))
      signs[s < median(s)] <- 1L
      signs[s > median(s)] <- -1L
      mults <- abs(s - median(s))
      ans <- s + offsets * signs * mults
      return(ans)
    }

    # do the deed
    data[[varZ]] <- factor(data[[varZ]], levels = rev(levels(data[[varZ]])))
    levs <- levels(data[[varZ]])
    colorScale <- list(z = f1(levs), color = f2(levs))
    colorBar <- list(tickvals = f3(levs), ticktext = levs)

    data[["varZlabel"]] <- as.character(data[[varZ]])
    data[[varZ]] <- as.integer(data[[varZ]])
    data[["hoverlabel"]] <- sprintf("%s%s: %s", data[["hoverlabel"]], varZ, data[["varZlabel"]])

    gapX <- 2
    gapY <- 2
    plotColor <- "#E0E0E0"

  } else {
    stop("varZ is of unsupported type: ", typeof(data[[varZ]]))
  }


  plotBase <- plotly::plot_ly(type = "heatmap",
                              x = data[[varX]], y = data[[varY]], z = data[[varZ]],
                              hoverinfo = "text", text = data[["hoverlabel"]],
                              colors = "RdYlBu", showscale = TRUE,
                              colorscale = colorScale, colorbar = colorBar,
                              xgap = gapX, ygap = gapY)

  plotLaidout <- plotly::layout(plotBase,
                                title = varZ,
                                plot_bgcolor = plotColor)
  plotFinal <- plotLaidout

  return(plotFinal)

}


#
# # notes for trelliscoping:
# dataset %>%
#   tidyr::nest(data = -plate) %>%
#   dplyr::mutate(panel = trelliscopejs::map_plot(data, plotPlate, varZ = "val")) %>%
#   trelliscopejs::trelliscope(title = "plates")
# # nesting variables will come up as key in trelliscope, allow filtering
# # first capture Z range for whole data set and add plotly::colorbar to map_plot call
# # x, y, z must be parameters, but can be passed directly to plotPlate
#
#
