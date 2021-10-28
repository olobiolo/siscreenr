plotExplore <- function(x, varX, varY, varCol, color = "lightyellow") {

  checkmate::assert_data_frame(x)
  checkmate::assert_string(varX)
  checkmate::assert_string(varY)
  checkmate::assert_string(varCol)
  lapply(c(varX, varY, varCol),
         function(v) checkmate::assert_choice(v, names(x)))
  # checkmate::assert_character(colors, min.len = 1)
  checkmate::assert_string(color)

  x <- dplyr::sample_frac(x, 0.1)

  # build base label
  x[["hoverlabel"]] <- sprintf("%s: %.2g \n%s: %.2g",
                               varX, x[[varX]], varY, x[[varY]])
  # add factors to label
  ## wishlist
  toLabel <- c("plate", "replica", "well", "position", "gene_symbol")
  ## extend label recursively
  x[["hoverlabel"]] <- extend_label(x, x[["hoverlabel"]], intersect(toLabel, names(x)))
  ## replace underscores with spaces
  x[["hoverlabel"]] <- gsub("_", " ", x[["hoverlabel"]], fixed = TRUE)

  xHigh <- plotly::highlight_key(x,
                                 key = stats::reformulate(varCol),
                                 group = stringi::stri_trans_totitle(gsub("_", " ", varCol, fixed = TRUE)))

  plotBase <- plotly::plot_ly(type = "scatter", mode = "markers",
                              data = xHigh,
                              x = stats::reformulate(varX), y = stats::reformulate(varY),
                              color = I(color),
                              hoverinfo = "text", text = ~hoverlabel,
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
# ss <- sample(a$gene_symbol, 10)
# a %>% dplyr::sample_frac(0.2) %>% dplyr::filter(well_type == "sample") %>%
#   plotExplore(., varX = 'nuclei', varY = 'total_intensity_BrdU_in_BrdU', varCol = "gene_symbol", valCol = ss)







## plotting hit numbers

#' plot hit frequencies
#'
#' @param data a data frame
#' @param varScore character string; name of variable that contains aggregated hitscores
#' @param varCol character string; name of variable that encodes well types
#' @param varGrp character vector; set of variables that uniquely identify wells;
#'               can use grouping from call to \code{flag_hits}
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

  # drop unnecessary information
  vars <- c(varGrp, varCol, varScore)
  data <- unique(data.table::as.data.table(data)[, ..vars])

  axisOptionsY = NULL

  # calculate hit number
  hn <- data[, .("hit_number" = .N), by = c(varCol, varScore)]
  if (freq == "relative") {
    tot <- data[, .N, by = varCol]
    hn <- merge(hn, tot)
    hn[, hit_number := hit_number / N]

    axisOptionsY <- list(range = 0, 1)
  }

  hn[["hoverlabel"]] <- sprintf("%s: %s \n%.2g wells",
                                varCol, hn[[varCol]],
                                hn[["hit_number"]])

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
