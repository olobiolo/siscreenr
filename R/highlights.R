#' @name highlights
#'
#' @title highlight genes
#'
#' @description
#' Highlight selected genes on a scatter plot of all screen data.
#'
#' @details
#' Given a screen object (a \code{data.frame}), produce a scatter plot of two variables.
#' The background is divided into sections with colors signifying the hit status of respective
#' plot regions. Selected points (genes) are plotted with a different color
#' and their gene symbols are added as text labels.
#'
#' Only sample wells will be plotted.
#'
NULL

#' @describeIn highlights
#' creates scatter plot on colored background
#'
#' @param canvas ggplot object produced by \code{color_sections}
#' @param data a \code{data.frame}
#' @param genes a character vector of \strong{gene symbols} to highlight
#' @param x.variable,y.variable names of variables to plot, given as character strings
#' @param color.points color to use for non-highlighted genes
#' @param color.highlight color to use for highlighted genes
#' @param plot.title character string passed to \code{link[ggplot2]{ggtitle}}
#'
#' @return a ggplot object
#'
#' @export
#'
highlight_genes <- function(canvas, data, genes, x.variable, y.variable,
                            color.points = 'lightyellow', color.highlight = 'purple',
                            plot.title = '') {
  # check canvas
  if (missing(canvas)) stop('"canvas" missing, run colour_sections() first')
  if (!missing(canvas) & !inherits(canvas, 'ggplot')) stop('"canas" must be a ggplot object')
  # check data
  if(!is.element('well_type', names(data))) stop('missing column: "well_type"')
  if(!is.element('gene_symbol', names(data))) stop('missing column: "gene_symbol"')
  if(!is.element(x.variable, names(data))) stop('missing column: ', x.variable)
  if(!is.element(y.variable, names(data))) stop('missing column: ', y.variable)
  # check genes
  if (!any(genes %in% data$gene_symbol)) stop('"data" contains none of "genes"')

  # in case the names are separated by periods rather than underscores
  colnames(data) <- gsub('\\.', '_', names(data))

  # filter out non-sample data and add column for text labels of selected genes
  data <- data[data$well_type == 'sample', , drop = FALSE]
  data$gene_symbol <- as.character(data$gene_symbol)
  data$labels <- ifelse(data$gene_symbol %in% genes, data$gene_symbol, 'sample wells')

  # subset genes to highlight
  thegenes <- data[data$labels != 'sample wells', , drop = FALSE]

  canvas +
    ggplot2::geom_point(inherit.aes = F, data = data,
                        ggplot2::aes_string(x = x.variable, y = y.variable),
                        pch = 16, size = 1.5, alpha = 0.75, color = color.points) +
    ggplot2::geom_point(inherit.aes = F, data = thegenes,
                        ggplot2::aes_string(x = x.variable, y = y.variable),
                        size = 2.5, color = color.highlight) +
    ggplot2::geom_text(inherit.aes = F, data = thegenes,
                       ggplot2::aes_string(x = x.variable, y = y.variable, label = labels),
                       size = 3, color = color.highlight, hjust = -.1, vjust = -.1) +
    ggplot2::ggtitle(plot.title) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = 'hit status')) +
    ggplot2::coord_cartesian(xlim = c(-5, 5), ylim = c(-5, 5))
}

#' @describeIn highlights
#' colors sections of the plot background according to hit class
#'
#' @param x.treshold,y.treshold,outer.limit coordinates that determine section borders
#'                                          x.treshold and y.treshold may be determined
#'                                          as numeric vectors of length one or two
#' @param categories names for sections, in order to appear in the plot legend
#' @param category.layout matrix that maps categories to sections; numbers refer to categories
#' @param colpal colors to assign to sections, in the same order as categories
#'
#' @return a ggplot object with \code{geom_polygon} applied
#'
#' @export
#'
color_sections <- function(x.treshold = c(-2,2), y.treshold = c(-2,2), outer.limit = 100,
                           categories = c('non-hit', 'negative interaction',
                                          'positive interaction', 'doubtful viability'),
                           category.layout = matrix(c(4,4,4, 2,1,3, 2,1,3), nrow = 3, ncol = 3),
                           colpal = c('steelblue1', 'springgreen2', 'indianred2', 'grey85')) {
  cat('for reference, the following categories will be displayed:\n')
  print(cbind(categories))

  # check arguments
  if (length(categories) != length(colpal))
    stop('"categories" and "colpal" must be of the same length')
  if (length(category.layout) != 9)
    stop('"category.layout" must be of length 9')

  # construct data frames for drawing polygons
  # names of zones (9)
  zones <- c('topleft', 'midleft', 'bottomleft',
             'top', 'middle', 'bottom',
             'topright', 'mitright', 'bottomright')
  # layout of categories
  hit_statuses <- categories[category.layout]
  # map categories to zones
  left <- data.frame(zone = zones, hit_status = hit_statuses)
  # construct coordinates of zone corners
  coordinates <- data.frame(zone = rep(zones, each = 4),
                            x = c(-outer.limit, -outer.limit, x.treshold[1], x.treshold[1],
                                  -outer.limit, -outer.limit, x.treshold[1], x.treshold[1],
                                  -outer.limit, -outer.limit, x.treshold[1], x.treshold[1],
                                  x.treshold[1], x.treshold[1], x.treshold[2], x.treshold[2],
                                  x.treshold[1], x.treshold[1], x.treshold[2], x.treshold[2],
                                  x.treshold[1], x.treshold[1], x.treshold[2], x.treshold[2],
                                  x.treshold[2], x.treshold[2], outer.limit, outer.limit,
                                  x.treshold[2], x.treshold[2], outer.limit, outer.limit,
                                  x.treshold[2], x.treshold[2], outer.limit, outer.limit),
                            y = c(outer.limit, y.treshold[2], y.treshold[2], outer.limit,
                                  y.treshold[2], y.treshold[1], y.treshold[1], y.treshold[2],
                                  y.treshold[1], -outer.limit, -outer.limit, y.treshold[1],
                                  outer.limit, y.treshold[2], y.treshold[2], outer.limit,
                                  y.treshold[2], y.treshold[1], y.treshold[1], y.treshold[2],
                                  y.treshold[1], -outer.limit, -outer.limit, y.treshold[1],
                                  outer.limit, y.treshold[2], y.treshold[2], outer.limit,
                                  y.treshold[2], y.treshold[1], y.treshold[1], y.treshold[2],
                                  y.treshold[1], -outer.limit, -outer.limit, y.treshold[1]))
  # combine the two data frames and reorder categories
  right <- merge(left, coordinates, all = TRUE)
  right$hit_status <- factor(right$hit_status, levels = categories)

  # plot color panels (this is returned)
  ggplot2::ggplot(data = right, ggplot2::aes_string(
    x = 'x', y = 'y', group = 'zone', fill = 'hit_status')) +
    ggplot2::geom_polygon() +ggplot2::scale_fill_manual(values = colpal) +
    ggplot2::coord_cartesian(xlim = c(-5, 5), ylim = c(-5, 5)) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = 'hit status'))
}

