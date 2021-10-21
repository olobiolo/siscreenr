
#' app for exploring hits
#'

library(shiny)

ui <- fluidPage(

  fileInput("file", "upload screen file", accept = ".rds, .txt, .csv, .xls, .xlsx",
            placeholder = "rds, txt, csv or Excel files"),
  uiOutput("select_variables"),
  NULL)

server <- function(input, output, session) {

  filePath <- reactive( input[["file"]]$datapath )

  scr <- reactive({
    ext <- tools::file_ext(req(filePath()))
    method <- switch(ext,
                     "rds" = readRDS,
                     "txt" = data.table::fread,
                     "csv" = data.table::fread,
                     "xls" = readxl::read_xls,
                     "xlsx" = readxl::read_xlsx,
                     function(x) validate(need(FALSE, "wrong file format")))
    file_content <- method(filePath())

    ## add some validation actions

    return(file_content)
  })


  output[["select_variables"]] <- renderUI({

    scr <- req(scr())
    numerics <- names(Filter(is.numeric, scr))
    factors <- setdiff(names(scr), numerics)

    tagList(
      selectInput("varX", "X axis", choices = c("", numerics)),
      selectInput("varY", "Y axis", choices = c("", numerics)),
      selectInput("varHighlight", "highlight variable", choices = c("", factors)),
      NULL
    )
  })

  output[["highlight"]] <- renderUI({
    values <- req(unique(scr()[[input[["varHighlight"]]]]))
    selectizeInput("valHighlight", "highlight values", choices = c("", values),
                   options = list(
                     `placeholder` = sprintf("select %s% to highlight", input[["varHighlight"]])
                   ))
  })

  ## choose color for points

  ## choose color of highlight
  ## multiple colors for multiple values?

  ## color background sections
  ## define sections...

  output[["plot"]] <- plotly::renderPlotly({
    scr <- req(scr)
    varX <- req(input[["varX"]])
    varY <- req(input[["varY"]])
    varCol <- req(input[["varHighlight"]])
    scr[["highlight"]] <- scr[[varCol]] %in% req(intput[["valHighlight"]])


    plotExplore(scr, varX, varY, varCol)

  })

}

shinyApp(ui, server)



plotExplore <- function(x, varX, varY, varCol, colors = c("lightyellow", "purple")) {

  checkmate::assert_data_frame(x)
  checkmate::assert_string(varX)
  checkmate::assert_string(varY)
  checkmate::assert_string(varCol)
  checkmate::assert_character(colors, min.len = 1)

  # build base label
  x[["hoverlabel"]] <- sprintf("%s: %.2g \n %s: %.2g \n",
                               varX, scr[[varX]], varY, scr[[varY]])
  # add factors to label
  ## wishlist
  toLabel <- c("plate", "replica", "well", "position", "well_type")
  ## extend label recursively
  x[["hoverlabel"]] <- extend_label(x, x[["hoverlabel"]], intersect(toLabel, names(x)))

  xHigh <- plotly::highlight_key(x, stats::reformulate(varCol))

  plotBase <- plotly::plot_ly(type = "scatter", mode = "markers",
                              data = xHigh,
                              x = stats::reformulate(varX), y = stats::reformulate(varY),
                              color = ~highlight, colors = colors,
                              hoverinfo = "text", text = ~hoverlabel,
                              showlegend = FALSE)

  plotLaidout <- plotly::layout(plotBase, ...)

  plotFinal <- pltoly::config(plotLaidout,
                              toImageButtonOptions = list(format = "svg"),
                              modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d"),
                              scrollZoom = TRUE, doubleClick = TRUE)

  plotHighlight <- plotly::highlight(plotFinal,
                                     on = "plotly_click", off = "plotly_doubleclick",
                                     opacityDim = 0.4,
                                     selected = attrs_selected(showlegend = TRUE))

  return(plot_highlight)

}




#' add items to hover label
#'
#' Recursively extend highlight text by adding more factors.
#'
#' This function takes a character vector \code{label} and pastes
#' each of the columns of \code{data} specified by \code{factors}.
#' It works recursively, so if \code{factors} is an empty vector,
#' it will returned the unchanged \code{label}.
#'
#' @param data a \code{data.frame}
#' @param label character vector of hover labels to be extended
#' @param factors names of columns of \code{data}
#'                to add to \code{label}; character vector
#'
#' @return A character vector of extended hover labels.
#'
#' @export
#'
extend_label <- function(data, label, factors) {

  checkmate::assert_data_frame(data)
  checkmate::assert_character(label)
  checkmate::assert_character(factors)
  checkmate::assert_choice(factors, choices = names(data))

  if (length(factors) == 0) return(label)
  label <- sprintf("%s\n %s: %s \n",
                   label, factors[1], data[[factors[1]]])
  factors <- factors[-1]
  extend_label(data, label, factors)
}
