
#' app for exploring hits
#'

library(shiny)

ui <- fluidPage(

  fileInput("file", "upload screen file", accept = ".rds, .txt, .csv, .xls, .xlsx",
            placeholder = "rds, txt, csv or Excel files"),
  uiOutput("select_variables"),
  uiOutput("select_values"),
  plotly::plotlyOutput("plot"),
  # actionButton("pick_color", "change point color"),
  colourpicker::colourInput("point_color", "select color for non-highlighted points",
                            value = "lightyellow", closeOnClick = FALSE),
  NULL)

server <- function(input, output, session) {

  filePath <- reactive( input[["file"]]$datapath )

  scr <- reactive({
    ext <- tools::file_ext(req(filePath()))
    meth <- switch(ext,
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
    # factors <- setdiff(names(scr), numerics)
    # include only factors with more than 1 value
    factors <- names(Filter(function(x) length(unique(x)) > 1L, scr[factors]))

    tagList(
      selectInput("varX", "X axis", choices = c("", numerics)),
      selectInput("varY", "Y axis", choices = c("", numerics)),
      selectInput("varCol", "highlight variable", choices = c("", factors)),
      NULL
    )
  })

  # # select values of highlight variable to assigne different color to
  # # then easy to add text as well
  # # this would be one color
  # #     maybe easy to build palette with colourpicker::colourPicker with numCols = length(input[["valCol"]])
  # # requires passing valCol to plotExplore
  # output[["select_values"]] <- renderUI({
  #   values <- req(unique(scr()[[input[["valCol"]]]]))
  #   selectizeInput("valCol", "highlight values", choices = c("", values),
  #                  options = list(
  #                    `placeholder` = sprintf("select %s to highlight", input[["valCol"]])
  #                  ))
  # })

  # # choose color for points
  # color <- reactiveVal("lightyellow")
  # observeEvent(input[["pick_color"]], {
  #   showModal(modalDialog(
  #     colourpicker::colourInput("point_color", "select color for non-highlighted points",
  #                 value = req(color()), closeOnClick = TRUE),
  #     actionButton("accept", "accept"),
  #     footer = NULL
  #   ))
  # })
  # observeEvent(input[["accept"]], {
  #   newColor <- req(input[["point_color"]])
  #   color(newColor)
  #   removeModal()
  # })

  color <- reactive({
    input[["point_color"]]
  })


  ## choose color of highlight
  # this is done by highlight with selectize = TRUE and dynamic = TRUE in plotExplore

  ## color background sections
  ## define sections...

  output[["plot"]] <- plotly::renderPlotly({
    scr <- req(scr())
    varX <- req(input[["varX"]])
    varY <- req(input[["varY"]])
    varCol <- req(input[["varCol"]])
    color <- req(color())

    plotExplore(scr, varX, varY, varCol, color)

  })

}

shinyApp(ui, server)



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


