
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
    file_content <- meth(filePath())

    ## add some validation actions

    return(file_content)
  })


  output[["select_variables"]] <- renderUI({

    scr <- req(scr())
    numerics <- names(Filter(is.numeric, scr))
    # factors <- setdiff(names(scr), numerics)
    # include only factors with more than 1 value
    factors <- uniques(scr)

    tagList(
      selectInput("varX", "X axis", choices = c("", numerics)),
      selectInput("varY", "Y axis", choices = c("", numerics)),
      selectInput("varCol", "highlight variable", choices = c("", factors)),
      NULL
    )
  })

  # # select values of highlight variable to assign different color to
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
