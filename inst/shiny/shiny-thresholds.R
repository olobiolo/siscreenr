
#' app for finding cut-off thresholds for hit scoring


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


  # load cleaned and normalized data
  # allow add zscores: select variable and apply button
  # select variable to select hits by
  # selector for thresholds
  # selector for stringency if replicates
  # RECALCULATE button
  # calculation (eventReactive)
    # hitscore(variable, thresholds), aggregation, stringency
  # barplot with hit numbers vs well type; stacked bars with hit strength; negative hits go below axis
    #
  # table with hit numbers vs well type


}

shinyApp(ui, server)
