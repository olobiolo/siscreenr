
#' app for finding cut-off thresholds for hit scoring


library(shiny)

ui <- fluidPage(

  fileInput("file", "upload screen file", accept = ".rds, .txt, .csv, .xls, .xlsx",
            placeholder = "rds, txt, csv or Excel files"),
  uiOutput("select_variable"),    # this is the variable that will be analyzed
                                  # transformation: method
                                  # normalization: method and reference
                                  # zscore: robust, deviation and reference
                                  # thresholds: either numericInput or sliderInput (based on range); small histogram
                                  # stringency: either numericInput or sliderInput (based on number of replicates)
  uiOutput("well_type_variable"), # this variable encodes well types
  uiOutput("well_type_values"),   # these well types will be included in barplot
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

    # convert to data table
    file_content <- data.table::as.Data.table(file_content)

    return(file_content)
  })

  # create reactive value to store current, possibly modified data
  currentData <- reactiveVal()
  # update with new file loaded
  observeEvent(scr(), {
    newData <- req(scr())
    curentData(newData)
  })
  # restore original data (combine with above?)
  observeEvent(input[["restore"]], {
    newData <- req(scr())
    curentData(newData)
  })

  # transform data on request
  observeEvent(input[["transform"]], { # create button (drop-in?)
    data <- currentData()
    vars <- req(input[["select_variable"]])
    meth <- req(input[["transformation_method"]])    # create input (log, log2, log10)

    # prepare names for added columns
    varss <- paste(vars, meth, sep = "_")
    # get the chosen transformation function
    meth <- get(meth)
    # modify data by reference
    data[, eval(varss, .SD) := meth(..vars)]

    currentData(data)
  })

  # normalize data on request
  observeEvent(input[["normalize"]], { # create button (drop-in?)
    data <- req(currentData())
    meth <- req(input[["normalization_method"]])    # create input
    meth <- get(meth)
    ref  <- req(input[["normalization_reference"]]) # create input
    grp  <- req(input[["normalization_grouping"]])  # create_input

    # get/construct variable name
    vars <- req(input[["select_variable"]])
    vars <- paste(vars,
                  input[["transformation_method"]],
                  sep = "_")
    # order column-first just in case
    data.table::setorderv(data, c("plate", "replica", "column", "row"))
    # perform normalization
    normalized <- data[, normalize(.SD, ..vars, ..meth), by = grp_n]
    currentData(normalized)
  })

  # standardize to zscores
  observeEvent(input[["zscores"]], {
    data <- req(currentData())
    rob  <- req(input[["zscore_robust"]])     # create input (logical flag)
    dev  <- req(input[["zscore_deviations"]]) # create input (logical flag)
    ref  <- req(input[["zscore_reference"]])  # create input (logical predicate...)
    grp  <- req(input[["zscore_grouping"]])   # create_input (selectize from non-unique factors)

    vars <- req(input[["select_variable"]])
    vars <- paste(vars,
                  input[["transformation_method"]],
                  "normalized", input[["normalization_method"]],
                  sep = "_")

    # perform standardization
    zscored <-
      data[, zscore(.SD, robust = rob, deviations = dev,
                    reference = ..ref, variables = ..vars),
           by = c('plate_type')]
    currentData(zscored)
  })

  hitThreshold <- renderUI({

  })
  hitStringency <- renderUI({

  })

  scoredData <- eventReactive(input[["flag_hits"]], {
    # option to skip stringency?
    # logical: are there replicates; if FALSE, set stringency to 1
    data <- req(currentData())
    threshold <- req(input[["threshold_hit"]])   # create input
    stringency <- req(input[["hit_stringency"]]) # create input
    grp  <- req(input[["stringency_grouping"]])  # create input (selectize from non-unique factors)

    # getconstruc variable name
    vars <- req(input[["select_variable"]])
    vars <- paste(vars,
                  input[["transformation_method"]],
                  "normalized", input[["normalization_method"]],
                  "zscore",
                  sep = "_")
    # prepare names for hitscore columns
    varss <- paste(vars, "hitscore", sep = "_")
    # prepare names for hit flag columns
    # score hits
    data[, eval(varss) := hitscore(eval(as.name(vars), .SD), threshold = threshold)]
    # apply stringency
    dataFlagged <-
      data[, flag_hits(.SD, vars = ..varss, stringency = ..STRINGENCY),
           by = c("plate", "well")]

    # rearrange rows and columns
    data.table::setorderv(dataFlagged, c("plate", "well", "replica"))
    data.table::setcolorder(dataFlagged, names(scr()))

    return(dataFlagged)
  })

  output[["well_type_variable"]] <- renderUI({
    data <- req(currentData())
    factors <- names(Filter(is.character | is.factor, data))
    selectizeInput("varWellType", "select variable that encodes well types",
                   choices = c("", factors))
  })
  output[["well_type_values"]] <- renderUI({
    data <- req(currentData())
    var <- req(input[["well_type_variable"]])
    vals <- unique(data[[var]])
    selectizeInput("varWellType", "select variable that encodes well types",
                   choices = c("", vals), multiple = TRUE)
  })


  output[["hit_histogram"]] <- plotly::renderPlotly({
    data <- validate(need(scoredData(), "flag hits"))

    data <- data[data$plate_type == "test"]
    vars <- req(input[["hitscore_variable"]])
    grp  <- req(input[["stringency_grouping"]])

    stringency <- req(input[["hit_stringency"]])
    suffix <- if (stringency >= 1) "sum" else "frac"
    varScore <- paste(vars, suffix, sep = "_")

    # function that will build the plot
    plotHits(data, varScore = varScore, varGrp = grp)


  })

  output[["hit_table"]] <- DT::renderDataTable(({
    data <- validate(need(scoredData(), "flag hits"))
  }))


  # load cleaned data DONE
  # select ONE variable TODO
  # allow transformation: choose method do UI
  # allow normalization: choose method and grouping do UI
  # allow add zscores: choose options; apply button do UI
  # selector for thresholds TODO
  # selector for stringency if replicates TODO
  #   need selector of column that stores replicates
  # RECALCULATE button
  # calculation (eventReactive)
    # hitscore(variable, thresholds)
    # flag_hits(variables, stringency)
  # barplot with hit numbers vs hit tiers, colored by well type
    # needs selector for well type variables (and values) DONE
  # table with hit numbers vs hit tiers sparated by well type


}

shinyApp(ui, server)
