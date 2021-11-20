# THE GREAT APP
# module for building screen
# module for quality control
# module for analysis (transformation, normalization, zscoring, thresholding)
# space for thresholding and hit flagging
#   hitscoring and hitflagging can be done for more than one variable
#   but are done one by one
# module for evaluation (hit numbers per)
#   select one hitflag column
# module for exploration (scatter plot, visualize thresholds, filter data(?))

# required IDs: plate, well, replica, well type, plate type(?), ...
# analyzed variable:
#   choose one
#   compute all variables on the way
#   keep variables in report

# problem 1:
#   data.table awareness requires that all data.frame functions have data.table methods due to df[cols] notation used
#   this concerns zscore and normalize
#   while not awareness makes it difficult to compute rownumber by groups in plotHits




#' app for finding cut-off thresholds for hit scoring


library(shiny)

ui <- fluidPage(

  fileInput("file", "upload screen file", accept = ".rds, .txt, .csv, .xls, .xlsx",
            placeholder = "rds, txt, csv or Excel files"),
  uiOutput("select_variable"),         # this is the variable that will be analyzed
  uiOutput("transformation_controls"), # transformation: method and grouping
  uiOutput("normalization_controls"),  # normalization: method, grouping and reference
  uiOutput("zscore_controls"),         # zscore: robust, deviation and reference and grouping
  uiOutput("hitscore_controls"),       # thresholds: sliderInput (based on range); quartiles and histogram
  uiOutput("stringency_controls"),     # stringency: numericInput
  uiOutput("well_type_variable"),      # this variable encodes well types
  uiOutput("well_type_values"),        # these well types will be included in barplot

  plotly::plotlyOutput("hit_histogram"),
  DT::dataTableOutput("hit_table"),
  # add hitscores to data (for reporting)
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

    # # convert to data table
    # file_content <- data.table::as.data.table(file_content)

    return(file_content)
  })

  # get categorical variables with unique values
  factors <- reactive({
    uniques(req(scr()))
  })

  # create reactive value to store current, possibly modified data
  currentData <- reactiveVal()
  # update with new file loaded
  observeEvent(scr(), {
    newData <- req(scr())
    currentData(newData)
  })
  # restore original data (combine with above?)
  observeEvent(input[["restore"]], {
    newData <- req(scr())
    currentData(newData)
  })

  # choose variable to analyze
  output[["select_variable"]] <- renderUI({
    # factors <- uniques(req(scr()))
    numerics <- names(Filter(is.numeric, req(scr())))
    selectInput("variable", "analyzed variable:", choices = c("", numerics), multiple = FALSE)
  })


  # transformation controls: method, grouping
  output[["transformation_controls"]] <- renderUI({
    factors <- req(factors())
    tagList(
      hr(),
      tags$h5("transformation controls"),
      selectInput("transformation_method", "transformation method:",
                  choices = c("", "log", "log2", "log10")),
      selectizeInput("transformation_grouping", "grouping variables:", choices = c("", factors), multiple = TRUE),
      # button to calculate transformed values
      actionButton("transform", "apply"),
      # checkbox to use them
      checkboxInput("use_transformed", "use transformed values", value = FALSE),
      hr(),
      NULL)
  })

  # transform data on request
  observeEvent(input[["transform"]], {
    data <- currentData()
    vars <- req(input[["variable"]])
    meth <- req(input[["transformation_method"]])
    grp  <- input[["transformation_grouping"]]
    # prepare names for added columns
    varss <- paste(vars, meth, sep = "_")
    # get the chosen transformation function
    meth <- get(meth)
    # modify data by reference
    data.table::setDT(data)
    data[, eval(varss) := meth(eval(as.name(vars))), by = grp]
    data.table::setDF(data)

    currentData(data)
    cat("transformed column:", vars, "\n")
  })

  # normalization controls: method, reference, grouping
  output[["normalization_controls"]] <- renderUI({
    factors <- req(factors())
    tagList(
      hr(),
      tags$h5("normalization controls"),
      selectInput("normalization_method", "normalization method:",
                  choices = c("", "mean", "median", "medpolish")),
      textInput("normalization_reference", "reference:",
                placeholder = "logical predicate, e.g. \"well_type\" == \"nt\""),
      selectizeInput("normalization_grouping", "grouping variables:", choices = c("", factors), multiple = TRUE),
      # button to run normalization
      actionButton("normalize", "apply"),
      checkboxInput("use_normalized", "use normalized values", value = FALSE),
      hr(),
      NULL)
  })

  # normalize data on request
  observeEvent(input[["normalize"]], {
    data <- req(currentData())
    meth <- req(input[["normalization_method"]])
    ref  <- input[["normalization_reference"]]
    grp  <- input[["normalization_grouping"]]

    # get/construct variable name
    vars <- req(input[["variable"]])
    if (input[["use_transformed"]]) {
      vars <- paste(vars, input[["transformation_method"]], sep = "_")
    }
    # order column-first just in case
    data.table::setorderv(data, c("plate", "replica", "column", "row"))

    # perform normalization
    data.table::setDT(data)
    normalized <- data[, normalize(.SD, ..vars, ..meth, ..ref), by = grp]
    data.table::setDF(normalized)

    currentData(normalized)
    cat("normalized column:", vars, "\n")
  })

  # zscore controls: robust, deviations, reference, grouping
  output[["zscore_controls"]] <- renderUI({
    factors <- req(factors())
    tagList(
      hr(),
      tags$h5("standardization controls"),
      checkboxInput("zscore_robust", "robust zscores?", value = FALSE),
      checkboxInput("zscore_deviations", "are values normalized?", value = FALSE),
      textInput("zscore_reference", "reference:",
                placeholder = "enter logical predicate, e.g. \"well_type\" == \"nt\""),
      selectizeInput("zscore_grouping", "grouping variables:", choices = c("", factors), multiple = TRUE),
      # button to calculate zscores
      actionButton("zscores", "apply"),
      # checkbox whether to use them
      checkboxInput("use_zscores", "use zscores", value = FALSE),
      hr(),
      NULL)
  })

  # standardize to zscores
  observeEvent(input[["zscores"]], {
    data <- req(currentData())
    rob  <- input[["zscore_robust"]]
    dev  <- input[["zscore_deviations"]]
    ref  <- input[["zscore_reference"]]
    grp  <- input[["zscore_grouping"]]
    # get/construct variable name
    vars <- req(input[["variable"]])
    if (input[["use_transformed"]]) {
      vars <- paste(vars, input[["transformation_method"]], sep = "_")
    }
    if (input[["use_normalized"]]) {
      vars <- paste(vars, "normalized", input[["normalization_method"]], sep = "_")
    }

    # perform standardization
    data.table::setDT(data)
    zscored <- data[, zscore(.SD, robust = rob, deviations = dev,
                             reference = ref, variables = vars),
                    by = grp]
    data.table::setDF(data)

    currentData(zscored)
    cat("standardized column:", vars, "\n")
  })

  # hitscore threshold controls
  output[["hitscore_controls"]] <- renderUI({
    data <- req(currentData())
    # get/construct variable name
    vars <- req(input[["variable"]])
    if (input[["use_transformed"]]) {
      vars <- paste(vars, input[["transformation_method"]], sep = "_")
    }
    if (input[["use_normalized"]]) {
      vars <- paste(vars, "normalized", input[["normalization_method"]], sep = "_")
    }
    if (input[["use_zscores"]]) {
      vars <- paste(vars, "zscore", sep = "_")
    }

    # calculate extent of data and initial suggestions (1% hit rate on each side)
    varRange <- range(data[[vars]], na.rm = TRUE)
    varQuantiles <- stats::quantile(data[[vars]], probs = c(0.1, 0.99), na.rm = TRUE)

    # TODO
    # add small histogram that will also show the current thresholsd (color parts?)
    # add info from quartile method

    # present threshold selections; also groping for hitscore aggregation
    tagList(
      hr(),
      tags$h5("hit scoring controls"),
      sliderInput("hit_threshold", "hit thresholds:",
                  min = varRange[1], max = varRange[2], value = varQuantiles,
                  dragRange = FALSE),
      hr(),
      NULL)
  })

  # stringency threshold controls
  output[["stringency_controls"]] <- renderUI({
    factors <- req(factors())
    tagList(
      hr(),
      tags$h5("stringency controls"),
      tags$h5("variables tha uniquely identify wells, e.g. plate and well, or gene symbol"),
      selectizeInput("stringency_grouping", "grouping variables:", choices = c("", factors), multiple = TRUE),
      numericInput("hit_stringency", "hit scoring stringency:",
                   # value = 1, min = 0, max = length(input[["stringency_grouping"]]), step = 0.01),
                   value = 1, min = 0, max = 5, step = 0.01),
      actionButton("flag_hits", "flag hits"),
      hr(),
      NULL)
  })

  scoredData <- eventReactive(input[["flag_hits"]], {

    # option to skip stringency?
    # logical: are there replicates; if FALSE, set stringency to 1
    data <- req(currentData())
    hitThreshold <- req(input[["hit_threshold"]])
    hitStringency <- req(input[["hit_stringency"]])
    grp  <- input[["stringency_grouping"]]

    # get/construct variable name
    vars <- req(input[["variable"]])
    if (input[["use_transformed"]]) {
      vars <- paste(vars, input[["transformation_method"]], sep = "_")
    }
    if (input[["use_normalized"]]) {
      vars <- paste(vars, "normalized", input[["normalization_method"]], sep = "_")
    }
    if (input[["use_zscores"]]) {
      vars <- paste(vars, "zscore", sep = "_")
    }

    # prepare name for hitscore column
    varss <- paste(vars, "hitscore", sep = "_")

    # score hits and apply stringency
    data.table::setDT(data)
    data[, eval(varss) := hitscore(eval(as.name(vars), .SD), threshold = hitThreshold)]
    dataFlagged <- data[, flag_hits(.SD, vars = ..varss, stringency = hitStringency), by = grp]
    data.table::setDF(dataFlagged)

    # rearrange rows and columns
    data.table::setorderv(dataFlagged, c("plate", "well", "replica")) # TODO this is sensitive to names
    data.table::setcolorder(dataFlagged, names(scr()))

    cat("scored column:", vars, "\n")
    return(dataFlagged)
  })



  # select variable that stores well type
  output[["well_type_variable"]] <- renderUI({
    factors <- req(factors())
    selectizeInput("varWellType", "select variable that encodes well types", choices = c("", factors))
  })
  # select well type values for plot
  output[["well_type_values"]] <- renderUI({
    data <- req(currentData())
    var <- req(input[["varWellType"]])
    vals <- unique(data[[var]])
    selectizeInput("valWellType", "select well types to plot",
                   choices = c("", vals), multiple = TRUE)
  })

  # create histogram
  output[["hit_histogram"]] <- plotly::renderPlotly({
    data <- req(scoredData())
    data <- data[data$plate_type == "test", ] # TODO sensitive

    # get/construct variable name
    vars <- req(input[["variable"]])
    if (input[["use_transformed"]]) {
      vars <- paste(vars, input[["transformation_method"]], sep = "_")
    }
    if (input[["use_normalized"]]) {
      vars <- paste(vars, "normalized", input[["normalization_method"]], sep = "_")
    }
    if (input[["use_zscores"]]) {
      vars <- paste(vars, "zscore", sep = "_")
    }
    vars <- paste(vars, "hitscore", sep = "_")

    # determine aggregate suffix based on stringency threshold
    grp  <- req(input[["stringency_grouping"]])
    stringency <- req(input[["hit_stringency"]])
    suffix <- if (stringency >= 1) "sum" else "frac"
    varScore <- paste(vars, suffix, sep = "_")

    # coloring is done by well type
    # sensitive but we already ask for well type ID
    varCol <- req(input[["varWellType"]])
    valCol <- req(input[["valWellType"]])

    # subset data
    data <- data[is.element(data[[varCol]], valCol), ]

    # function that will build the plot
    plotHits(data, varScore = varScore, varCol = varCol, varGrp = grp)
  })

  output[["hit_table"]] <- DT::renderDataTable({
    # data <- validate(need(scoredData(), "flag hits"))
  })


  # load cleaned data DONE
  # select ONE variable DONE
  # allow transformation: DONE
  # allow normalization: DONE
  # allow add zscores: DONE
  # selector for thresholds DONE
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
