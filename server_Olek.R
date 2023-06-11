library(ggplot2)
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "EasyNet"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Uploading files", tabName = "upload_files", icon = icon("upload")),
      menuItem("Data preprocessing", tabName = "data_preprocessing", icon = icon("sliders"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "upload_files",
        fluidRow(
          box(
            title = "Upload CSV File",
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            hr(),
            checkboxInput("header", "Header", TRUE),
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                         selected = ","),
            radioButtons("quote", "Quote",
                         choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                         selected = '"')
          )
        ),
        fluidRow(
          box(
            title = "Data File",
            DT::dataTableOutput("content1")
          )
        )
      ),
      tabItem(
        tabName = "data_preprocessing",
        fluidRow(
          box(
            title = "Preprocessing Options",
            checkboxGroupInput(
              inputId = "column_choice",
              label = "Choose columns for preprocessing:",
              selected = NULL,
              choiceNames = c(),
              choiceValues = c(),
              inline = FALSE
            ),
            hr(),
            radioButtons(
              inputId = "transform_choice",
              label = "Transformation method:",
              selected = "None",
              choiceNames = c("Standardization", "Normalization", "None"),
              choiceValues = c("Standardization", "Normalization", "None"),
              inline = TRUE
            ),
            radioButtons(
              inputId = "imputation_method",
              label = "Impute",
              selected = "Mean",
              choiceNames = c("Mean", "Median"),
              choiceValues = c("Mean", "Median"),
              inline = TRUE
            ),
            hr(),
            actionButton("undo_button", "Undo previous operation"),
            actionButton("preprocess", "Do the preprocessing"),
            tags$br(),
            tags$br(),
            textOutput("transformation_history")
          )
        ),
        fluidRow(
          box(
            title = "Processed Data",
            DT::dataTableOutput("content2")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  values <- reactiveValues(
    df = NULL,
    prev_dfs = list(),
    original_df = NULL,
    transformations = character()
  )
  
  observeEvent(input$file1, {
    tryCatch(
      {
        values$original_df <- read.csv(input$file1$datapath,
                                       header = input$header,
                                       sep = input$sep,
                                       quote = input$quote)
        values$df <- values$original_df
        values$prev_dfs <- list()
        values$transformations <- character()
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
  })
  
  output$content1 <- DT::renderDataTable(DT::datatable(values$df, options = list(scrollX = TRUE)))
  output$content2 <- DT::renderDataTable(DT::datatable(values$df, options = list(scrollX = TRUE)))
  
  observe({
    req(input$file1)
    dfnames <- colnames(values$df)
    updateCheckboxGroupInput(
      session,
      "column_choice",
      choiceNames = dfnames,
      choiceValues = dfnames
    )
  })
  
  observeEvent(input$preprocess, {
    if (input$transform_choice == "Standardization") {
      columns <- input$column_choice
      temp <- as.data.frame(values$df[, columns])
      temp <- scale(temp)
      values$prev_dfs <- c(values$prev_dfs, list(values$df))
      values$df[, columns] <- temp
      values$transformations <- c(
        values$transformations,
        paste("Standardization on columns:", columns, collapse = ", ")
      )
    } else if (input$transform_choice == "Normalization") {
      columns <- input$column_choice
      temp <- as.data.frame(values$df[, columns])
      col_min <- apply(temp, 2, min, na.rm = TRUE)
      col_max <- apply(temp, 2, max, na.rm = TRUE)
      temp <- sweep(temp, 2, col_min, FUN = "-")
      temp <- sweep(temp, 2, (col_max - col_min), FUN = "/")
      values$prev_dfs <- c(values$prev_dfs, list(values$df))
      values$df[, columns] <- temp
      values$transformations <- c(
        values$transformations,
        paste("Normalization on columns:", columns, collapse = ", ")
      )
    }
    
    if (input$imputation_method == "Mean") {
      # Perform mean imputation if necessary
    } else if (input$imputation_method == "Median") {
      all_column_median <- apply(values$df, 2, median, na.rm = TRUE)
      for (i in colnames(values$df))
        values$df[, i][is.na(values$df[, i])] <- all_column_median[i]
    }
  })
  
  observeEvent(input$undo_button, {
    if (length(values$transformations) > 0) {
      undo_transformations <- tail(values$transformations, 1)
      values$transformations <- head(values$transformations, -1)
      
      if (grepl("Standardization", undo_transformations)) {
        values$df <- tail(values$prev_dfs, 1)[[1]]
        values$prev_dfs <- head(values$prev_dfs, -1)
      } else if (grepl("Normalization", undo_transformations)) {
        values$df <- tail(values$prev_dfs, 1)[[1]]
        values$prev_dfs <- head(values$prev_dfs, -1)
      }
    }
  })
  
  output$transformation_history <- renderText({
    paste("Recent Transformations:", paste(values$transformations, collapse = "\n"))
  })
}

shinyApp(ui, server)
