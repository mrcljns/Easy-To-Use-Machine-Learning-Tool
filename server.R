library(ggplot2)
library(shiny)
library(shinydashboard)
library('corrplot')

ui <- dashboardPage(
  dashboardHeader(title = "Easy to use ML Tool"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Uploading files", tabName = "upload_files", icon = icon("upload")),
      menuItem("Data visualization", tabName = "data_visualization", icon = icon("bar-chart")),
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
          ),
          box(
            title = "Data File",
            DT::dataTableOutput("content1")
          )
        )
      ),
      tabItem(
        tabName = "data_visualization",
        fluidRow(
          box(
            title = "Visualize variables",
            checkboxGroupInput(
              inputId = "variable_choice",
              label = "Choose columns for visualization:",
              selected = NULL,
              choiceNames = c(),
              choiceValues = c(),
              inline = FALSE
            ),
            hr(),
            actionButton("generate_plots", "Generate Plots", icon = icon("bar-chart")),
            actionButton("generate_heatmap", "Generate Heatmap")
          )
        ),
        fluidRow(
          box(
            title = "Plots",
            uiOutput("plot_outputs")
          ),
          box(
            title = "Heatmap",
            plotOutput("heatmap_plot")
          )
        )
      ),
      tabItem(
        tabName = "data_preprocessing",
        fluidRow(
          box(
            title = "Imputation Options",
            checkboxGroupInput(
              inputId = "imputation_columns",
              label = "Choose columns for imputation:",
              selected = NULL,
              choiceNames = c(),
              choiceValues = c(),
              inline = FALSE
            ),
            hr(),
            radioButtons(
              inputId = "imputation_method",
              label = "Imputation method:",
              selected = "Mean",
              choiceNames = c("Mean", "Median"),
              choiceValues = c("Mean", "Median"),
              inline = TRUE
            ),
            hr(),
            actionButton("imputation_button", "Apply Imputation"),
            tags$br(),
            tags$strong("Note:"),
            tags$span(
              "Imputation is an irreversible action. Please ensure you have a backup of your data.", style = "color: red; margin-top: 10px;"
            ),
          ),
          box(
            title = "Processed Data",
            DT::dataTableOutput("content2")
          )
        ),
        fluidRow(
          box(
            title = "One-Hot encoding",
            checkboxGroupInput(
              inputId = "onehot_columns",
              label = "Choose columns for encoding:",
              selected = NULL,
              choiceNames = c(),
              choiceValues = c(),
              inline = FALSE
            ),
            hr(),
            actionButton("onehot_button", "Apply One-Hot Encoding"),
          )
        ),
        fluidRow(
          box(
            title = "Transformation Options",
            checkboxGroupInput(
              inputId = "transformation_columns",
              label = "Choose columns for transformation:",
              selected = NULL,
              choiceNames = c(),
              choiceValues = c(),
              inline = FALSE
            ),
            hr(),
            actionButton("standardization_button", "Apply Standardization"),
            actionButton("normalization_button", "Apply Normalization"),
            hr(),
            actionButton("undo_button", "Undo last transformation"),
            tags$br(),
            tags$br(),
            textOutput("transformation_history")
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
    transformations = character(),
    imputation_info = ""
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
    req(values$df)
    var_names <- names(values$df)
    updateCheckboxGroupInput(
      session,
      "variable_choice",
      choices = var_names
    )
  })
  
  observeEvent(input$generate_plots, {
    req(input$variable_choice)
    var <- input$variable_choice
    
    plot_outputs <- lapply(var, function(var_name) {
      local_var_name <- var_name
      
      renderPlot({
        if (is.numeric(values$df[[local_var_name]]) || is.integer(values$df[[local_var_name]])) {
          # Histogram plot for numeric or integer variables
          ggplot(values$df, aes_string(x = local_var_name)) +
            geom_histogram(fill = "steelblue", color = "black") +
            labs(title = paste("Histogram of", local_var_name),
                 x = local_var_name, y = "Count")
        } else {
          # Bar plot for factor
          ggplot(values$df, aes_string(x = local_var_name)) +
            geom_bar(fill = "green", color = "black") +
            labs(title = paste("Bar Plot of", local_var_name),
                 x = local_var_name, y = "Count")
        }
      })
    })
    
    output$plot_outputs <- renderUI({
      do.call(tagList, plot_outputs)
    })
  })
  
  # Generate heatmap
  output$heatmap_plot <- renderPlot({
    req(input$generate_heatmap, input$variable_choice)
    var <- input$variable_choice
    selected_df <- values$df[, var, drop = FALSE]
    corr_matrix <- cor(selected_df)
    corrplot(corr_matrix, method = "color", tl.col = "black", tl.srt = 0)
  })
  
  observe({
    req(input$file1)
    dfnames <- colnames(values$df)
    updateCheckboxGroupInput(
      session,
      "imputation_columns",
      choiceNames = dfnames,
      choiceValues = dfnames
    )
    updateCheckboxGroupInput(
      session,
      "onehot_columns",
      choiceNames = dfnames,
      choiceValues = dfnames
    )
    updateCheckboxGroupInput(
      session,
      "transformation_columns",
      choiceNames = dfnames,
      choiceValues = dfnames
    )
  })
  
  observeEvent(input$imputation_button, {
    columns <- input$imputation_columns
    if (input$imputation_method == "Mean") {
      for (col in columns) {
        col_mean <- mean(values$df[, col], na.rm = TRUE)
        values$df[is.na(values$df[, col]), col] <- col_mean
      }
    } else if (input$imputation_method == "Median") {
      for (col in columns) {
        col_median <- median(values$df[, col], na.rm = TRUE)
        values$df[is.na(values$df[, col]), col] <- col_median
      }
    }
  })
  
  observeEvent(input$standardization_button, {
    columns <- input$transformation_columns
    temp <- as.data.frame(values$df[, columns])
    temp <- scale(temp)
    values$prev_dfs <- c(values$prev_dfs, list(values$df))
    values$df[, columns] <- temp
    values$transformations <- c(
      values$transformations,
      paste("Standardization on columns:", paste(columns, collapse = ", "))
    )
  })
  
  observeEvent(input$normalization_button, {
    columns <- input$transformation_columns
    temp <- as.data.frame(values$df[, columns])
    col_min <- apply(temp, 2, min, na.rm = TRUE)
    col_max <- apply(temp, 2, max, na.rm = TRUE)
    temp <- sweep(temp, 2, col_min, FUN = "-")
    temp <- sweep(temp, 2, (col_max - col_min), FUN = "/")
    values$prev_dfs <- c(values$prev_dfs, list(values$df))
    values$df[, columns] <- temp
    values$transformations <- c(
      values$transformations,
      paste("Normalization on columns:", paste(columns, collapse = ", "))
    )
  })
  
  observeEvent(input$onehot_button, {
    columns <- input$onehot_columns
    encoded_cols <- model.matrix(~ 0 + as.factor(values$df[, columns]))
    colnames(encoded_cols) <- paste0(columns, "_", colnames(encoded_cols))
    values$prev_dfs <- c(values$prev_dfs, list(values$df))
    values$df <- cbind(values$df, encoded_cols)
    values$df <- values$df[, !names(values$df) %in% columns, drop = FALSE]
    values$transformations <- c(
      values$transformations,
      paste("One-Hot Encoding on columns:", paste(columns, collapse = ", ")
      )
    )
  })
  
  observeEvent(input$undo_button, {
    if (length(values$prev_dfs) > 0) {
      values$df <- tail(values$prev_dfs, n = 1)[[1]]
      values$prev_dfs <- head(values$prev_dfs, n = length(values$prev_dfs) - 1)
      values$transformations <- head(values$transformations, n = length(values$transformations) - 1)
    }
  })
  
  output$transformation_history <- renderText({
    paste(values$transformations, collapse = "\n")
  })
}

shinyApp(ui, server)
