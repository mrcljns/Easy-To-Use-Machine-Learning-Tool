if (!require('pacman')) install.packages('pacman')
pacman::p_load(devtools, ggplot2, shiny, shinydashboard, caret, corrplot)

if (!require(NeuralWNEt)){
  install_github("mrcljns/Easy-To-Use-Machine-Learning-Tool/NeuralWNEt", force = TRUE)
}

library(NeuralWNEt)

options(scipen=999)
  
  ui <- dashboardPage(
    dashboardHeader(title = "Easy to use ML Tool"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Uploading files", tabName = "upload_files", icon = icon("upload")),
        menuItem("Modifying data types", tabName = "modif_dtypes", icon = icon("cog")),
        menuItem("Data visualization", tabName = "data_visualization", icon = icon("bar-chart")),
        menuItem("Data preprocessing", tabName = "data_preprocessing", icon = icon("sliders")),
        menuItem("Neural network", tabName = "neural_net", icon = icon("flag", lib="glyphicon"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "upload_files",
          fluidRow(
            box(
              width = 6,
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
              width = 12,
              title = "Data File",
              DT::dataTableOutput("content1")
            )
          )
        ),
        tabItem(
          tabName = "modif_dtypes",
          fluidRow(
            box(
              title = "Data Summary",
              DT::dataTableOutput("summary_table")
            ),
            box(
              title = "Change Variable Type",
              selectInput("selected_variable", "Select Variable:", choices = NULL),
              selectInput("selected_type", "Select Type:", choices = c("logical", "integer", "numeric", "character")),
              actionButton("change_type_btn", "Change Type")
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
            ),
            box(
              width = 6,
              title = "Variable Summary",
              DT::dataTableOutput("variable_summary_table")
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
              width = 4,
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
                selected = NULL,
                choiceNames = c("Mean", "Median", "Mode"),
                choiceValues = c("Mean", "Median", "Mode"),
                inline = TRUE
              ),
              hr(),
              actionButton("imputation_button", "Apply Imputation"),
            ),
            box(
              width = 4,
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
            ),
            box(
              width = 4,
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
              actionButton("normalization_button", "Apply Normalization")
            )
          ),
          fluidRow(
            box(
              width = 12,
              actionButton("undo_button", "Undo last transformation"),
              tags$br(),
              tags$strong("Note:"),
              tags$span(
                "Imputation is an irreversible action. Please ensure you have a backup of your data.", style = "color: red; margin-top: 10px;"
              ),
              tags$br(),
              tags$br(),
              textOutput("transformation_history")
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "Processed Data",
              DT::dataTableOutput("content2")
            )
          )
        ),
        tabItem(
          tabName = "neural_net",
          fluidRow(
            box(
              width = 12,
              title = "Choose features and target",
              column(
                width = 6,
                selectInput(
                  inputId = "target_variable",
                  label = "Select Target Variable:",
                  selected = NULL,
                  choices = c()
                )
              ),
              column(
                width = 6,
                checkboxGroupInput(
                  inputId = "feature_variables",
                  label = "Select Feature Variables:",
                  selected = NULL,
                  choiceNames = c(),
                  choiceValues = c(),
                  inline = FALSE
                )
              )
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "Customize the network",
              column(
                width = 4,
                numericInput(
                  inputId = "random_state_choice",
                  label = "Enter the random state seed:",
                  value = 42,
                  min = 1,
                  max = 100,
                  step = 1,
                  width = "300px"
                )
              ),
              column(
                width = 4,
                numericInput(
                  inputId = "hidden_layer_num_choice",
                  label = "Enter the number of hidden layers:",
                  value = 2,
                  min = 1,
                  max = 3,
                  step = 1,
                  width = "300px"
                )),
              column(
                width = 4,
                numericInput(
                  inputId = "hidden_neuron_num_choice",
                  label = "Enter the number of neurons in hidden layer:",
                  value = 4,
                  min = 1,
                  max = 20,
                  step = 1,
                  width = "300px"
                )
              ),
              hr(),
              radioButtons(
                inputId = "problem_type_choice",
                label = "Problem type:",
                selected = "regression",
                choiceNames = c("Regression", "Classification"),
                choiceValues = c("regression", "classification"),
                inline = TRUE
              ),
              radioButtons(
                inputId = "activation_function_choice",
                label = "Activation function:",
                selected = "sigmoid",
                choiceNames = c("Sigmoid", "Tanh", "ReLu"),
                choiceValues = c("sigmoid", "tanh", "relu"),
                inline = TRUE
              ),
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "Training options",
              column(
                width = 4,
                numericInput(
                  inputId = "epochs_num_choice",
                  label = "Enter the number of epochs:",
                  value = 1000,
                  min = 100,
                  max = 10000,
                  step = 1,
                  width = "300px"
                )),
              column(
                width = 4,
                numericInput(
                  inputId = "learning_rate_choice",
                  label = "Enter the learning rate:",
                  value = 0.01,
                  min = 1e-5,
                  max = 1e-1,
                  step = 1e-4,
                  width = "300px"
                )),
              column(
                width = 4,
                numericInput(
                  inputId = "batch_size_choice",
                  label = "Enter the batch size",
                  value = 64,
                  min = 32,
                  max = 128,
                  step = 1,
                  width = "300px"
                )
              ),
              hr(),
              actionButton("training_button", "Train the network", icon = icon("cog", lib="glyphicon")),
            )
          ),
          fluidRow(
            box(
              width = 12,
              verbatimTextOutput("results")
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
      imputation_info = "",
      split_df = NULL
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
          
          updateSelectInput(session, "selected_variable", choices = colnames(values$df))
        },
        error = function(e) {
          stop(safeError(e))
        }
      )
    })
    
    output$content1 <- DT::renderDataTable(DT::datatable(values$df, options = list(scrollX = TRUE)))
    output$content2 <- DT::renderDataTable(DT::datatable(values$df, options = list(scrollX = TRUE)))
    
    data_summary <- reactive({
      column_names <- colnames(values$df)
      column_types <- sapply(values$df, class)
      
      summary_data <- data.frame(Type = column_types, stringsAsFactors = FALSE)
      summary_data
    })
    
    output$summary_table <- DT::renderDataTable({
      DT::datatable(data_summary())
    })
    
    observeEvent(input$change_type_btn, {
      req(input$selected_variable, input$selected_type)
      selected_var <- input$selected_variable
      new_type <- input$selected_type
      
      if (selected_var %in% colnames(values$df)) {
        if (new_type == "integer" && is.character(values$df[[selected_var]])) {
          unique_values <- unique(values$df[[selected_var]])
          converted_var <- match(values$df[[selected_var]], unique_values) - 1
          converted_var <- as.integer(converted_var)
        } else {
          converted_var <- try(as(values$df[[selected_var]], new_type), silent = TRUE)
        }
        
        if (inherits(converted_var, "try-error")) {
          showModal(
            modalDialog(
              title = "Warning",
              paste("Failed to convert", selected_var, "to", new_type),
              easyClose = TRUE
            )
          )
        } else {
          values$df[[selected_var]] <- converted_var
          updateSelectInput(session, "selected_variable", choices = colnames(values$df))
        }
      }
    })
    
    
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
            ggplot(values$df, aes_string(x = local_var_name)) +
              geom_histogram(fill = "steelblue", color = "black") +
              labs(title = paste("Histogram of", local_var_name),
                   x = local_var_name, y = "Count")
          } else {
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
    
    observeEvent(input$generate_heatmap, {
      req(input$variable_choice)
      var <- input$variable_choice
      
      selected_df <- values$df[, var, drop = FALSE]
      numeric_vars <- sapply(selected_df, is.numeric)
      selected_df <- selected_df[, numeric_vars, drop = FALSE]
      
      output$heatmap_plot <- renderPlot({
        if (ncol(selected_df) > 1) {
          vars_with_missing <- colSums(is.na(selected_df)) > 0
          
          if (any(vars_with_missing)) {
            missing_vars <- names(selected_df)[vars_with_missing]
            plot.new()
            text(0.5, 0.5, paste("Variables with missing values:\n", missing_vars, "\nPlease deal with NAs in the 'Data Preprocessing' page."), cex = 1.2, col = "red", font = 2)
          } else {
            corr_matrix <- cor(selected_df)
            corrplot(corr_matrix, method = "color", tl.col = "black", tl.srt = 0)
          }
        } else {
          plot.new()
          text(0.5, 0.5, "Please select more than one numerical variable for the heatmap.", cex = 1.2, col = "red", font = 2)
        }
      })
    })
    
    variable_summary <- reactive({
      summary_data <- data.frame(
        Variable = colnames(values$df),
        Min = sapply(values$df, function(x) if (is.numeric(x)) min(x, na.rm = TRUE) else NA),
        Max = sapply(values$df, function(x) if (is.numeric(x)) max(x, na.rm = TRUE) else NA),
        Mean_Mode = sapply(values$df, function(x) if (is.numeric(x)) mean(x, na.rm = TRUE) else names(which.max(table(x)))),
        Standard_Deviation = sapply(values$df, function(x) if (is.numeric(x)) sd(x, na.rm = TRUE) else NA),
        Missing_Values = colSums(is.na(values$df))
      )
      summary_data <- summary_data[, c(2, 3, 4, 5, 6)]
      summary_data[!sapply(values$df, is.numeric), c("Min", "Max")] <- NA
      summary_data
    })
    
    output$variable_summary_table <- DT::renderDataTable({
      DT::datatable(variable_summary(), options = list(scrollX = TRUE))
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
      updateSelectInput(
        session,
        "target_variable",
        choices = dfnames,
        selected = dfnames[1]
      )
      updateCheckboxGroupInput(
        session,
        "feature_variables",
        choices = dfnames
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
      } else if (input$imputation_method == "Mode") {
        for (col in columns) {
          col_mode <- names(sort(table(values$df[, col]), decreasing = TRUE))[1]
          values$df[is.na(values$df[, col]), col] <- col_mode
        }
      }
    })
    
    observeEvent(input$standardization_button, {
      columns <- input$transformation_columns
      temp <- as.data.frame(values$df[, columns])
      temp <- as.data.frame(scale(temp))
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
      
      if (length(columns) != 1) {
        showModal(modalDialog(
          title = "Invalid Selection",
          "Please select exactly one column for one-hot encoding.",
          easyClose = TRUE
        ))
      } else {
        if (any(!sapply(values$df[, columns], is.character))) {
          showModal(modalDialog(
            title = "Invalid Selection",
            "One-hot encoding can only be applied to categorical columns.",
            easyClose = TRUE
          ))
        } else if (any(sapply(values$df[, columns], function(x) any(is.na(x))))) {
          showModal(modalDialog(
            title = "Missing Values",
            "One-hot encoding cannot be applied to columns with missing values.",
            easyClose = TRUE
          ))
        } else {
          encoded_cols <- model.matrix(~ 0 + as.factor(values$df[, columns]))
          colnames(encoded_cols) <- paste0(columns, "_", sub("as\\.factor\\(values\\$df\\[, columns\\]\\)", "", colnames(encoded_cols)))
          values$prev_dfs <- c(values$prev_dfs, list(values$df))
          values$df <- cbind(values$df, encoded_cols)
          values$df <- values$df[, !names(values$df) %in% columns, drop = FALSE]
          values$transformations <- c(
            values$transformations,
            paste("One-Hot Encoding on columns:", paste(columns, collapse = ", "))
          )
        }
      }
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
    
    observeEvent(input$training_button, {
      
      if ((!is.null(input$target_variable) || !is.null(input$feature_variables)) && 
          length(input$feature_variables) > 0) {
        values$target_var <- input$target_variable
        values$feature_vars <- input$feature_variables
        if (any(is.na(values$df[, c(values$target_var, values$feature_vars)]))) {
          showModal(
            modalDialog(
              title = "Missing Values",
              "Please fill the empty values in your database before proceeding.",
              easyClose = TRUE
            )
          )
        }
        else if (any(sapply(values$df[, c(values$target_var, values$feature_vars)], 
                            function(x) class(x) == "character"))){
          showModal(
            modalDialog(
              title = "Wrong data type",
              "Character variables are not supported as feature/target.",
              easyClose = TRUE
            )
          )
        }
        else if (any(sapply(values$df[, c(values$target_var, values$feature_vars)], 
                            function(x) class(x) == "factor"))){
          showModal(
            modalDialog(
              title = "Wrong data type",
              "Factor variables are not supported as feature/target.",
              easyClose = TRUE
            )
          )
        }
        else if (any(sapply(values$df[, c(values$target_var, values$feature_vars)], 
                            function(x) class(x) == "Date"))){
          showModal(
            modalDialog(
              title = "Wrong data type",
              "Date variables are not supported as feature/target.",
              easyClose = TRUE
            )
          )
        }
        else {
          modeling_df <- isolate(values$df)[c(values$target_var, values$feature_vars)]
          data.index <- createDataPartition(modeling_df[,values$target_var], p = 0.8, list = FALSE)
          train_data <- modeling_df[data.index, ]
          test_data <- modeling_df[-data.index, ]
          
          if (input$problem_type_choice == "classification"){
            set.seed(42)
            train_data <- downSample(x = train_data[values$feature_vars],
                                     y = as.factor(train_data[, values$target_var]),
                                     yname = values$target_var)
            
            train_data[values$target_var] <- as.integer(train_data[, values$target_var]) - 1
            
            train_data <- train_data[sample(1:nrow(train_data)),]
          }
          
          values$X_train <- train_data[values$feature_vars]
          values$y_train <- train_data[,values$target_var]
          values$X_test <- test_data[values$feature_vars]
          values$y_test <- test_data[,values$target_var]
          
          if (input$problem_type_choice == "regression" && !is.numeric(values$y_train)){
            showModal(
              modalDialog(
                title = "Wrong target type",
                "For regression, target must be numeric.",
                easyClose = TRUE
              )
            )
          }
          else if (input$problem_type_choice == "classification" && 
                   (!(is.numeric(values$y_train) || is.logical(values$y_train)) ||
                    length(unique(values$y_train)) != 2)){
            showModal(
              modalDialog(
                title = "Wrong target type",
                "For classification, target must be taking one of two values.",
                easyClose = TRUE
              )
            )
          }
          else{
            
            net <- NeuralNetwork$new(input$random_state_choice, input$hidden_layer_num_choice,
                                     input$hidden_neuron_num_choice, input$problem_type_choice,
                                     input$activation_function_choice)
            
            net$init_network(ncol(values$X_train))
            
            net$train(values$X_train, values$y_train, input$epochs_num_choice, input$learning_rate_choice,
                      input$batch_size_choice)

            preds <- net$predict(values$X_test)

            if (input$problem_type_choice == "regression"){
              if (!is.null(preds) || !is.infinite(preds)){
                output$results <- renderPrint({
                  print(paste("MSE:", mean((preds-values$y_test)**2)))
                  print(paste("RMSE: ", sqrt(mean((preds-values$y_test)**2))))
                  print(paste("MAPE: ", mean(abs((preds-values$y_test)/preds), na.rm=TRUE)))
                })
              }
              else {
                output$results <- renderPrint({
                  print(paste("MSE: ", NaN))
                  print(paste("RMSE: ", NaN))
                  print(paste("MAPE: ", NaN))
                })
              }
            }
            else {
              output$results <- renderPrint({ confusionMatrix(data = as.factor(as.integer(preds)),
                                                              reference = as.factor(as.integer(values$y_test))) })
            }
          }
        }
      }
      else {
        showModal(
          modalDialog(
            title = "Missing target or features",
            "Please choose the features/target before proceeding.",
            easyClose = TRUE
          )
        )
      }
    })
    
    
  }
  
  shinyApp(ui, server)
