library(ggplot2)
library(caret)

function(input, output, session) {
  
  values <- reactiveValues(df = NULL)
  
  observeEvent(input$file1, {
    tryCatch(
      {
        values$df <- read.csv(input$file1$datapath,
                              header = input$header,
                              sep = input$sep,
                              quote = input$quote)
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
  })
  
  output$content1 <- DT::renderDataTable(DT::datatable(({values$df}), options = list(scrollX = TRUE)))
  output$content2 <- DT::renderDataTable(DT::datatable(({values$df}), options = list(scrollX = TRUE)))
  
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
      temp <- as.data.frame(values$df[, input$column_choice])
      preproc <- preProcess(temp, method = c("center", "scale"))
      temp <- predict(preproc, temp)
      values$df[, input$column_choice] <- temp
    } else if (input$transform_choice == "Normalization") {
      temp <- as.data.frame(values$df[, input$column_choice])
      preproc <- preProcess(temp, method = c("range"))
      temp <- predict(preproc, temp)
      values$df[, input$column_choice] <- temp
    }
    
    if (input$imputation_method == "Mean") {
      # Perform mean imputation logic here
    } else if (input$imputation_method == "Median") {
      all_column_median <- apply(values$df, 2, median, na.rm = TRUE)
      for (i in colnames(values$df))
        values$df[, i][is.na(values$df[, i])] <- all_column_median[i]
    }
  })
  
  observeEvent(input$clear_nas, {
    values$df <- na.omit(values$df)
  })
  
  output$na_count <- renderText({
    na_count <- sum(is.na(values$df))
    na_count
  })
  
  # Update variable choices for Data Visualization tab
  observe({
    req(values$df)
    var_names <- names(values$df)
    updateCheckboxGroupInput(
      session,
      "variable_choice",
      choices = var_names
    )
  })
  
  # Generate histogram plots
  output$histogram_plots <- renderPlot({
    req(input$generate_plots, input$variable_choice)
    var <- input$variable_choice
    if (is.numeric(values$df[[var]]) || is.integer(values$df[[var]])) {
      ggplot(values$df, aes_string(x = var)) +
        geom_histogram(fill = "steelblue", color = "black") +
        labs(title = paste("Histogram of", var),
             x = var, y = "Count")
    }
  })
  
  # Generate bar plots
  output$barplot_plots <- renderPlot({
    req(input$generate_plots, input$variable_choice)
    var <- input$variable_choice
    if (is.factor(values$df[[var]]) || is.logical(values$df[[var]])) {
      ggplot(values$df, aes_string(x = var)) +
        geom_bar(fill = "red", color = "black") +
        labs(title = paste("Bar Plot of", var),
             x = var, y = "Count")
    }
  })
  
  # Generate heatmap
  output$heatmap_plot <- renderPlot({
    req(input$generate_heatmap, input$variable_choice)
    vars <- input$variable_choice
    selected_df <- values$df[, vars, drop = FALSE]
    correlation_matrix <- cor(selected_df, use = "complete.obs")
    ggplot(data = melt(correlation_matrix), aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(title = "Correlation Heatmap",
           x = "Variable 1", y = "Variable 2")
  })
  
}

