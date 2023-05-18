library(ggplot2)
library(caret)
library('corrplot')

library(gridExtra)



server<-function(input, output, session) {
  
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
  
  
  output$plots <- renderPlot({
    req(input$generate_plots, input$variable_choice)
    var <- input$variable_choice
    
    plots <- lapply(var, function(var_name) {
      if (is.numeric(values$df[[var_name]]) || is.integer(values$df[[var_name]])) {
        # Histogram plot for numeric or integer variables
        ggplot(values$df, aes_string(x = var_name)) +
          geom_histogram(fill = "steelblue", color = "black") +
          labs(title = paste("Histogram of", var_name),
               x = var_name, y = "Count")
      } else {
        # Bar plot for factor
        ggplot(values$df, aes_string(x = var_name)) +
          geom_bar(fill = "green", color = "black") +
          labs(title = paste("Bar Plot of", var_name),
               x = var_name, y = "Count")
      }
    })
    
    grid.arrange(grobs = plots, ncol = 1)
  })
  
  
  # Generate heatmap
  output$heatmap_plot <- renderPlot({
    req(input$generate_heatmap, input$variable_choice)
    var <- input$variable_choice
    #elected_df <- values$df[, vars, drop = FALSE]
    corr_matrix <- cor(values$df[var])
    corrplot(corr_matrix, method = "color", tl.col = "black", tl.srt = 0)
  })
  
}

shinyApp(ui = ui, server = server)
