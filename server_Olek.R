library(ggplot2)
library(caret)

function(input, output, session) {
  
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
      values$prev_dfs <- c(values$prev_dfs, list(values$df))  # Store the previous dataframe
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
      values$prev_dfs <- c(values$prev_dfs, list(values$df))  # Store the previous dataframe
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
  
  # Undo the most recent transformation
  observeEvent(input$undo_button, {
    if (length(values$transformations) > 0) {
      undo_transformations <- tail(values$transformations, 1)
      values$transformations <- head(values$transformations, -1)
      
      if (grepl("Standardization", undo_transformations)) {
        values$df <- tail(values$prev_dfs, 1)[[1]]  # Revert the dataframe to the previous state
        values$prev_dfs <- head(values$prev_dfs, -1)  # Remove the most recent previous dataframe
      } else if (grepl("Normalization", undo_transformations)) {
        values$df <- tail(values$prev_dfs, 1)[[1]]  # Revert the dataframe to the previous state
        values$prev_dfs <- head(values$prev_dfs, -1)  # Remove the most recent previous dataframe
      }
    }
  })
  
  output$transformation_history <- renderText({
    paste("Recent Transformations:", paste(values$transformations, collapse = "\n"))
  })
}
