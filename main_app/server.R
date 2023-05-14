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
    if(input$transform_choice == "Standardization"){
      temp = as.data.frame(values$df[,input$column_choice])
      preproc <- preProcess(temp, method=c("center", "scale"))
      temp <- predict(preproc, temp)
      values$df[,input$column_choice] <- temp
    }
    else if(input$transform_choice == "Normalization"){
      temp = as.data.frame(values$df[,input$column_choice])
      preproc <- preProcess(temp, method=c("range"))
      temp <- predict(preproc, temp)
      values$df[,input$column_choice] <- temp
    }
  })
  
}