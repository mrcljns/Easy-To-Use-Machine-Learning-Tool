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
    temp <- values$df[,input$column_choice]
    values$df <- temp
  })
  
}