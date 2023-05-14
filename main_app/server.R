library(ggplot2)

function(input, output, session) {
  
  df <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  
  output$content1 <- DT::renderDataTable(DT::datatable(({df()}), options = list(scrollX = TRUE)))
  output$content2 <- DT::renderDataTable(DT::datatable(({df()}), options = list(scrollX = TRUE)))
  
  observe({
    req(input$file1)
    dfnames <- colnames(df())
    updateCheckboxGroupInput(
      session,
      "column_choice",
      choiceNames = dfnames,
      choiceValues = dfnames
    )
  })
  
}