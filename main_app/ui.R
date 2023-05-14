library(ggplot2)

navbarPage(
  "EasyNet",
  
  tabPanel("Uploading files",
    
    sidebarLayout(
      
      sidebarPanel(
        
        # Input: Select a file
        fileInput("file1", "Choose CSV File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        
        # Horizontal line
        tags$hr(),
        
        # Input: Checkbox if file has header
        checkboxInput("header", "Header", TRUE),
        
        # Input: Select separator
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","),
        
        # Input: Select quotes
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"')
      ),
      # Main panel for displaying outputs
      mainPanel(
        
        # Output: Data file
        DT::dataTableOutput("content1")
        
      )
      
    )
  ),
  tabPanel("Data preprocessing",
           sidebarLayout(
             
             sidebarPanel(
               checkboxGroupInput(
                 inputId = "column_choice",
                 label = "Choose columns for preprocessing:",
                 selected = NULL,
                 choiceNames = c(),
                 choiceValues = c(),
                 inline = FALSE
               ),
               tags$hr(),
               radioButtons(
                 inputId = "transform_choice",
                 label = "Transformation method:",
                 selected = "None",
                 choiceNames = c("Standardization", "Normalization", "None"),
                 choiceValues = c("Standardization", "Normalization", "None"),
                 inline = TRUE
               ),
               
               # Input: Select imputation method
               radioButtons("imputation_method", "Impute",
                            selected = "Mean",
                            choiceNames = c("Mean", "Median"),
                            choiceValues = c("Mean", "Median"),
                            inline = TRUE),
               
               tags$hr(),
               actionButton("preprocess", "Do the preprocessing")
             ),
             mainPanel(
               DT::dataTableOutput("content2")
             )
           )
           )
)