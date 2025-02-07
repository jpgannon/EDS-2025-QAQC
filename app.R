library(shiny)
library(plotly)
library(readr)

options(shiny.maxRequestSize = 30 * 1024^2)  # Increase file upload size limit

# Define UI for the application
ui <- fluidPage(
  titlePanel("File Upload and Interactive Graph"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),  # File upload input
      uiOutput("parameter_selector"),  # Dynamic parameter selector
      actionButton("plot_button", "Generate Plot"),  # Button to generate plot
      actionButton("reset_button", "Reset")  # Button to reset the plot
    ),
    
    mainPanel(
      plotlyOutput("interactive_plot"),  # Interactive plot output
      textOutput("feedback")  # Feedback message for the user
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to read the uploaded CSV file
  data <- reactive({
    req(input$file)  # Ensure a file is uploaded
    tryCatch({
      read_csv(input$file$datapath)  # Read the CSV file
    }, error = function(e) {
      showNotification("Invalid file format. Please upload a valid CSV file.", type = "error")  # Show error message
      return(NULL)
    })
  })
  
  # Dynamically generate the parameter selector, waiting for the data to be available
  observeEvent(input$file, {
    
    output$parameter_selector <- renderUI({
      
      req(data())
      
      if (is.null(data()) || ncol(data()) < 2) return(NULL)
      
      cols <- colnames(data())
      
      checkboxGroupInput("parameters", "Select Parameter(s)", choices = colnames(data())[2:10]) # Exclude the first column (assumed to be time)
    
      })

  })
  
  
  # Observe plot button click
  observeEvent(input$plot_button, {
    req(data(), input$parameters)
    
    print(input$parameters) # Check the selection is accurate
    
    df <- data()
    
    current_plot <- plot_ly(data = df, type = "scatter", mode = "lines")
    
    for (param in input$parameters){
      
      current_plot <- current_plot %>% add_trace(x = df[[1]], y = df[[param]], name = param)
      
      }

    
    output$interactive_plot <- renderPlotly({current_plot})
  })
  
  # Reset button functionality
  observeEvent(input$reset_button, {
    output$interactive_plot <- renderPlotly({ NULL })  # Clears the plot
  })
}

# Run the application 
shinyApp(ui = ui, server = server)