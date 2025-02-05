# Load required libraries
library(shiny)
library(plotly)
library(readr)

options(shiny.maxRequestSize = 30*1024^2)

# Define UI for the application
ui <- fluidPage(
  
  titlePanel("File Upload and Interactive Graph"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload File", accept = c(".csv")),
      uiOutput("parameter_selector"),
      actionButton("plot_button", "Generate Plot")
    ),
    
    mainPanel(
      plotlyOutput("interactive_plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to read the uploaded CSV file
  data <- reactive({
    req(input$file)
    read_csv(input$file$datapath)
  })
  
  # Dynamically generate the parameter selector based on the uploaded CSV
  output$parameter_selector <- renderUI({
    req(data())
    
    colnames(data()) -> names
    
    checkboxGroupInput("parameter", "Select Parameter(s)", choices = names[])
  })
  
  # Generate the interactive plot when the button is clicked
  observeEvent(input$plot_button, {
    output$interactive_plot <- renderPlotly({
      req(data(), input$parameter)
      
      # Initialize the plot
      p <- plot_ly()
      
      # Loop through selected parameters and add them to the plot
      for (param in input$parameter) {
        p <- p |>  add_trace(
          x = ~data()[[1]],  # Use row numbers as time steps (replace with a time column if available)
          y = ~data()[[param]],
          type = "scatter",
          mode = "lines",
          name = param  # Add a legend for each parameter
        )
      }
      
      # Customize the plot layout
      p <- p |>  layout(
        xaxis = list(title = "Time"),
        yaxis = list(title = "Value"),
        showlegend = TRUE  # Show legend to distinguish between parameters
      )
      
      #p
    })
  })
  p
}

# Run the application 
shinyApp(ui = ui, server = server)