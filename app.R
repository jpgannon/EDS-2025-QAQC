library(shiny)
library(ggplot2)
library(plotly)
library(readr)
library(bslib)

options(shiny.maxRequestSize = 30 * 1024^2)  # Increase file upload size limit

# Define UI
ui <- fluidPage(
  #theme = bs_theme(preset = "darkly"),
  
  titlePanel("File Upload and Interactive ggplot"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),  # File upload input
      uiOutput("parameter_selector"),  # Dynamic parameter selector
      actionButton("plot_button", "Generate Plot"),  # Button to generate plot
      actionButton("reset_button", "Reset")  # Reset button
    ),
    
    mainPanel(
      plotlyOutput("interactive_plot"),  # ggplotly interactive plot output
      radioButtons("interaction_mode", "Mode:", 
                   choices = list("Select Points" = "select", 
                                  "Zoom/Pan" = "zoom"), 
                   inline = TRUE),
      
      verbatimTextOutput("brush_info")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Read uploaded CSV file
  data <- reactive({
    req(input$file)
    tryCatch({
      read_csv(input$file$datapath)
    }, error = function(e) {
      showNotification("Invalid file format. Please upload a valid CSV file.", type = "error")
      return(NULL)
    })
  })
  
  # Dynamic parameter selection
  observeEvent(input$file, {
    output$parameter_selector <- renderUI({
      req(data())
      cols <- colnames(data())
      checkboxGroupInput("parameters", "Select Parameter", choices = cols[-1])
    })
  })
  
  # Generate ggplot on button click
  observeEvent(input$plot_button, {
    req(data(), input$parameters)
    df <- data()
    
    # Create a base Plotly scatterplot with multiple traces for selected parameters
    current_plot <- plot_ly(df, x = df[[1]])  # First column as X-axis
    
    for (param in input$parameters) {
      current_plot <- current_plot %>%
        add_trace(y = df[[param]], type = "scatter", mode = "lines", name = param)
    }
    
    output$interactive_plot <- renderPlotly({
      current_plot |> layout(dragmode = input$interaction_mode)  # Dynamic mode switching
    })
  })
  
  # Display brushed points
  output$brush_info <- renderPrint({
    selected <- event_data("plotly_selected")  # Get brushed points
    if (is.null(selected)) {
      "No points selected"
    } else {
      selected
    }
  })
  
  # Reset button
  observeEvent(input$reset_button, {
    output$interactive_plot <- renderPlotly({ NULL })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
