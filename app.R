library(shiny)
library(ggplot2)
library(plotly)
library(readr)
library(bslib)

options(shiny.maxRequestSize = 30 * 1024^2)  # Increase file upload size limit

# Define UI
ui <- fluidPage(
  titlePanel("File Upload and Interactive ggplot"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),  # File upload input
      uiOutput("parameter_selector"),
      uiOutput("airTemp_selector"),
      uiOutput("precip_selector"), # Dynamic parameter selector
      actionButton("plot_button", "Generate Plot"),  # Button to generate plot
      actionButton("reset_button", "Reset")  # Reset button
    ),
    
    mainPanel(
      plotlyOutput("interactive_plot"),  # ggplotly interactive plot output
      radioButtons("interaction_mode", "Mode:", 
                   choices = list("Select Points" = "select", "Zoom/Pan" = "zoom"), 
                   inline = TRUE),
      verbatimTextOutput("brush_info")  # Displays selected points
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Reactive values to store the plot
  rv <- reactiveValues(plot = NULL)
  
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
      selectInput("parameters", "Select Parameter(s)", choices = cols[-1], multiple = TRUE)
    })
    
    output$airTemp_selector <- renderUI({
      req(data())
      cols <- colnames(data())
      selectInput("air_temp_param", "Select Air Temperature Parameter", choices = cols[-1])
    })
    
    output$precip_selector <- renderUI({
      req(data())
      cols <- colnames(data())
      selectInput("precip_param", "Select Precipitation Parameter", choices = cols[-1])
    })
  })
  
  # Generate ggplot on button click
  observeEvent(input$plot_button, {
    req(data(), input$parameters)
    df <- data()
    
    # Create a base Plotly scatterplot with multiple traces for selected parameters
    plot <- plot_ly(df, x = ~df[[1]])  # First column as X-axis
    
    for (param in input$parameters) {
      plot <- plot %>%
        add_trace(y = df[[param]], type = "scatter", mode = "lines", name = param)
    }
    
    # Store the plot in reactiveValues
    rv$plot <- plot
    output$interactive_plot <- renderPlotly({ rv$plot })
  })
  
  # Use plotlyProxy to update mode **without redrawing**
  observeEvent(input$interaction_mode, {
    req(rv$plot)  # Ensure a plot exists
    plotlyProxy("interactive_plot", session) %>%
      plotlyProxyInvoke("relayout", list(dragmode = input$interaction_mode))
  })
  
  # Display brushed points correctly
  output$brush_info <- renderPrint({
    selected <- event_data("plotly_selected")  # Get brushed points
    if (is.null(selected) || length(selected) == 0) {
      "No points selected"
    } else {
      selected  # Print the list of selected points
    }
  })
  
  # Reset button
  observeEvent(input$reset_button, {
    rv$plot <- NULL
    output$interactive_plot <- renderPlotly({ NULL })
  })
}

# Run the app
shinyApp(ui = ui, server = server)