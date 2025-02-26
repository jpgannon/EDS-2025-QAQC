library(shiny)
library(ggplot2)
library(plotly)
library(readr)
library(bslib)

options(shiny.maxRequestSize = 30 * 1024^2)  # Increase file upload size limit

# Define UI
ui <- fluidPage(
  titlePanel("File Upload and Interactive Plot"),
  
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
      verbatimTextOutput("brush_info"),  # Displays selected points
    )
  )
)

# Define servers
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
      
      
      selectInput("air_temp_param", "Select Air Temperature Parameter", selected = cols[[length(cols) - 1]],choices = cols[-1])
    })
    
    output$precip_selector <- renderUI({
      req(data())
      cols <- colnames(data())
      selectInput("precip_param", "Select Precipitation Parameter",selected = cols[[length(cols)]], choices = cols[-1])
    })
  })
  
  # Generate on button click
  observeEvent(input$plot_button, {
    
    req(data(), input$parameters, input$air_temp_param, input$precip_param)
    df <- data()
    
    # Main plot: User-selected parameters
    main_plot <- plot_ly(df, x = df[[1]])  # First column as X-axis
    
    for (param in input$parameters) {
      main_plot <- main_plot %>%
        add_trace(y = df[[param]], type = "scatter", mode = "lines", name = param)
    }
    
    # Air temperature subplot
    airtemp_plot <- plot_ly(df, x = ~df[[1]]) %>%
      add_trace(y = ~df[[input$air_temp_param]], type = "scatter", name = "Air Temp")
    
    # Precipitation subplot
    precip_plot <- plot_ly(df, x = ~df[[1]]) %>%
      add_trace(y = ~df[[input$precip_param]], type = "scatter", name = "Precipitation")
    
    # Combine all three plots into a vertical subplot layout
    combined_plot <- subplot(main_plot, airtemp_plot, precip_plot, nrows = 3, shareX = TRUE, titleX = FALSE)
    
    # Store the plot in reactiveValues
    rv$plot <- combined_plot
    output$interactive_plot <- renderPlotly({ rv$plot })
  
    })
  
    output$brush_info <- renderPrint({
     
      selected <- event_data("plotly_selected")  # Capture selected points
      summary(selected)
    })

  
  # Reset button
  observeEvent(input$reset_button, {
    rv$plot <- NULL
    output$interactive_plot <- renderPlotly({ NULL })
  }) 
}

# Run the app
shinyApp(ui = ui, server = server)