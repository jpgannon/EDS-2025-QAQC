library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

options(shiny.maxRequestSize = 30 * 1024^2)  # Increase file upload size limit

# Define UI
ui <- fluidPage(
  titlePanel("File Upload and Interactive ggplot"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),  # File upload input
      uiOutput("x_axis_selector"),  # X-axis selection
      uiOutput("y_axis_selector"),  # Y-axis selection (multiple)
      actionButton("plot_button", "Generate Plot"),  # Button to generate plot
      actionButton("reset_button", "Reset")  # Reset button
    ),
    
    mainPanel(
      plotOutput("scatter_plot", brush = brushOpts(id = "plot_brush")),  # GGplot brushing
      tableOutput("selected_points")  # Displays selected brushed points
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
    output$x_axis_selector <- renderUI({
      req(data())
      cols <- colnames(data())
      selectInput("x_axis", "Select X-Axis", choices = cols, selected = cols[1])
    })
    
    output$y_axis_selector <- renderUI({
      req(data())
      cols <- colnames(data())
      selectInput("y_axis", "Select Y-Axis Parameter(s)", choices = cols, multiple = TRUE)
    })
  })
  
  # Reactive value to store plot data
  plot_data <- reactiveVal(NULL)
  
  # Generate ggplot on button click
  observeEvent(input$plot_button, {
    req(data(), input$x_axis, input$y_axis)
    df <- data()
    
    # Ensure at least one Y-axis parameter is selected
    if (length(input$y_axis) == 0) {
      showNotification("Please select at least one Y-axis parameter.", type = "error")
      return()
    }
    
    # Reshape data for ggplot (for multiple Y-axes)
    df_long <- df %>%
      select(all_of(c(input$x_axis, input$y_axis))) %>%
      pivot_longer(cols = all_of(input$y_axis), names_to = "Parameter", values_to = "Value")
    
    # Store plot data
    plot_data(df_long)
    
    output$scatter_plot <- renderPlot({
      ggplot(df_long, aes_string(x = input$x_axis, y = "Value", color = "Parameter")) +
        geom_point(size = 3, alpha = 0.8) +
        geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +  # Add trendlines
        labs(title = "Interactive Scatter Plot with Brushing",
             x = input$x_axis,
             y = "Values",
             color = "Parameter") +
        theme_minimal()
    })
  })
  
  # Display brushed (selected) points
  output$selected_points <- renderTable({
    req(plot_data())
    brushedPoints(plot_data(), input$plot_brush)
  })
  
  # Reset button functionality
  observeEvent(input$reset_button, {
    output$scatter_plot <- renderPlot({ NULL })
    output$selected_points <- renderTable({ NULL })
    updateSelectInput(session, "x_axis", selected = character(0))
    updateSelectInput(session, "y_axis", selected = character(0))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
