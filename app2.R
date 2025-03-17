library(shiny)
library(ggplot2)
library(readr)
library(bslib)
library(tidyverse)

options(shiny.maxRequestSize = 30 * 1024^2)  # Increase file upload size limit

# Define UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "slate"),
  titlePanel("File Upload and Interactive Plot"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      uiOutput("param_edit"), 
      uiOutput("parameter_selector"),
      uiOutput("airTemp_selector"),
      uiOutput("precip_selector"),
      actionButton("plot_button", "Generate Plot"),
      actionButton("reset_button", "Reset Zoom")
    ),
    mainPanel(
      plotOutput("interactive_plot", brush = brushOpts(id = "plot_brush", resetOnNew = TRUE))
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values to store data
  rv <- reactiveValues(data = NULL, zoomed_data = NULL)
  
  # Read CSV file
  observeEvent(input$file, {
    req(input$file)
    rv$data <- tryCatch({
      read_csv(input$file$datapath, show_col_types = FALSE)
    }, error = function(e) {
      showNotification("Invalid file format. Please upload a valid CSV file.", type = "error")
      NULL
    })
    rv$zoomed_data <- rv$data  # Initialize zoomed data
  })
  
  # Reactive variable for available parameters
  available_parameters <- reactive({
    req(rv$data)
    setdiff(colnames(rv$data)[-1], input$param_edit %||% character(0))
  })
  
  # UI Selectors
  observeEvent(rv$data, {
    req(rv$data)
    
    output$param_edit <- renderUI({
      selectInput("param_edit", "Select Watershed to Edit", choices = colnames(rv$data)[-1], multiple = FALSE)
    })
    
    output$parameter_selector <- renderUI({
      req(available_parameters())
      selectInput("parameters", "Select Watershed(s) for Comparison", 
                  choices = available_parameters(), multiple = TRUE)
    })
    
    output$airTemp_selector <- renderUI({
      cols <- colnames(rv$data)
      selectInput("air_temp_param", "Select Air Temperature Parameter", 
                  choices = cols[-1], selected = ifelse(length(cols) > 1, cols[length(cols) - 1], NULL))
    })
    
    output$precip_selector <- renderUI({
      cols <- colnames(rv$data)
      selectInput("precip_param", "Select Precipitation Parameter", 
                  choices = cols[-1], selected = ifelse(length(cols) > 1, cols[length(cols)], NULL))
    })
  })
  
  # Plot Button
  observeEvent(input$plot_button, {
    req(rv$data, input$param_edit, input$air_temp_param, input$precip_param)
    rv$zoomed_data <- rv$data  # Reset zoomed data
    update_plot()
  })
  
  # Update Plot Function
  update_plot <- function() {
    req(rv$zoomed_data, input$param_edit, input$air_temp_param, input$precip_param)
    
    df <- rv$zoomed_data
    timestamp_col <- colnames(df)[1]
    
    cols_to_plot <- c(input$param_edit, input$air_temp_param, input$precip_param, input$parameters)
    cols_to_plot <- cols_to_plot[!is.null(cols_to_plot) & cols_to_plot != ""]  # Remove NULL/empty values
    
    if (length(cols_to_plot) == 0) return()  # Ensure there are columns to plot
    
    # Prepare the data for long format and pivot for plotting
    long_df <- df %>% 
      select(all_of(c(timestamp_col, cols_to_plot))) %>% 
      pivot_longer(cols = all_of(cols_to_plot), names_to = "parameter", values_to = "value")
    
    # Debug: Print the structure of long_df
    print("Structure of long_df:")
    print(str(long_df))
    
    # Check if the selected precipitation column exists in the dataset
    if (!(input$precip_param %in% colnames(df))) {
      stop(paste("Error: Column '", input$precip_param, "' not found in the dataset.", sep = ""))
    }
    
    # Ensure long_df has the 'parameter' column
    if (!"parameter" %in% colnames(long_df)) {
      stop("Error: 'parameter' column is missing in long_df.")
    }
    
    # Debugging step: Ensure long_df is not an unexpected object
    if (inherits(long_df, "data.frame")) {
      print("long_df is a valid data frame.")
    } else {
      stop("Error: long_df is not a valid data frame.")
    }
    
    output$interactive_plot <- renderPlot({
      ggplot(long_df, aes(x = .data[[timestamp_col]], y = value, color = parameter)) +
        # Line plot for selected parameters
        geom_line(size = 1) +
        
        # Inverted bar graph for precipitation (this uses long_df, not df)
        geom_bar(data = long_df, aes(x = .data[[timestamp_col]], y = - .data[[input$precip_param]]), 
                 stat = "identity", fill = "blue", alpha = 0.5, width = 0.1) +
        
        labs(x = "Timestamp", y = "Value", title = "Selected Data Over Time") +
        theme_classic() +
        
        # Adjust x-axis limits (if zoomed in)
        scale_x_datetime(limits = rv$x_range, expand = expansion(mult = 0.01)) +  
        
        # Adjust y-axis limits (if zoomed in)
        scale_y_continuous(limits = rv$y_range, expand = expansion(mult = 0.01))
    })
  }
  
  # Brush event for zooming
  observeEvent(input$plot_brush, {
    req(input$plot_brush, rv$data)
    
    brush <- input$plot_brush
    df <- rv$data
    timestamp_col <- colnames(df)[1]
    
    # Get selected columns
    cols_to_plot <- c(input$param_edit, input$air_temp_param, input$precip_param, input$parameters)
    cols_to_plot <- cols_to_plot[!is.null(cols_to_plot) & cols_to_plot != ""]  # Remove NULL/empty values
    
    if (length(cols_to_plot) == 0) return()  # Ensure there are columns to plot
    
    # Filter data before pivoting
    filtered_df <- df %>%
      filter(
        .data[[timestamp_col]] >= as.POSIXct(brush$xmin) & 
          .data[[timestamp_col]] <= as.POSIXct(brush$xmax)
      ) %>%
      select(all_of(c(timestamp_col, cols_to_plot)))  # Keep only relevant columns
    
    if (nrow(filtered_df) == 0) return()  # Ensure there is data after filtering
    
    rv$zoomed_data <- filtered_df  # Store filtered data directly
    
    update_plot()
  })
  
  # Reset Button
  observeEvent(input$reset_button, {
    rv$zoomed_data <- rv$data
    update_plot()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)