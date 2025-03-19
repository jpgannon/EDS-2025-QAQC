library(shiny)
library(shinyjs)
library(readr)
library(bslib)
library(tidyverse)
library(echarts4r)

options(shiny.maxRequestSize = 30 * 1024^2)  # Increase file upload size limit

# Define UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("File Upload and Interactive Plot"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      uiOutput("param_edit"), 
      uiOutput("parameter_selector"),
      actionButton("plot_button", "Generate Plot"),
      div(id = "loading_message", style = "color: white; font-weight: bold; display: none;", 
          "Loading... Please wait."),
      br(),
      verbatimTextOutput("selected_range"),
      actionButton("clear_brushed", "Clear Selection")
    ),
    mainPanel(
      echarts4rOutput("interactive_plot", height = "500px")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  rv <- reactiveValues(data = NULL, brushed_range = "No data selected.")
  
  # Read CSV file
  observeEvent(input$file, {
    req(input$file)
    rv$data <- tryCatch({
      read_csv(input$file$datapath, show_col_types = FALSE)
    }, error = function(e) {
      showNotification("Invalid file format. Please check your CSV.", type = "error")
      NULL
    })
  })
  
  # UI Selectors
  observeEvent(rv$data, {
    req(rv$data)
    cols <- colnames(rv$data)
    
    # Select Input for `param_edit`
    output$param_edit <- renderUI({
      selectInput("param_edit", "Select Parameter to Edit", choices = cols[-1], selected = cols[2])
    })
    
    # Select Input for `parameter_selector` (depends on `param_edit`)
    output$parameter_selector <- renderUI({
      selectInput("parameters", "Select Parameters for Comparison", 
                  choices = setdiff(cols[-1], input$param_edit), 
                  multiple = TRUE)
    })
  })
  
  # Update `parameter_selector` options when `param_edit` changes
  observeEvent(input$param_edit, {
    req(rv$data)
    cols <- colnames(rv$data)
    
    # Update the selectable columns, excluding the chosen `param_edit`
    updateSelectInput(session, "parameters", 
                      choices = setdiff(cols[-1], input$param_edit),
                      selected = NULL)  # Reset selection
  })
  
  # Generate Plot
  observeEvent(input$plot_button, {
    req(rv$data, input$param_edit, input$parameters)
    
    shinyjs::show("loading_message")
    Sys.sleep(2)  # Ensure UI updates before rendering
    
    df <- rv$data
    timestamp_col <- "TIMESTAMP"
    
    selected_columns <- unique(c(input$param_edit, input$parameters))
    selected_columns <- selected_columns[selected_columns %in% colnames(df)]
    
    if (length(selected_columns) == 0) {
      showNotification("No valid parameters selected for plotting.", type = "error")
      return()
    }
    
    df_long <- df %>%
      select(all_of(c(timestamp_col, selected_columns))) %>%
      pivot_longer(cols = -all_of(timestamp_col), names_to = "parameters", values_to = "value")
    
    # Render line graph with brushing
    output$interactive_plot <- renderEcharts4r({
      df_long %>%
        group_by(parameters) %>%
        e_charts(TIMESTAMP) %>%
        e_line(serie = value, smooth = TRUE, lineStyle = list(width = 1), 
               symbol = "none") %>%  
        e_brush(enabled = TRUE, 
                brushMode = "single",
                brushType = "rect") %>%
        e_datazoom(type = "inside") %>%
        e_datazoom(type = "slider", realtime = FALSE) %>%
        e_tooltip(trigger = "axis") %>%
        e_legend(TRUE)
    })
    
    shinyjs::hide("loading_message")
  })
  
  # Update UI display for selected range
  observeEvent(input$brushData, {
    req(input$brushData)
    
    # Convert timestamps properly
    brushed_timestamps <- unlist(input$brushData)  # Keep timestamps as strings
    
    # Debugging: Print timestamps to console
    print(brushed_timestamps)
    
    if (length(brushed_timestamps) > 0) {
      rv$brushed_range <- paste0("Start: ", brushed_timestamps[1], 
                                 " | End: ", brushed_timestamps[length(brushed_timestamps)])
    } else {
      rv$brushed_range <- "No data selected."
    }
  })
  
  # Display brushed range in UI
  output$selected_range <- renderText({
    req(rv$brushed_range)
    rv$brushed_range
  })
}

# Run the application 
shinyApp(ui = ui, server = server)