library(shiny)
library(ggplot2)
library(readr)
library(bslib)
library(tidyverse)
library(lubridate)
library(shinyjs)

# ---------------------------- Home Page UI ------------------------
home_ui <- fluidPage(
  useShinyjs(),  # Activate shinyjs
  # Custom JS to clear the brush overlay when receiving the "resetBrush" message
  tags$script(HTML("
    Shiny.addCustomMessageHandler('resetBrush', function(plotId) {
      $('#' + plotId).trigger('mouseup');
    });
  ")),
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("QA/QC Tool"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      uiOutput("edit_column_ui"),
      uiOutput("comparison_selector_ui"),
      fluidRow(
        column(6, uiOutput("precip_select")),
        column(6, uiOutput("temp_select"))
      ),
      fluidRow(
        column(6, actionButton("plot_button", "Generate Plot", width = "100%")),
        column(6, actionButton("reset_graph", "Reset Graph", width = "100%"))
      ),
      br(),
      wellPanel(
        h6("Removal Options"),
        # Removal slider from brushed selection
        uiOutput("removal_slider_ui"),
        br(),
        # Refinement slider to fine-tune endpoints
        uiOutput("refinement_slider_ui"),
        br(),
        fluidRow(
          column(6, actionButton("clear_selection", "Clear Selection", class = "btn btn-warning", width = "100%")),
          column(6, actionButton("confirm_removal", "Confirm Removal", class = "btn btn-success", width = "100%"))
        ),
        br(),
        actionButton("remove_final", "⚠️ Are You Sure? ", width = "100%", class = "btn btn-danger")
      ),
        downloadButton("Download", style = "margin-top: 400px")
    ),
    mainPanel(
      plotOutput("static_plot", 
                 brush = brushOpts(id = "plot_brush", direction = "x", resetOnNew = TRUE)),
      br(),
      uiOutput("date_slider_ui")
      # Note: The brushed data table has been removed.
    )
  )
)

# ---------------------- Documentation Page UI ----------------------
documentation_ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Documentation"),
  sidebarLayout(
    sidebarPanel(
      h3("How to Use This App")
    ),
    mainPanel(
      h4("Overview"),
      p("This app lets you upload a CSV file where the first column contains date-times (down to the minute) and the remaining columns are numeric parameters."),
      h4("Steps to Use the App"),
      tags$ol(
        tags$li("Upload your CSV file on the Home page."),
        tags$li("Select the column you want to edit from the 'Select Column to Edit' dropdown."),
        tags$li("Select additional columns for comparison using the 'Select Columns for Comparison' dropdown."),
        tags$li("Click 'Generate Plot' to create a dynamic plot. A date slider will then appear beneath the plot."),
        tags$li("Use the date slider to filter the data by date and time."),
        tags$li("Brush (click and drag horizontally) on the plot to select a subset of data. After brushing, a removal slider and a refinement slider will appear in the Removal Options panel."),
        tags$li("Adjust the refinement slider as needed, then click 'Confirm Removal' to set the brushed values for the edit column to NA (without modifying the original data)."),
        tags$li("Click 'Clear Selection' to clear the brush overlay, and 'Reset Graph' to revert to the original data.")
      ),
      h4("Notes"),
      p("Ensure your CSV file has a header row with the first column as date-times and the remaining columns as numeric values."),
      p("The app uses ggplot2 for plotting and restricts brushing to the horizontal axis.")
    )
  )
)

# ---------------------- Overall UI ----------------------
ui <- navbarPage(
  title = "HBEF Data Cleaner",
  tabPanel("Home", home_ui),
  tabPanel("Documentation", documentation_ui)
)

# ---------------------- Server ----------------------
server <- function(input, output, session) {
  
  rv <- reactiveValues(data = NULL, filtered_data = NULL, date_range = NULL)
  
  # Initially hide the date slider UI and removal sliders
  hide("date_slider_ui")
  hide("removal_slider_ui")
  hide("refinement_slider_ui")
  
  # Process CSV upload
  observeEvent(input$file, {
    req(input$file)
    df <- tryCatch({
      read_csv(input$file$datapath, show_col_types = FALSE)
    }, error = function(e) {
      showNotification("Invalid file format. Please upload a valid CSV file.", type = "error")
      return(NULL)
    })
    if (!is.null(df)) {
      # Convert first column to POSIXct for minute-level resolution.
      date_col <- colnames(df)[1]
      df[[date_col]] <- as.POSIXct(df[[date_col]])
      rv$data <- df
      rv$date_range <- range(df[[date_col]], na.rm = TRUE)
      # Date slider UI will be rendered after plot generation.
    }
  })
  
  # UI for selecting column to edit (exclude the first column)
  output$edit_column_ui <- renderUI({
    req(rv$data)
    selectInput("edit_column", "Select Column to Edit",
                choices = colnames(rv$data)[-1],
                selected = NULL)
  })
  
  # UI for selecting comparison columns (exclude first column and the edit column)
  output$comparison_selector_ui <- renderUI({
    req(rv$data)
    choices <- colnames(rv$data)[-1]
    if (!is.null(input$edit_column))
      choices <- setdiff(choices, input$edit_column)
    selectInput("comparison_columns", "Select Columns for Comparison",
                choices = choices, multiple = TRUE)
  })
  
  output$precip_select <- renderUI({
    req(rv$data)
    choices <- colnames(rv$data)[-1]
    selectInput("precip_select", "Select Precip. Column", choices = choices, multiple = FALSE, selected = choices[length(choices) - 1])
  })
  
  output$temp_select <- renderUI({
    req(rv$data)
    choices <- colnames(rv$data)[-1]
    selectInput("temp_select", "Select Temp. Column", choices = choices, multiple = FALSE, selected = choices[length(choices)])
  })
  
  # When "Generate Plot" is clicked, update filtered_data, render the date slider UI, and show removal options.
  observeEvent(input$plot_button, {
    req(rv$data)
    date_col <- colnames(rv$data)[1]
    rv$filtered_data <- rv$data  # Initially, use all data
    output$date_slider_ui <- renderUI({
      sliderInput("date_slider", "Select Date Range",
                  min = rv$date_range[1], # lowest date value
                  max = rv$date_range[2], # highest date value 
                  value = rv$date_range,
                  timeFormat = "%Y-%m-%d %H:%M",
                  width = "100%")
    })
    show("date_slider_ui")
    show("refinement_slider_ui")  # Initially empty; will update on brush.
  })
  
  # Update filtered_data when the date slider changes.
  observeEvent(input$date_slider, {
    req(rv$data, input$date_slider)
    date_col <- colnames(rv$data)[1]
    rv$filtered_data <- rv$data %>%
      filter(.data[[date_col]] >= input$date_slider[1],
             .data[[date_col]] <= input$date_slider[2])
  })
  
  # Render the ggplot2 plot.
  output$static_plot <- renderPlot({
    
    req(rv$filtered_data, input$edit_column, input$comparison_columns)
    date_col <- colnames(rv$filtered_data)[1]
    plot_cols <- c(input$edit_column, input$comparison_columns)
    
    long_df <- rv$filtered_data %>%
      select(all_of(c(date_col, plot_cols))) %>%
      pivot_longer(cols = all_of(plot_cols), names_to = "parameter", values_to = "value")
    
    edit_df <- long_df %>% filter(parameter == input$edit_column)
    comp_df <- long_df %>% filter(parameter != input$edit_column)
    
    p <- ggplot() +
      geom_line(data = comp_df, aes(x = .data[[date_col]], y = value, color = parameter),
                linewidth = 1, alpha = 0.25) +
      geom_line(data = edit_df, aes(x = .data[[date_col]], y = value, color = parameter),
                linewidth = 1, alpha = 1) +
      geom_point(data = edit_df, aes(x = .data[[date_col]], y = value, color = parameter),
                 size = 1.25) +
      labs(x = "Date", y = "Value") +
      theme_classic()
    
    # If a removal slider exists (i.e. after brushing), overlay a rectangle.
    if (!is.null(input$refinement_slider)) {
      p <- p +
        geom_rect(aes(xmin = input$refinement_slider[1], xmax = input$refinement_slider[2],
                      ymin = -Inf, ymax = Inf),
                  fill = "red", alpha = 0.2, inherit.aes = FALSE)
    }
    p
  })
  
  # When the plot is brushed, render a removal slider and a refinement slider.
  observeEvent(input$plot_brush, {
    req(input$plot_brush)
    if (!is.null(input$plot_brush$xmin) && !is.null(input$plot_brush$xmax)) {
      brushed_min <- as.POSIXct(input$plot_brush$xmin, origin = "1970-01-01", tz = "UTC")
      brushed_max <- as.POSIXct(input$plot_brush$xmax, origin = "1970-01-01", tz = "UTC")
      
      output$refinement_slider_ui <- renderUI({
        sliderInput("refinement_slider", "Refine Removal Range",
                    min = brushed_min,
                    max = brushed_max,
                    value = c(brushed_min, brushed_max),
                    timeFormat = "%Y-%m-%d %H:%M",
                    width = "100%")
      })
      
      # Clear the brush after capturing it, so the user can refine using the slider.
      session$sendCustomMessage("resetBrush", "static_plot")
    }
  })
  
  # When "Confirm Removal" is clicked, set the brushed values in the edit column to NA using the refinement slider values.
  observeEvent(input$confirm_removal, {
    req(input$refinement_slider, rv$filtered_data, input$edit_column)
    
    date_col <- colnames(rv$data)[1]
    
    removal_min <- input$refinement_slider[1]
    removal_max <- input$refinement_slider[2]
    
    new_data <- rv$filtered_data
    new_data[new_data[[date_col]] >= removal_min & new_data[[date_col]] <= removal_max, input$edit_column] <- NA
    rv$filtered_data <- new_data
  })
  
  # When "Clear Selection" is clicked, clear the brush overlay.
  observeEvent(input$clear_selection, {
    session$sendCustomMessage("resetBrush", "static_plot")
  })
  
  # When "Reset Graph" is clicked, revert filtered_data to the original data and update the date slider.
  observeEvent(input$reset_graph, {
    req(rv$data)
    rv$filtered_data <- rv$data
    date_col <- colnames(rv$data)[1]
    updateSliderInput(session, "date_slider", value = rv$date_range)
  })
  
}

shinyApp(ui = ui, server = server)