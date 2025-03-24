library(shiny)
library(ggplot2)
library(readr)
library(bslib)
library(tidyverse)
library(lubridate)
library(shinyjs)

options(shiny.maxRequestSize = 30 * 1024^2) 

# ---------------------------- Home Page UI ------------------------
home_ui <- fluidPage(
  useShinyjs(),  # Activate shinyjs
  
  # Custom JS to clear the brush overlay
  tags$script(HTML("
    Shiny.addCustomMessageHandler('resetBrush', function(plotId) {
      $('#' + plotId).trigger('mouseup');
    });
  ")),
  
  # Custom CSS for fixed download button and rotated slider text
  tags$style(HTML("
    .sidebar-wrapper {
      position: relative;
      height: 100%;
      padding-bottom: 60px; /* Space for download button */
    }
    .download-fixed {
      position: absolute;
      bottom: 0;
      left: 0;
      right: 0;
      padding: 10px;
      background-color: white;
    }
    .irs-grid-text {
      transform: rotate(-45deg);
      transform-origin: top right;
    }
  ")),
  
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("QA/QC Tool"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Step 1: Upload .csv File", accept = ".csv"),
      
      # Conditional UI: Only shows after file upload
      conditionalPanel(
        condition = "output.fileUploaded",
        div(
          class = "sidebar-wrapper",
          
          selectInput("edit_column", "Select Column to Edit", choices = NULL),
          selectInput("comparison_columns", "Select Columns for Comparison", choices = NULL, multiple = TRUE),
          fluidRow(
            column(6, uiOutput("precip_select")),
            column(6, uiOutput("temp_select"))
          ),
          fluidRow(
            column(6, actionButton("plot_button", "Generate Plot", width = "100%", class = "btn btn-info")),
            column(6, actionButton("reset_graph", "Reset Graph", width = "100%", class = "btn btn-info"))
          ),
          br(),
          wellPanel(
            h6("Removal Options"),
            # The refinement slider UI will be rendered here when a brush event occurs.
            uiOutput("refinement_slider_ui"),
            fluidRow(
              column(6, actionButton("clear_selection", "Clear Selection", class = "btn btn-warning", width = "100%")),
              column(6, actionButton("confirm_removal", "Confirm Removal", class = "btn btn-success", width = "100%"))
            ),
            br(),
            column(6, actionButton("save_changes", "Save Changes", class = "btn btn-primary", width = "100%"))
          ),
          br(),
          div(
            class = "download-fixed",
            downloadButton("Download")
          )
        )
      )
    ),
    
    mainPanel(
      plotOutput("static_plot", 
                 brush = brushOpts(id = "plot_brush", direction = "x", resetOnNew = FALSE)),
      br(),
      uiOutput("date_slider_ui")
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
        tags$li("Brush (click and drag horizontally) on the plot to select a subset of data. After brushing, a refinement slider will appear in the Removal Options panel."),
        tags$li("Adjust the refinement slider as needed, then click 'Confirm Removal' to set the brushed values for the edit column to NA."),
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
  rv <- reactiveValues(
    data = NULL,
    filtered_data = NULL,
    date_range = NULL,
    edit_df = NULL,
    original = NULL,
    sel_edit = NULL,
    sel_comp = NULL,
    removal_confirmed = FALSE,
    removal_range = NULL
  )
  plot_trigger <- reactiveVal(0)
  
  # Reactive flag for file upload
  output$fileUploaded <- reactive({
    !is.null(input$file)
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  # Process file upload
  observeEvent(input$file, {
    req(input$file)
    df <- tryCatch({
      read_csv(input$file$datapath, show_col_types = FALSE)
    }, error = function(e) {
      showNotification("Invalid file format. Please upload a valid CSV file.", type = "error")
      return(NULL)
    })
    
    rv$original <- df  # keep untouched original data
    
    if (!is.null(df)) {
      date_col <- colnames(df)[1]
      df[[date_col]] <- as.POSIXct(df[[date_col]])
      rv$data <- df
      rv$date_range <- range(df[[date_col]], na.rm = TRUE)
      
      all_cols <- colnames(df)[-1]
      updateSelectInput(session, "edit_column", choices = all_cols, selected = all_cols[1])
      updateSelectInput(session, "comparison_columns", choices = all_cols)
    }
  })
  
  output$precip_select <- renderUI({
    req(rv$data)
    choices <- colnames(rv$data)[-1]
    selectInput("precip_select", "Select Precip. Column", choices = choices, 
                selected = choices[length(choices) - 1])
  })
  
  output$temp_select <- renderUI({
    req(rv$data)
    choices <- colnames(rv$data)[-1]
    selectInput("temp_select", "Select Temp. Column", choices = choices, 
                selected = choices[length(choices)])
  })
  
  # Removed auto-updating observer for comparison_columns to preserve saved selections.
  
  observeEvent(input$plot_button, {
    req(rv$data)
    date_col <- colnames(rv$data)[1]
    rv$filtered_data <- rv$data
    output$date_slider_ui <- renderUI({
      sliderInput("date_slider", "Data Range Slider",
                  min = rv$date_range[1],
                  max = rv$date_range[2],
                  value = rv$date_range,
                  timeFormat = "%Y-%m-%d %H:%M",
                  width = "100%")
    })
    show("refinement_slider_ui")
  })
  
  observeEvent(input$date_slider, {
    req(rv$data, input$date_slider)
    date_col <- colnames(rv$data)[1]
    rv$filtered_data <- rv$data %>%
      filter(.data[[date_col]] >= input$date_slider[1],
             .data[[date_col]] <= input$date_slider[2])
  })
  
  output$static_plot <- renderPlot({
    req(rv$data, rv$filtered_data, input$edit_column)
    date_col <- colnames(rv$data)[1]
    
    # Create a data frame for the edit column from the filtered data
    edit_df_all <- rv$filtered_data %>%
      select(all_of(c(date_col, input$edit_column))) %>%
      pivot_longer(cols = input$edit_column, names_to = "parameter", values_to = "value")
    
    # Use rv$data (full dataset) for the comparison columns, filtering by date if needed
    comparison_cols <- input$comparison_columns
    if (!is.null(comparison_cols) && length(comparison_cols) > 0) {
      comp_df <- rv$data %>%
        filter(.data[[date_col]] >= input$date_slider[1],
               .data[[date_col]] <= input$date_slider[2]) %>%
        select(all_of(c(date_col, comparison_cols))) %>%
        pivot_longer(cols = all_of(comparison_cols), names_to = "parameter", values_to = "value")
    }
    
    p <- ggplot() +
      { if (!is.null(comparison_cols) && length(comparison_cols) > 0)
        geom_line(data = comp_df, aes(x = .data[[date_col]], y = value, color = parameter),
                  linewidth = 1, alpha = 0.25) } +
      geom_line(data = edit_df_all, aes(x = .data[[date_col]], y = value, color = parameter),
                linewidth = 1, alpha = 1) +
      geom_point(data = edit_df_all, aes(x = .data[[date_col]], y = value, color = parameter),
                 size = 1.25)
    
    # If removal has been confirmed, highlight only points in the brushed area
    if (rv$removal_confirmed && !is.null(rv$removal_range)) {
      edit_df_removed <- edit_df_all %>%
        filter(.data[[date_col]] >= rv$removal_range[1],
               .data[[date_col]] <= rv$removal_range[2])
      # Use a less saturated yellow (light yellow) for these points
      p <- p + geom_point(data = edit_df_removed, aes(x = .data[[date_col]], y = value),
                          size = 1.25, color = "#FFFFE0")
    }
    
    p <- p + labs(x = "Date", y = "Value") +
      theme_classic()
    
    # Draw red box(s) if brush or refinement values exist
    if (!is.null(input$plot_brush$xmin) && !is.null(input$plot_brush$xmax)) {
      p <- p + geom_rect(aes(xmin = as.POSIXct(input$plot_brush$xmin, origin = "1970-01-01"),
                             xmax = as.POSIXct(input$plot_brush$xmax, origin = "1970-01-01"),
                             ymin = -Inf, ymax = Inf),
                         fill = "red", alpha = 0.2, inherit.aes = FALSE)
    }
    if (!is.null(input$refinement_slider)) {
      p <- p + geom_rect(aes(xmin = input$refinement_slider[1], xmax = input$refinement_slider[2],
                             ymin = -Inf, ymax = Inf),
                         fill = "red", alpha = 0.2, inherit.aes = FALSE)
    }
    
    p
  })
  
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
      
      # Clear the brush overlay
      session$sendCustomMessage("resetBrush", "static_plot")
    }
  })
  
  observeEvent(input$confirm_removal, {
    req(input$refinement_slider, rv$filtered_data, input$edit_column)
    date_col <- colnames(rv$data)[1]
    removal_min <- input$refinement_slider[1]
    removal_max <- input$refinement_slider[2]
    
    # Save the removal range so we know which points to highlight
    rv$removal_range <- c(removal_min, removal_max)
    
    # Work only with date and the column to be edited
    working_df <- rv$filtered_data %>%
      select(all_of(c(date_col, input$edit_column))) %>%
      mutate(
        !!sym(input$edit_column) := ifelse(
          .data[[date_col]] >= removal_min & .data[[date_col]] <= removal_max,
          NA,
          .data[[input$edit_column]]
        )
      )
    
    rv$edit_df <- working_df
    # Set flag so that the plot will highlight only the brushed-area points
    rv$removal_confirmed <- TRUE
  })
  
  observeEvent(input$save_changes, {
    req(rv$edit_df, rv$filtered_data)
    
    # Store the current selections in reactive values
    rv$sel_edit <- input$edit_column
    rv$sel_comp <- input$comparison_columns
    
    date_col <- colnames(rv$filtered_data)[1]
    
    updated <- rv$filtered_data %>%
      select(-all_of(input$edit_column)) %>%
      right_join(rv$edit_df, by = date_col) %>%
      arrange(.data[[date_col]])
    
    # Update the working dataset and filtered view
    rv$data <- updated
    rv$filtered_data <- updated
    rv$edit_df <- NULL
    
    # Update the select inputs using the stored selections
    all_cols <- colnames(updated)[-1]
    
    updateSelectInput(session, "edit_column", choices = all_cols, selected = rv$sel_edit)
    updateSelectInput(session, "comparison_columns", 
                      choices = setdiff(all_cols, rv$sel_edit), 
                      selected = rv$sel_comp)
    
    # Force the plot to re-render
    plot_trigger(plot_trigger() + 1)
    
    # Clear the brush overlay and remove the red box
    session$sendCustomMessage("resetBrush", "static_plot")
    output$refinement_slider_ui <- renderUI({ NULL })
    
    showNotification("Changes saved to working data.", type = "message")
  })
  
  observeEvent(input$clear_selection, {
    session$sendCustomMessage("resetBrush", "static_plot")
  })
  
  observeEvent(input$reset_graph, {
    req(rv$data)
    rv$filtered_data <- rv$data
    date_col <- colnames(rv$data)[1]
    updateSliderInput(session, "date_slider", value = rv$date_range)
  })
}

# ---------------------- Launch App ----------------------
shinyApp(ui = ui, server = server)