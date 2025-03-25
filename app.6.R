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
  useShinyjs(),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('resetBrush', function(plotId) {
      $('#' + plotId).trigger('mouseup');
    });
  ")),
  tags$style(HTML("
    .sidebar-wrapper {
      position: relative;
      height: 100%;
      padding-bottom: 60px;
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
            downloadButton("Download", "Download Cleaned Data")
          )
        )
      )
    ),
    
    mainPanel(
      plotOutput("static_plot", brush = brushOpts(id = "plot_brush", direction = "x", resetOnNew = FALSE)),
      br(),
      uiOutput("date_slider_ui"),
      br(),
      plotOutput("precip_plot"),
      plotOutput("temp_plot")
    )
  )
)

# ---------------------- Documentation Page UI ----------------------
documentation_ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Documentation"),
  sidebarLayout(
    sidebarPanel(h3("How to Use This App")),
    mainPanel(
      h4("Overview"),
      p("This app lets you upload a CSV file..."),
      h4("Steps to Use the App"),
      tags$ol(
        tags$li("Upload your CSV file..."),
        tags$li("Select the column you want to edit..."),
        tags$li("Select additional columns for comparison..."),
        tags$li("Click 'Generate Plot'..."),
        tags$li("Use the date slider..."),
        tags$li("Brush on the plot..."),
        tags$li("Adjust the refinement slider..."),
        tags$li("Click 'Clear Selection'...")
      ),
      h4("Notes"),
      p("Ensure your CSV file has a header row...")
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
  
  output$fileUploaded <- reactive({
    !is.null(input$file)
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  output$Download <- downloadHandler(
    filename = function() {
      paste0("cleaned_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$data)
      write_csv(rv$data, file)
    }
  )
  
  # Re-adding the server logic to enable dropdowns and plotting
  
  observeEvent(input$file, {
    req(input$file)
    df <- tryCatch({
      read_csv(input$file$datapath, show_col_types = FALSE)
    }, error = function(e) {
      showNotification("Invalid file format.", type = "error")
      return(NULL)
    })
    rv$original <- df
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
    selectInput("precip_select", "Select Precip. Column", choices = choices, selected = choices[length(choices) - 1])
  })
  
  output$temp_select <- renderUI({
    req(rv$data)
    choices <- colnames(rv$data)[-1]
    selectInput("temp_select", "Select Temp. Column", choices = choices, selected = choices[length(choices)])
  })
  
  observeEvent(input$plot_button, {
    req(rv$data)
    date_col <- colnames(rv$data)[1]
    rv$filtered_data <- rv$data
    output$date_slider_ui <- renderUI({
      sliderInput("date_slider", "Data Range Slider", min = rv$date_range[1], max = rv$date_range[2],
                  value = rv$date_range, timeFormat = "%Y-%m-%d %H:%M", width = "100%")
    })
    show("refinement_slider_ui")
  })
  
  observeEvent(input$date_slider, {
    req(rv$data, input$date_slider)
    date_col <- colnames(rv$data)[1]
    rv$filtered_data <- rv$data %>%
      filter(.data[[date_col]] >= input$date_slider[1], .data[[date_col]] <= input$date_slider[2])
  })
  
  output$static_plot <- renderPlot({
    req(rv$data, rv$filtered_data, input$edit_column)
    date_col <- colnames(rv$data)[1]
    
    edit_df_all <- rv$filtered_data %>%
      select(all_of(c(date_col, input$edit_column))) %>%
      pivot_longer(cols = input$edit_column, names_to = "parameter", values_to = "value")
    
    comparison_cols <- input$comparison_columns
    if (!is.null(comparison_cols) && length(comparison_cols) > 0) {
      comp_df <- rv$data %>%
        filter(.data[[date_col]] >= input$date_slider[1], .data[[date_col]] <= input$date_slider[2]) %>%
        select(all_of(c(date_col, comparison_cols))) %>%
        pivot_longer(cols = all_of(comparison_cols), names_to = "parameter", values_to = "value")
    }
    
    p <- ggplot() +
      { if (!is.null(comparison_cols) && length(comparison_cols) > 0)
        geom_line(data = comp_df, aes(x = .data[[date_col]], y = value, color = parameter), linewidth = 1, alpha = 0.25) } +
      geom_line(data = edit_df_all, aes(x = .data[[date_col]], y = value, color = parameter), linewidth = 1, alpha = 1) +
      geom_point(data = edit_df_all, aes(x = .data[[date_col]], y = value, color = parameter), size = 1.25)
    
    if (rv$removal_confirmed && !is.null(rv$removal_range)) {
      edit_df_removed <- edit_df_all %>%
        filter(.data[[date_col]] >= rv$removal_range[1], .data[[date_col]] <= rv$removal_range[2])
      p <- p + geom_point(data = edit_df_removed, aes(x = .data[[date_col]], y = value), size = 1.25, color = "#FFFFE0")
    }
    
    if (!is.null(input$plot_brush$xmin) && !is.null(input$plot_brush$xmax)) {
      p <- p + geom_rect(aes(xmin = as.POSIXct(input$plot_brush$xmin, origin = "1970-01-01"),
                             xmax = as.POSIXct(input$plot_brush$xmax, origin = "1970-01-01"),
                             ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.2, inherit.aes = FALSE)
    }
    if (!is.null(input$refinement_slider)) {
      p <- p + geom_rect(aes(xmin = input$refinement_slider[1], xmax = input$refinement_slider[2],
                             ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.2, inherit.aes = FALSE)
    }
    
    p + labs(x = "Date", y = "Value") + theme_classic()
  })
  
  output$precip_plot <- renderPlot({
    req(rv$filtered_data, input$precip_select)
    date_col <- colnames(rv$filtered_data)[1]
    ggplot(rv$filtered_data, aes_string(x = date_col, y = input$precip_select)) +
      geom_line(color = "blue") +
      geom_point(color = "blue", size = 1.25) +
      labs(title = "Precipitation", x = "Date", y = input$precip_select) +
      theme_minimal()
  })
  
  output$temp_plot <- renderPlot({
    req(rv$filtered_data, input$temp_select)
    date_col <- colnames(rv$filtered_data)[1]
    ggplot(rv$filtered_data, aes_string(x = date_col, y = input$temp_select)) +
      geom_line(color = "red") +
      geom_point(color = "red", size = 1.25) +
      labs(title = "Air Temperature", x = "Date", y = input$temp_select) +
      theme_minimal()
  })
  observeEvent(input$plot_brush, {
    req(input$plot_brush)
    brushed_min <- as.POSIXct(input$plot_brush$xmin, origin = "1970-01-01", tz = "UTC")
    brushed_max <- as.POSIXct(input$plot_brush$xmax, origin = "1970-01-01", tz = "UTC")
    output$refinement_slider_ui <- renderUI({
      sliderInput("refinement_slider", "Refine Removal Range", min = brushed_min, max = brushed_max,
                  value = c(brushed_min, brushed_max), timeFormat = "%Y-%m-%d %H:%M", width = "100%")
    })
    session$sendCustomMessage("resetBrush", "static_plot")
  })
  
  observeEvent(input$confirm_removal, {
    req(input$refinement_slider, rv$filtered_data, input$edit_column)
    date_col <- colnames(rv$data)[1]
    removal_min <- input$refinement_slider[1]
    removal_max <- input$refinement_slider[2]
    rv$removal_range <- c(removal_min, removal_max)
    working_df <- rv$filtered_data %>%
      select(all_of(c(date_col, input$edit_column))) %>%
      mutate(!!sym(input$edit_column) := ifelse(
        .data[[date_col]] >= removal_min & .data[[date_col]] <= removal_max,
        NA, .data[[input$edit_column]]
      ))
    rv$edit_df <- working_df
    rv$removal_confirmed <- TRUE
  })
  
  observeEvent(input$save_changes, {
    req(rv$edit_df, rv$filtered_data)
    rv$sel_edit <- input$edit_column
    rv$sel_comp <- input$comparison_columns
    date_col <- colnames(rv$filtered_data)[1]
    updated <- rv$filtered_data %>%
      select(-all_of(input$edit_column)) %>%
      right_join(rv$edit_df, by = date_col) %>%
      arrange(.data[[date_col]])
    rv$data <- updated
    rv$filtered_data <- updated
    rv$edit_df <- NULL
    all_cols <- colnames(updated)[-1]
    updateSelectInput(session, "edit_column", choices = all_cols, selected = rv$sel_edit)
    updateSelectInput(session, "comparison_columns", choices = setdiff(all_cols, rv$sel_edit), selected = rv$sel_comp)
    plot_trigger(plot_trigger() + 1)
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
