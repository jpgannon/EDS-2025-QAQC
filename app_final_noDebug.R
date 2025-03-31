required_versions <- list(
  shiny = "1.8.0",
  ggplot2 = "3.5.0",
  readr = "2.1.5",
  bslib = "0.6.1",
  tidyverse = "2.0.0",
  lubridate = "1.9.3",
  shinyjs = "2.1.0",
  DT = "0.32",
  later = "1.3.1",
  ggthemes = "5.0.0"
)

# Install missing packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Check versions and warn if outdated
check_version <- function(pkg, required_version) {
  installed <- packageVersion(pkg)
  if (installed < required_version) {
    warning(paste0("Package ", pkg, " is version ", installed,
                   ", but version ", required_version, " or higher is required."))
  }
}

# Ensure all packages are installed and version-checked
for (pkg in names(required_versions)) {
  install_if_missing(pkg)
  suppressWarnings(check_version(pkg, required_versions[[pkg]]))
}

# Load packages
library(shiny)
library(ggplot2)
library(readr)
library(bslib)
library(tidyverse)
library(lubridate)
library(shinyjs)
library(DT)
library(later)
library(ggthemes)

options(shiny.maxRequestSize = 30 * 1024^2)
  
  # ---------------------------- Home Page UI ------------------------
  home_ui <- fluidPage(
    useShinyjs(),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('resetBrush', function(plotId) {
        if ($('#' + plotId).length > 0) {
          $('#' + plotId).trigger('mouseup');
        }
      });
    ")),
    tags$style(HTML("
      .sidebar-wrapper { position: relative; height: 100%; padding-bottom: 60px; }
      .download-fixed { position: absolute; bottom: 0; left: 0; right: 0; padding: 10px; background-color: white; }
      .irs-grid-text { transform: rotate(-45deg); transform-origin: top right; }
    ")),
    theme = bs_theme(bootswatch = "flatly"),
    titlePanel("QA/QC Tool"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Step 1: Upload .csv File", accept = ".csv"),
        conditionalPanel(
          condition = "output.fileUploaded",
          div(class = "sidebar-wrapper",
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
                textOutput("range_text"),
                uiOutput("removal_slider_ui"),
                fluidRow(
                  column(3, actionButton("nudge_left_minus", "Left -", class = "btn btn-warning", width = "100%")),
                  column(3, actionButton("nudge_left_plus", "Left +", class = "btn btn-warning", width = "100%")),
                  column(3, actionButton("nudge_right_minus", "Right -", class = "btn btn-warning", width = "100%")),
                  column(3, actionButton("nudge_right_plus", "Right +", class = "btn btn-warning", width = "100%"))
                ),
                br(),
                fluidRow(
                  column(4, actionButton("confirm_removal", "Make NA", class = "btn btn-primary", width = "100%")),
                  column(4, actionButton("cancel_removal", "Cancel", class = "btn btn-danger", width = "100%")),
                  column(4, actionButton("save_changes", "💾 Save", class = "btn btn-success", width = "100%"))
                ),
                br()
              ),
              br(),
              div(class = "download-fixed", downloadButton("Download")),
              br(),
              h4("Selection Table"),
              DT::dataTableOutput("edit_table")
          )
        )
      ),
      mainPanel(
        plotOutput("precip_plot", height = "250px"),
        br(),
        plotOutput("static_plot", brush = brushOpts(id = "plot_brush", direction = "x", resetOnNew = FALSE)),
        br(),
        plotOutput("temp_plot", height = "250px"),
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
      sidebarPanel(h3("How to Use This App")),
      mainPanel(
        h4("Overview"),
        p("Upload a CSV file (first column = date-times, others numeric)."),
        h4("Steps to Use the App"),
        tags$ol(
          tags$li("Upload your CSV file on the Home page."),
          tags$li("Select the column to edit and additional columns for comparison."),
          tags$li("Click 'Generate Plot' to view the data with a date slider."),
          tags$li("Brush on the main plot to select an initial removal range."),
          tags$li("Use the removal slider or the four nudge buttons to fine-tune the left/right boundaries."),
          tags$li("Click 'Make NA' to set the selected values to NA."),
          tags$li("Then click 'Save Changes' to update the graph (with NA-ed values) and add a new column <column>_cleaned (overwriting if needed)."),
          tags$li("Use 'Clear Selection' or 'Reset Graph' as needed.")
        ),
        h4("Notes"),
        p("The graphs display precipitation (royalblue) and air temperature (orange)."),
        p("The final cleaned data is available for download on the Final Data Preview page.")
      )
    )
  )
  
  # ---------------------- Final Data Preview Page ----------------------
  final_preview_ui <- fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    titlePanel("Final Data Preview"),
    DT::dataTableOutput("final_preview_page")
  )
  
  # ---------------------- Overall UI ----------------------
  ui <- navbarPage(
    title = "HBEF Data Cleaner",
    tabPanel("Home", home_ui),
    tabPanel("Final Data Preview", final_preview_ui),
    tabPanel("Documentation", documentation_ui)
  )
  
  # ---------------------- Server ----------------------
  server <- function(input, output, session) {
    
    # Show fullscreen suggestion popup on app load
    observe({
      showNotification("This app is best in fullscreen.", type = "message", duration = 6)
    })
    
    theme_set(
      theme_linedraw(base_size = 12) +
        theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))
    )
    
      rv <- reactiveValues(
      data = NULL,
      filtered_data = NULL,
      date_range = NULL,
      edit_df = NULL,
      original = NULL,
      cleaned = NULL,  # Stores all cleaned columns (accumulated)
      removal_range = NULL,
      nudgeMode = FALSE,
      busy = FALSE,
      edit_df_backup = NULL # flag to block rapid nudge events
    )
    
    plot_trigger <- reactiveVal(0)
    
    output$fileUploaded <- reactive({ !is.null(input$file) })
    outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
    
    # Helper: assume date column is first.
    dateCol <- reactive({
      req(rv$data)
      colnames(rv$data)[1]
    })
    
    observeEvent(input$file, {
      req(input$file)
      df <- tryCatch(
        read_csv(input$file$datapath, show_col_types = FALSE),
        error = function(e) {
          showNotification("Invalid file format.", type = "error")
          return(NULL)
        }
      )
      rv$original <- df
      if (!is.null(df)) {
        date_col <- colnames(df)[1]
        df[[date_col]] <- as.POSIXct(df[[date_col]])
        rv$data <- df
        rv$date_range <- range(df[[date_col]], na.rm = TRUE)
        all_cols <- colnames(df)[-1]
        updateSelectInput(session, "edit_column", choices = all_cols, selected = all_cols[1])
        updateSelectInput(session, "comparison_columns", choices = setdiff(all_cols, all_cols[1]))
      }
    })
    
    observeEvent(input$edit_column, {
      req(rv$data)
      all_cols <- colnames(rv$data)[-1]
      updateSelectInput(session, "comparison_columns", choices = setdiff(all_cols, input$edit_column))
    })
    
    output$precip_select <- renderUI({
      req(rv$data)
      choices <- colnames(rv$data)[-1]
      sel <- if(any(tolower(choices) == "precip")) choices[tolower(choices) == "precip"][1] else tail(choices, 1)
      selectInput("precip_select", "Select Precipitation Column", choices = choices, selected = sel)
    })
    
    output$temp_select <- renderUI({
      req(rv$data)
      choices <- colnames(rv$data)[-1]
      sel <- if(any(tolower(choices) == "airtemp")) choices[tolower(choices) == "airtemp"][1] else head(choices, 1)
      selectInput("temp_select", "Select Air Temp Column", choices = choices, selected = sel)
    })
    
    nonint_data <- reactive({
      req(rv$original, input$date_slider)
      rv$original %>%
        filter(.data[[dateCol()]] >= input$date_slider[1],
               .data[[dateCol()]] <= input$date_slider[2])
    })
    
    observeEvent(input$plot_button, {
      req(rv$data)
      rv$filtered_data <- rv$data
      output$date_slider_ui <- renderUI({
        sliderInput("date_slider", "Data Range Slider",
                    min = rv$date_range[1],
                    max = rv$date_range[2],
                    value = rv$date_range,
                    timeFormat = "%Y-%m-%d %H:%M", width = "100%")
      })
    })
    
    observeEvent(input$date_slider, {
      req(rv$data)
      rv$filtered_data <- tryCatch({
        rv$data %>%
          filter(.data[[dateCol()]] >= input$date_slider[1],
                 .data[[dateCol()]] <= input$date_slider[2])
      }, error = function(e) { rv$data })
      # Reset nudging mode so that brushing can overwrite any previous state.
      rv$nudgeMode <- FALSE
    })
    
    # Only re-render the plot when removal_range changes (debounced)
    static_plot_data <- reactive({
      req(rv$data, rv$filtered_data, input$edit_column, input$date_slider)
      
      date_col <- dateCol()
      edit_source <- if (!is.null(rv$edit_df)) rv$edit_df else rv$filtered_data
      
      edit_df_all <- edit_source %>%
        select(all_of(c(date_col, input$edit_column))) %>%
        pivot_longer(cols = input$edit_column, names_to = "parameter", values_to = "value")
      
      comp_df <- NULL
      
      if (!is.null(input$comparison_columns) && length(input$comparison_columns) > 0) {
        comp_df <- rv$data %>%
          filter(.data[[date_col]] >= input$date_slider[1],
                 .data[[date_col]] <= input$date_slider[2]) %>%
          select(all_of(c(date_col, input$comparison_columns))) %>%
          pivot_longer(cols = all_of(input$comparison_columns), names_to = "parameter", values_to = "value")
      }
      
      list(edit_df_all = edit_df_all, comp_df = comp_df)
    })
    
    output$static_plot <- renderPlot({
      req(static_plot_data())
      date_col <- dateCol()
      p <- ggplot()
      
      if (!is.null(static_plot_data()$comp_df)) {
        p <- p + geom_line(data = static_plot_data()$comp_df,
                           aes(x = .data[[date_col]], y = value, color = parameter),
                           linewidth = 1, alpha = 0.25)
      }
      
      p <- p + geom_line(data = static_plot_data()$edit_df_all,
                         aes(x = .data[[date_col]], y = value, color = parameter),
                         linewidth = 1, alpha = 1) +
        geom_point(data = static_plot_data()$edit_df_all,
                   aes(x = .data[[date_col]], y = value, color = parameter),
                   size = 1.25)
      
      if (!is.null(rv$removal_range)) {
        p <- p + geom_point(data = static_plot_data()$edit_df_all %>%
                              filter(.data[[date_col]] >= rv$removal_range[1] &
                                       .data[[date_col]] <= rv$removal_range[2]),
                            aes(x = .data[[date_col]], y = value),
                            size = 2, color = "darkblue")
      }
      
      p + labs(x = NULL, y = "Value") + 
        theme(legend.margin = margin(0, 0, 0, 0),
              legend.position = c(0.99, 0.99),
              legend.justification = c("right", "top"))
    })
    
    output$range_text <- renderText({
      if (is.null(rv$removal_range))
        "No removal range selected."
      else
        paste("Current removal range:",
              format(rv$removal_range[1], "%Y-%m-%d %H:%M:%S"), "to",
              format(rv$removal_range[2], "%Y-%m-%d %H:%M:%S"))
    })
    
    output$removal_slider_ui <- renderUI({
      req(input$date_slider)
      if (is.null(rv$removal_range)) return(NULL)
      sliderInput("removal_slider", "Set Removal Range:",
                  min = input$date_slider[1],
                  max = input$date_slider[2],
                  value = rv$removal_range,
                  timeFormat = "%Y-%m-%d %H:%M:%S", width = "100%")
    })
    
    observeEvent(input$removal_slider, {
      rv$removal_range <- input$removal_slider
    })
    
    # Define the nudge step (in seconds)
    delta <- 300
    
    # Helper function for nudging the removal range.
    nudge_removal_range <- function(side, increment) {
      if (rv$busy) return()
      rv$busy <- TRUE
      current_range <- isolate(rv$removal_range)
      date_range <- isolate(input$date_slider)
      if (side == "left") {
        new_left <- if (increment < 0) {
          max(current_range[1] - delta, date_range[1])
        } else {
          min(current_range[1] + delta, current_range[2] - delta)
        }
        rv$removal_range <- c(new_left, current_range[2])
      } else if (side == "right") {
        new_right <- if (increment < 0) {
          max(current_range[2] - delta, current_range[1] + delta)
        } else {
          min(current_range[2] + delta, date_range[2])
        }
        rv$removal_range <- c(current_range[1], new_right)
      }
      later(function(){ rv$busy <- FALSE }, 0.1)
    }
    
    observeEvent(input$nudge_left_minus, { nudge_removal_range("left", -1) })
    observeEvent(input$nudge_left_plus,   { nudge_removal_range("left", 1) })
    observeEvent(input$nudge_right_minus, { nudge_removal_range("right", -1) })
    observeEvent(input$nudge_right_plus,  { nudge_removal_range("right", 1) })
    
    # "Make NA" uses the current removal range.
    observeEvent(input$confirm_removal, {
      req(rv$filtered_data, input$edit_column, rv$removal_range)
      
      rv$edit_df <- rv$filtered_data %>%
        select(all_of(c(dateCol(), input$edit_column))) %>%
        mutate(!!sym(input$edit_column) := ifelse(
          .data[[dateCol()]] >= rv$removal_range[1] & .data[[dateCol()]] <= rv$removal_range[2],
          NA,
          .data[[input$edit_column]]
        ))
    })
    
    # Save changes: update the full and filtered datasets and accumulate the cleaned column.
    observeEvent(input$save_changes, {
      req(rv$edit_df, rv$filtered_data)
      date_col <- dateCol()
      # Update full dataset for rows corresponding to the edited dates.
      full_data <- rv$data
      idx <- which(full_data[[date_col]] %in% rv$edit_df[[date_col]])
      full_data[idx, input$edit_column] <- rv$edit_df[[input$edit_column]]
      rv$data <- full_data
      
      # Update filtered dataset.
      filt_data <- rv$filtered_data
      idx2 <- which(filt_data[[date_col]] %in% rv$edit_df[[date_col]])
      filt_data[idx2, input$edit_column] <- rv$edit_df[[input$edit_column]]
      rv$filtered_data <- filt_data
      
      # Create new cleaned column name.
      new_col_name <- paste0(input$edit_column, "_cleaned")
      # Update full and filtered datasets with the new cleaned column.
      rv$data[[new_col_name]] <- rv$data[[input$edit_column]]
      rv$filtered_data[[new_col_name]] <- rv$filtered_data[[input$edit_column]]
      
      # Create a temporary data frame with the date and new cleaned column.
      temp_clean <- rv$data %>% select(!!sym(date_col), !!sym(new_col_name))
      # Accumulate cleaned columns: if rv$cleaned is NULL, initialize it; if the column exists, update it; else, full join.
      if (is.null(rv$cleaned)) {
        rv$cleaned <- temp_clean
      } else {
        if(new_col_name %in% colnames(rv$cleaned)){
          rv$cleaned[[new_col_name]] <- temp_clean[[new_col_name]]
        } else {
          rv$cleaned <- full_join(rv$cleaned, temp_clean, by = date_col)
        }
      }
      
      rv$edit_df <- NULL
      showNotification(paste("Changes saved for", input$edit_column), type = "message")
    })
    
    observeEvent(input$cancel_removal, {
      session$sendCustomMessage("resetBrush", "static_plot")
      rv$removal_range <- NULL
      rv$edit_df <- rv$edit_df_backup  # Restore previous selection (if it existed)
      showNotification("Make NA Cancelled", type = "message")
    })
    
    observeEvent(input$clear_selection, {
      session$sendCustomMessage("resetBrush", "static_plot")
    })
    
    observeEvent(input$reset_graph, {
      req(rv$data)
      rv$filtered_data <- rv$data
      updateSliderInput(session, "date_slider", value = rv$date_range)
    })
    
    output$edit_table <- DT::renderDataTable({
      req(rv$edit_df)
      DT::datatable(rv$edit_df, options = list(pageLength = 5))
    })
    
    output$precip_plot <- renderPlot({
      req(nonint_data(), input$precip_select)
      date_col <- colnames(nonint_data())[1]
      precip_col <- input$precip_select
      global_max <- max(rv$original[[precip_col]], na.rm = TRUE)
      ggplot(rv$original %>% filter(.data[[date_col]] >= input$date_slider[1],
                                    .data[[date_col]] <= input$date_slider[2]),
             aes_string(x = date_col, y = precip_col)) +
        geom_col(fill = "royalblue") +
        scale_y_reverse(limits = c(global_max, 0)) +
        labs(x = NULL,  y = "Precipitation")
    })
    
    output$temp_plot <- renderPlot({
      req(nonint_data(), input$temp_select)
      date_col <- colnames(nonint_data())[1]
      temp_col <- input$temp_select
      ggplot(rv$original %>% filter(.data[[date_col]] >= input$date_slider[1],
                                    .data[[date_col]] <= input$date_slider[2]),
             aes_string(x = date_col, y = temp_col)) +
        geom_line(color = "orange", na.rm = TRUE, size = 0.5) +
        geom_point(color = "orange", na.rm = TRUE, size = 0.5) +
        labs(x = NULL,  y = "Air Temperature") 
    })
    
    final_data <- reactive({
      req(rv$original)
      final_df <- rv$original
      if (!is.null(rv$cleaned))
        final_df <- left_join(final_df, rv$cleaned, by = dateCol())
      final_df
    })
    
    output$final_preview <- DT::renderDataTable({
      req(final_data())
      DT::datatable(final_data(), options = list(pageLength = 10))
    })
    
    output$final_preview_page <- DT::renderDataTable({
      req(final_data())
      DT::datatable(final_data(), options = list(pageLength = 10))
    })
    
    output$Download <- downloadHandler(
      filename = function() { paste0("updated_data_", Sys.Date(), ".csv") },
      content = function(file) { write_csv(final_data(), file) }
    )
  }
  
  shinyApp(ui = ui, server = server)