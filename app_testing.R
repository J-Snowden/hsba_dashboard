# Shinylive Dashboard for GitHub Pages
# This script creates a Shiny web application that can be hosted on GitHub Pages using Shinylive

library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(shinyWidgets)

# --- Data Loading and Pre-processing ---
# This section reads local CSVs and prepares the data objects for the app.

# Function to read the consolidated CSV file
load_data_and_process <- function(path = "data") {
  consolidated_file <- file.path(path, "all_tasks_long_by_task.csv")
  
  if (!file.exists(consolidated_file)) {
    # Return empty lists if consolidated file is not found
    return(list(data_list = list(), task_mapping = list(), column_order = list(), all_data_averages = list()))
  }
  
  # 1. Read the consolidated data file
  all_data <- read.csv(consolidated_file, stringsAsFactors = FALSE, colClasses = "character")
  
  if(nrow(all_data) == 0 || !"Task_Name" %in% names(all_data)) {
    return(list(data_list = list(), task_mapping = list(), column_order = list(), all_data_averages = list()))
  }
  
  # 2. Create data_list by filtering data by Task_Name
  unique_tasks <- unique(all_data$Task_Name)
  unique_tasks <- unique_tasks[!is.na(unique_tasks) & unique_tasks != ""]
  
  data_list <- list()
  for(task in unique_tasks) {
    data_list[[task]] <- all_data[all_data$Task_Name == task, ]
  }
  
  # 3. Create task_mapping (for user-friendly names)
  create_task_name <- function(task_name) {
    clean_name <- gsub("_clean$", "", task_name)
    clean_name <- gsub("_", " ", clean_name)
    tools::toTitleCase(clean_name)
  }
  task_mapping <- setNames(sapply(names(data_list), create_task_name), names(data_list))
  
  # 4. Create column_order (maintains original column order)
  column_order <- lapply(data_list, names)
  
  # 5. Create all_data_averages (pre-calculates averages for all 'w_' variables)
  # Use the consolidated data directly since it already contains all tasks
  combined_df <- all_data
  w_vars <- names(combined_df)[startsWith(names(combined_df), "w_")]
  demographicVars <- c("Race", "Gender", "Grade", "English")
  
  all_data_averages <- list()
  for (w_var in w_vars) {
    all_data_averages[[w_var]] <- list()
    for (dem_var in demographicVars) {
      if (dem_var %in% names(combined_df)) {
        avg_data <- combined_df %>%
          filter(!is.na(.data[[w_var]]) & .data[[w_var]] != "",
                 !is.na(.data[[dem_var]]) & .data[[dem_var]] != "") %>%
          mutate(w_value = as.numeric(.data[[w_var]])) %>%
          group_by(dem_group = .data[[dem_var]]) %>%
          summarise(average = mean(w_value, na.rm = TRUE), 
                    sd = if(n() > 1) sd(w_value, na.rm = TRUE) else NA,
                    count = n(), .groups = 'drop')
        
        # Structure the data as the app expects
        dem_list <- list()
        for (i in 1:nrow(avg_data)) {
          dem_list[[avg_data$dem_group[i]]] <- list(
            average = avg_data$average[i], 
            sd = avg_data$sd[i],
            count = avg_data$count[i]
          )
        }
        all_data_averages[[w_var]][[dem_var]] <- dem_list
      }
    }
  }
  
  return(list(
    data_list = data_list,
    task_mapping = task_mapping,
    column_order = column_order,
    all_data_averages = all_data_averages,
    consolidated_data = all_data
  ))
}

# Run the function to load and process data
processed <- load_data_and_process()
data_list <- processed$data_list
task_mapping <- processed$task_mapping
column_order <- processed$column_order
all_data_averages <- processed$all_data_averages

# Create cached combined data for performance
combined_data_cache <- NULL
w_vars_cache <- c()
if(length(data_list) > 0 && !is.null(processed$consolidated_data)) {
  # Use the consolidated data directly since it already contains all tasks
  combined_data_cache <- processed$consolidated_data
  # Cache list of w_ variables for faster filtering
  w_vars_cache <- names(combined_data_cache)[startsWith(names(combined_data_cache), "w_")]
}

# --- End of Data Loading Section ---

# Demographic mappings
demographic_mappings <- list(
  "Grade" = c("1" = "9th", "2" = "10th", "3" = "11th", "4" = "12th"),
  "Gender" = c("1" = "Man/Boy", "2" = "Woman/Girl", "3" = "My Gender is not listed.", "4" = "I prefer not to say."),
  "Race" = c("1" = "American Indian/Alaskan Native", "2" = "African American/Black", "3" = "Asian American/Asian",
             "4" = "Hispanic/Latinx", "5" = "Hawaiian/Pacific Islander", "6" = "White/European",
             "7" = "Multiracial/multiethnic", "8" = "My race/ethnicity is not listed.", "9" = "I prefer not to say."),
  "English" = c("1" = "English primary", "2" = "Difficulty with English.", "3" = "Somewhat Comfortable", "4" = "Very Comfortable")
)

# Define columns to exclude from variable dropdown
excluded_columns <- c(
  "Student_ID", "TID", "Test_Label", "Task_Position", "Start", "End", 
  "Time_Spent", "Grade", "F_L2", "Gender", "Race", "English", "Task_Time", 
  "NGSS_Sc_Adpot", "NGSS_Di_Adpot", "NGSS_Transition", "NGSS_Time", 
  "NGSS_Time_Other", "NGSS_Exp", "Cur_Purchased", "Cur_Free", 
  "Cur_School", "Cur_Designed", "Cur_Adapted", "Cur_Other", 
  "Res_OSE", "Res_BSCS", "Res_Other", "OSE_BSCS", "Task_Name", "Task_Prefix", "Test_Position"
)

demographicVars <- c("Race", "Gender", "Grade", "English", "Test_Position", "OSE_BSCS")

# F_L and SC item mappings
fl_item_mappings <- c("1" = "Disagree", "2" = "Somewhat disagree", "3" = "Somewhat agree", "4" = "Agree")
sc_item_mappings <- c("0" = "Incorrect", "1" = "Correct")

# Helper functions
map_demographic_value <- function(demographic, value) {
  if(length(demographic) > 1 || length(value) > 1) {
    # Vectorized version
    return(sapply(value, function(v) {
      if(demographic %in% names(demographic_mappings) && v %in% names(demographic_mappings[[demographic]])) {
        return(demographic_mappings[[demographic]][[v]])
      }
      return(v)
    }))
  } else {
    # Single value version
    if(demographic %in% names(demographic_mappings) && value %in% names(demographic_mappings[[demographic]])) {
      return(demographic_mappings[[demographic]][[value]])
    }
    return(value)
  }
}

map_variable_value <- function(variable, value) {
  if(length(value) > 1) {
    # Vectorized version
    return(sapply(value, function(v) {
      if(grepl("F_L", variable) && v %in% names(fl_item_mappings)) {
        return(fl_item_mappings[[v]])
      }
      if(endsWith(variable, "_SC") && v %in% names(sc_item_mappings)) {
        return(sc_item_mappings[[v]])
      }
      return(v)
    }))
  } else {
    # Single value version
    if(grepl("F_L", variable) && value %in% names(fl_item_mappings)) {
      return(fl_item_mappings[[value]])
    }
    if(endsWith(variable, "_SC") && value %in% names(sc_item_mappings)) {
      return(sc_item_mappings[[value]])
    }
    return(value)
  }
}

# Helper function to create display names for w_ variables
create_w_variable_name <- function(variable_name) {
  if (startsWith(variable_name, "w_")) {
    # Remove "w_" prefix, replace underscores with spaces, and apply title case
    clean_name <- gsub("w_", "", variable_name)
    clean_name <- gsub("_", " ", clean_name)
    clean_name <- tools::toTitleCase(clean_name)
    return(paste("Weighted", clean_name, "Factor"))
  }
  return(variable_name)
}

is_average_variable <- function(variable) {
  return(startsWith(variable, "w_"))
}

is_open_ended_variable <- function(variable) {
  return(endsWith(variable, "OE") || endsWith(variable, "DT") || 
           endsWith(variable, "OE1") || endsWith(variable, "OE2"))
}

is_categorical_chartable <- function(variable) {
  return(grepl("F_L", variable) || endsWith(variable, "_SC") || 
           endsWith(variable, "_MC") || endsWith(variable, "_CH"))
}

is_likert_variable <- function(variable) {
  return(grepl("F_L", variable))
}

# UI
ui <- fluidPage(
  title = "HSBA Data Dashboard",
  
  tags$head(
    tags$title("HSBA Data Dashboard"),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    ),
    tags$style(HTML("
      body {
        font-family: Arial, sans-serif;
        background-color: #f5f5f5;
        font-size: 14px;
      }
      .main-header {
        background: #4a90e2;
        color: white;
        padding: 30px 0;
        margin-bottom: 30px;
        border-radius: 0 0 8px 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .main-header h1 {
        margin: 0;
        text-align: center;
        font-weight: bold;
        font-size: 2.5em;
      }
      .control-panel {
        background: white;
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        margin-bottom: 20px;
      }
      .results-container {
        background: white;
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        margin-bottom: 20px;
      }
      .summary-box {
        background: #e7f3ff;
        padding: 15px;
        border-radius: 4px;
        margin-top: 20px;
      }
      .summary-box h4 {
        color: #333;
        margin-top: 0;
        font-weight: bold;
      }
      .select-input {
        margin-bottom: 15px;
      }
      .header-row {
        background-color: #f2f2f2 !important;
        font-weight: bold !important;
      }
      .overall-row {
        background-color: #f0f8ff !important;
        font-weight: bold !important;
      }
      .container {
        max-width: 1200px;
        margin: 0 auto;
        padding: 0 15px;
      }
      .btn-custom {
        background: #4a90e2;
        border: none;
        color: white;
        padding: 8px 16px;
        border-radius: 4px;
        cursor: pointer;
        font-size: 14px;
      }
      .btn-custom:hover {
        background: #357abd;
      }
      .show-all-btn {
        margin: 10px 0;
      }
      .dataTables_wrapper {
        font-size: 14px !important;
        margin-top: 0 !important;
      }
      .dataTables_wrapper table {
        font-size: 14px !important;
        border-collapse: collapse !important;
        width: 100% !important;
      }
      .dataTables_wrapper th, .dataTables_wrapper td {
        padding: 8px !important;
        font-size: 14px !important;
        border: 1px solid #ddd !important;
      }
      .dataTables_wrapper th {
        background-color: #f2f2f2 !important;
        font-weight: bold !important;
        border: 1px solid #ddd !important;
      }
      .clickable-link {
        color: #4a90e2;
        text-decoration: underline;
        cursor: pointer;
      }
      .clickable-link:hover {
        color: #357abd;
        text-decoration: none;
      }
      .image-preview {
        background: #4a90e2 !important;
        border: none !important;
        color: white !important;
        padding: 4px 8px !important;
        border-radius: 3px !important;
        cursor: pointer !important;
        font-size: 12px !important;
      }
      .image-preview:hover {
        background: #357abd !important;
      }
      .modal {
        display: none;
        position: fixed;
        z-index: 9999;
        left: 0;
        top: 0;
        width: 100%;
        height: 100%;
        background-color: rgba(0,0,0,0.8);
      }
      .modal-content {
        position: relative;
        margin: 5% auto;
        width: 90%;
        max-width: 800px;
        text-align: center;
      }
      .modal img {
        max-width: 100%;
        max-height: 80vh;
        border-radius: 8px;
      }
      .modal-close {
        position: absolute;
        top: -40px;
        right: 0px;
        color: white;
        font-size: 35px;
        font-weight: bold;
        cursor: pointer;
      }
      .modal-close:hover {
        color: #ccc;
      }
      .dataTables_filter {
        float: right !important;
        margin-bottom: 10px !important;
      }
      .dataTables_length {
        float: left !important;
        margin-bottom: 10px !important;
      }
      .loading-spinner {
        display: inline-block;
        width: 40px;
        height: 40px;
        border: 3px solid #f3f3f3;
        border-top: 3px solid #4a90e2;
        border-radius: 50%;
        animation: spin 1s linear infinite;
        margin: 20px auto;
      }
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
      .loading-container {
        text-align: center;
        padding: 50px;
        color: #666;
      }
      .selectize-input {
        text-align: left !important;
      }
      .selectize-dropdown-content .option {
        text-align: left !important;
      }
      .selectize-input > div.item {
        text-align: left !important;
      }
      .checkbox-group-header {
        font-weight: bold !important;
        color: #495057 !important;
        margin-top: 15px !important;
        margin-bottom: 8px !important;
      }
      .checkbox-group-header:first-child {
        margin-top: 0 !important;
      }
    ")),
    tags$script(HTML("
      $(document).ready(function() {
        // Image modal functionality
        $(document).on('click', '.image-preview', function(e) {
          e.preventDefault();
          var imgUrl = $(this).data('url');
          var modal = $('#imageModal');
          var modalImg = $('#modalImage');
          
          // Clear the old image first
          modalImg.attr('src', '');
          modal.show();
          
          // Load the new image
          modalImg.attr('src', imgUrl);
        });
        
        $(document).on('click', '.modal-close, .modal', function(e) {
          if (e.target === this) {
            $('#imageModal').hide();
            // Clear image when closing
            $('#modalImage').attr('src', '');
          }
        });
        
        $(document).keyup(function(e) {
          if (e.keyCode === 27) { // ESC key
            $('#imageModal').hide();
            // Clear image when closing
            $('#modalImage').attr('src', '');
          }
        });
      });
    "))
  ),
  
  div(class = "main-header",
      h1("HSBA Data Dashboard")
  ),
  
  div(class = "container",
      # Controls
      div(class = "control-panel",
          h3("Analysis Controls"),
          fluidRow(
            column(4,
                   div(class = "select-input",
                       selectInput("task_select", "Select Task:",
                                   choices = c("Choose a task..." = ""),
                                   selected = "")
                   )
            ),
            column(4,
                   div(class = "select-input",
                       selectInput("variable_select", "Select Variable:",
                                   choices = c("Choose a variable..." = ""),
                                   selected = "")
                   )
            ),
            column(4,
                   div(class = "select-input",
                       selectInput("demographic_select", "Select Demographic:",
                                   choices = c("Choose a demographic..." = "",
                                               "Grade" = "Grade",
                                               "Gender" = "Gender", 
                                               "Race" = "Race",
                                               "English" = "English",
                                               "Test Position" = "Test_Position",
                                               "OSE BSCS" = "OSE_BSCS"),
                                   selected = "")
                   )
            )
          )
      ),
      
      
      # Results
      div(class = "results-container",
          fluidRow(
            column(4, h3("Results", style = "margin-top: 0; margin-bottom: 15px;")),
            column(4, div(style = "margin-top: 0; display: flex; justify-content: center; align-items: center;", 
                          conditionalPanel(
                            condition = "input.task_select != '' && input.variable_select != '' && input.demographic_select != ''",
                            uiOutput("demographic_filter_dropdown_ui")
                          ))),
            column(4, div(style = "text-align: right; margin-top: 0; display: flex; justify-content: flex-end; align-items: center;", 
                          conditionalPanel(
                            condition = "input.task_select != '' && input.variable_select != '' && input.demographic_select != '' && input.results_tabs == 'Chart View'",
                            uiOutput("chart_controls_ui")
                          )))
          ),
          
          conditionalPanel(
            condition = "input.task_select == '' || input.variable_select == '' || input.demographic_select == ''",
            div(style = "text-align: center; padding: 20px; color: #666;",
                icon("table", "fa-2x"),
                h4("Please select all options above to view results"),
                p("Choose a task, variable, and demographic to generate the analysis table.")
            )
          ),
          
          conditionalPanel(
            condition = "input.task_select != '' && input.variable_select != '' && input.demographic_select != ''",
            tabsetPanel(
              id = "results_tabs",
              tabPanel("Table View", DT::dataTableOutput("results_table")),
              tabPanel("Chart View", 
                       conditionalPanel(
                         condition = "$('html').hasClass('shiny-busy')",
                         div(class = "loading-container",
                             div(class = "loading-spinner"),
                             h4("Loading chart...")
                         )
                       ),
                       plotOutput("results_chart", height = "600px")
              )
            )
          )
      ),
      
      # Summary
      conditionalPanel(
        condition = "input.task_select != '' && input.variable_select != '' && input.demographic_select != ''",
        htmlOutput("summary_output")
      )
  ),
  
  # Image modal
  div(id = "imageModal", class = "modal",
      div(class = "modal-content",
          span(class = "modal-close", "×"),
          img(id = "modalImage", src = "", alt = "Image preview")
      )
  )
)

# Server
server <- function(input, output, session) {
  
  # Initialize task choices
  observe({
    if(length(data_list) > 0) {
      # Create task choices with display names
      task_options <- list()
      for(key in names(data_list)) {
        display_name <- if(key %in% names(task_mapping)) task_mapping[[key]] else key
        task_options[[display_name]] <- key
      }
      
      # Sort alphabetically by display names
      sorted_names <- sort(names(task_options))
      task_choices <- c("Choose a task..." = "")
      for(display_name in sorted_names) {
        task_choices <- c(task_choices, setNames(task_options[[display_name]], display_name))
      }
      
      updateSelectInput(session, "task_select", choices = task_choices)
    }
  })
  
  # Update variable choices based on selected task
  observe({
    if(input$task_select != "" && input$task_select %in% names(data_list)) {
      if(input$task_select %in% names(column_order)) {
        available_variables <- column_order[[input$task_select]]
      } else {
        available_variables <- names(data_list[[input$task_select]])
      }
      
      # Get task prefix for filtering task-specific variables
      selected_data <- data_list[[input$task_select]]
      task_prefix <- ""
      if("Task_Prefix" %in% names(selected_data) && nrow(selected_data) > 0) {
        task_prefix <- selected_data$Task_Prefix[1]
      }
      
      # Filter variables: exclude unwanted columns, then keep only:
      # 1. Task-specific variables (those starting with task prefix)
      # 2. F_L and F_OE variables (common across tasks)
      # 3. w_ variables (weighted variables)
      filtered_variables <- available_variables[!available_variables %in% c(demographicVars, excluded_columns)]
      
      # Further filter to task-specific variables
      if(task_prefix != "" && task_prefix != "NA") {
        task_specific_variables <- filtered_variables[
          startsWith(filtered_variables, paste0(task_prefix, "_")) |  # Task-specific variables
            startsWith(filtered_variables, "F_L") |                     # F_L variables
            startsWith(filtered_variables, "F_OE") |                    # F_OE variables  
            startsWith(filtered_variables, "w_")                        # Weighted variables
        ]
        filtered_variables <- task_specific_variables
      }
      
      # Create user-friendly names for the dropdown
      variable_choices <- setNames(filtered_variables, sapply(filtered_variables, create_w_variable_name))
      
      updateSelectInput(session, "variable_select",
                        choices = c("Choose a variable..." = "", variable_choices),
                        selected = "")
    } else {
      updateSelectInput(session, "variable_select",
                        choices = c("Choose a variable..." = ""),
                        selected = "")
    }
  })
  
  # Generate demographic filter dropdown UI
  output$demographic_filter_dropdown_ui <- renderUI({
    req(input$demographic_select, input$task_select)
    
    if(input$task_select %in% names(data_list)) {
      # Get all available demographic data across all variables
      dataset <- data_list[[input$task_select]]
      
      # Create choices list with optgroups for pickerInput (grouped structure)
      choices_list <- list()
      
      # Define the order for demographics
      ordered_demographics <- c("Grade", "Gender", "Race", "English")
      
      for(demo_var in ordered_demographics) {
        if(demo_var %in% names(dataset)) {
          available_values <- unique(dataset[[demo_var]])
          available_values <- available_values[!is.na(available_values) & available_values != ""]
          
          if(length(available_values) > 0 && demo_var %in% names(demographic_mappings)) {
            # Sort available values for consistent ordering
            if(demo_var == "Grade") {
              # Order grades numerically
              available_values <- available_values[order(as.numeric(available_values))]
            } else {
              available_values <- sort(available_values)
            }
            
            # Create options for this demographic group
            demo_options <- c()
            for(val in available_values) {
              if(val %in% names(demographic_mappings[[demo_var]])) {
                label <- demographic_mappings[[demo_var]][[val]]
                option_key <- paste0(demo_var, "_", val)
                demo_options <- c(demo_options, setNames(option_key, label))
              }
            }
            
            # Add this demographic group to choices_list as an optgroup
            if(length(demo_options) > 0) {
              choices_list[[demo_var]] <- demo_options
            }
          }
        }
      }
      
      # Get default selected values (all sub-options of currently selected demographic)
      default_selected <- c()
      if(input$demographic_select %in% names(dataset)) {
        available_values <- unique(dataset[[input$demographic_select]])
        available_values <- available_values[!is.na(available_values) & available_values != ""]
        
        for(val in available_values) {
          if(val %in% names(demographic_mappings[[input$demographic_select]])) {
            option_key <- paste0(input$demographic_select, "_", val)
            default_selected <- c(default_selected, option_key)
          }
        }
      }
      
      if(length(choices_list) > 0) {
        div(style = "min-width: 250px;",
            pickerInput("demographic_filter_multi", 
                        "Filter Demographic Groups:",
                        choices = choices_list,
                        selected = default_selected,
                        multiple = TRUE,
                        options = list(
                          `actions-box` = TRUE,
                          `selected-text-format` = "count > 3",
                          `count-selected-text` = "{0} demographic groups selected"
                        ))
        )
      }
    }
  })
  
  # Update picker selections when demographic selection changes
  observe({
    req(input$demographic_select, input$task_select)
    
    if(input$task_select %in% names(data_list)) {
      dataset <- data_list[[input$task_select]]
      
      # Get all sub-options for the currently selected demographic
      if(input$demographic_select %in% names(dataset)) {
        available_values <- unique(dataset[[input$demographic_select]])
        available_values <- available_values[!is.na(available_values) & available_values != ""]
        
        new_selected <- c()
        for(val in available_values) {
          if(val %in% names(demographic_mappings[[input$demographic_select]])) {
            option_key <- paste0(input$demographic_select, "_", val)
            new_selected <- c(new_selected, option_key)
          }
        }
        
        # Update the picker selection
        updatePickerInput(session, "demographic_filter_multi",
                          selected = new_selected)
      }
    }
  })
  
  # Reactive data processing with caching and debouncing
  processed_data <- reactive({
    req(input$task_select, input$variable_select, input$demographic_select)
    
    # Add reactive dependency on the filter - force reactivity
    filter_input <- input$demographic_filter_multi
    if(is.null(filter_input)) filter_input <- character(0)
    
    if(input$task_select == "" || input$variable_select == "" || input$demographic_select == "" ||
       length(data_list) == 0 || !input$task_select %in% names(data_list)) {
      return(NULL)
    }
    
    # Create a cache key for this specific combination
    cache_key <- paste(input$task_select, input$variable_select, input$demographic_select, sep = "_")
    
    dataset <- data_list[[input$task_select]]
    if(is.null(dataset) || nrow(dataset) == 0) {
      return(NULL)
    }
    
    # Check if columns exist
    if(!input$variable_select %in% names(dataset) || !input$demographic_select %in% names(dataset)) {
      return(NULL)
    }
    
    # Filter out missing data
    filtered_data <- dataset[
      !is.na(dataset[[input$variable_select]]) & dataset[[input$variable_select]] != "" &
        !is.na(dataset[[input$demographic_select]]) & dataset[[input$demographic_select]] != "", 
    ]
    
    # Apply demographic filter if available
    if(!is.null(input$demographic_filter_multi) && length(input$demographic_filter_multi) > 0) {
      # Parse the selected options to extract demographic values
      selected_filters <- input$demographic_filter_multi
      
      if(length(selected_filters) > 0) {
        # Parse demographic_variable_value format
        allowed_values <- list()
        
        for(filter_item in selected_filters) {
          parts <- strsplit(filter_item, "_", fixed = TRUE)[[1]]
          if(length(parts) >= 2) {
            demo_var <- parts[1]
            demo_val <- paste(parts[-1], collapse = "_")  # In case value contains underscores
            
            if(!demo_var %in% names(allowed_values)) {
              allowed_values[[demo_var]] <- c()
            }
            allowed_values[[demo_var]] <- c(allowed_values[[demo_var]], demo_val)
          }
        }
        
        # Store original count for debugging
        original_count <- nrow(filtered_data)
        
        # Apply filters for each demographic variable
        for(demo_var in names(allowed_values)) {
          if(demo_var %in% names(filtered_data)) {
            # Apply filter for this demographic variable
            filtered_data <- filtered_data[filtered_data[[demo_var]] %in% allowed_values[[demo_var]], ]
          }
        }
        
        # Debug output (only in development)
        if(nrow(filtered_data) != original_count) {
          cat("Filter applied:", length(allowed_values), "demographics,", 
              original_count, "->", nrow(filtered_data), "rows\n")
        }
      }
    }
    
    if(nrow(filtered_data) == 0) {
      return(NULL)
    }
    
    # Process based on variable type with early returns for performance
    variable_type <- if(is_average_variable(input$variable_select)) "average" else 
      if(is_open_ended_variable(input$variable_select)) "open_ended" else "categorical"
    
    switch(variable_type,
           "average" = process_average_data(filtered_data, input$variable_select, input$demographic_select, input$task_select, input$demographic_filter_multi),
           "open_ended" = process_open_ended_data(filtered_data, input$variable_select, input$demographic_select),
           "categorical" = process_categorical_data(filtered_data, input$variable_select, input$demographic_select)
    )
  })
  process_categorical_data <- function(filtered_data, variable, demographic) {
    # Only process demographic values that are actually in the filtered data
    available_demo_values <- unique(filtered_data[[demographic]])
    available_demo_values <- available_demo_values[!is.na(available_demo_values)]
    
    all_var_values <- unique(filtered_data[[variable]])
    
    results <- data.frame()
    
    for(demo_val in available_demo_values) {
      demo_label <- if(demo_val %in% names(demographic_mappings[[demographic]])) {
        demographic_mappings[[demographic]][[demo_val]]
      } else {
        demo_val
      }
      demo_data <- filtered_data[filtered_data[[demographic]] == demo_val, ]
      
      for(var_val in all_var_values) {
        var_label <- map_variable_value(variable, var_val)
        count <- sum(demo_data[[variable]] == var_val, na.rm = TRUE)
        total_in_group <- nrow(demo_data)
        percentage <- if(total_in_group > 0) round((count / total_in_group) * 100, 2) else 0
        
        results <- rbind(results, data.frame(
          Demographic = demo_label,
          Variable = var_label,
          Count = count,
          Percentage = paste0(percentage, "%"),
          stringsAsFactors = FALSE
        ))
      }
    }
    
    return(results)
  }
  
  # Process average data
  process_average_data <- function(filtered_data, variable, demographic, task_key, demographic_filters = NULL) {
    # Only process demographic values that are actually in the filtered data for TASK calculations
    available_demo_values <- unique(filtered_data[[demographic]])
    available_demo_values <- available_demo_values[!is.na(available_demo_values)]
    
    # Get demographic values for OVERALL calculations 
    # Use filtered values when demographic filtering is applied, otherwise use all values
    all_demo_values <- available_demo_values  # Use the same filtered values as task calculations
    if(length(all_data_averages) > 0 && 
       variable %in% names(all_data_averages) &&
       demographic %in% names(all_data_averages[[variable]])) {
      # Only use the demographic values that are present in filtered data
      all_available_values <- names(all_data_averages[[variable]][[demographic]])
      all_demo_values <- all_available_values[all_available_values %in% available_demo_values]
    }
    
    results <- data.frame()
    raw_values <- list()
    
    # Calculate overall totals for the Overall Average row (TASK data - filtered)
    all_task_values <- as.numeric(filtered_data[[variable]])
    all_task_values <- all_task_values[!is.na(all_task_values)]
    overall_task_avg <- if(length(all_task_values) > 0) round(mean(all_task_values), 2) else NA
    overall_task_sd <- if(length(all_task_values) > 0) round(sd(all_task_values), 2) else NA
    overall_task_count <- length(all_task_values)
    
    # Overall all-data average and SD - calculate from consolidated data with same filtering
    overall_all_data_count <- 0
    overall_all_data_avg <- NA
    overall_all_data_sd <- NA
    
    # Calculate overall statistics from consolidated data with filtering
    if(!is.null(combined_data_cache) && variable %in% names(combined_data_cache) && demographic %in% names(combined_data_cache)) {
      # Start with all consolidated data
      overall_raw_data <- combined_data_cache %>%
        filter(!is.na(.data[[variable]]), .data[[variable]] != "",
               !is.na(.data[[demographic]]), .data[[demographic]] != "") %>%
        mutate(numeric_value = as.numeric(.data[[variable]])) %>%
        filter(!is.na(numeric_value))
      
      # Apply the same demographic filtering
      if(!is.null(demographic_filters) && length(demographic_filters) > 0) {
        selected_filters <- demographic_filters
        allowed_values <- list()
        
        for(filter_item in selected_filters) {
          parts <- strsplit(filter_item, "_", fixed = TRUE)[[1]]
          if(length(parts) >= 2) {
            demo_var <- parts[1]
            demo_val_filter <- paste(parts[-1], collapse = "_")
            
            if(!demo_var %in% names(allowed_values)) {
              allowed_values[[demo_var]] <- c()
            }
            allowed_values[[demo_var]] <- c(allowed_values[[demo_var]], demo_val_filter)
          }
        }
        
        # Apply filters to consolidated data
        for(demo_var in names(allowed_values)) {
          if(demo_var %in% names(overall_raw_data)) {
            overall_raw_data <- overall_raw_data %>%
              filter(.data[[demo_var]] %in% allowed_values[[demo_var]])
          }
        }
      }
      
      if(nrow(overall_raw_data) > 0) {
        overall_all_data_count <- nrow(overall_raw_data)
        overall_all_data_avg <- round(mean(overall_raw_data$numeric_value), 2)
        overall_all_data_sd <- if(overall_all_data_count > 1) round(sd(overall_raw_data$numeric_value), 2) else NA
      }
    }
    
    
    for(demo_val in available_demo_values) {
      demo_label <- if(demo_val %in% names(demographic_mappings[[demographic]])) {
        demographic_mappings[[demographic]][[demo_val]]
      } else {
        demo_val
      }
      demo_data <- filtered_data[filtered_data[[demographic]] == demo_val, ]
      
      # Task average and standard deviation
      numeric_values <- as.numeric(demo_data[[variable]])
      numeric_values <- numeric_values[!is.na(numeric_values)]
      task_avg <- if(length(numeric_values) > 0) round(mean(numeric_values), 2) else NA
      task_sd <- if(length(numeric_values) > 1) round(sd(numeric_values), 2) else NA
      task_count <- length(numeric_values)
      
      # All data average and SD - use pre-calculated when possible for performance
      all_data_avg <- NA
      all_data_sd <- NA
      all_data_count <- 0
      
      # Calculate all data statistics from consolidated data with same filtering as applied to task data
      if(!is.null(combined_data_cache) && variable %in% names(combined_data_cache) && demographic %in% names(combined_data_cache)) {
        # Start with all consolidated data for this demographic value
        demo_raw_data <- combined_data_cache %>%
          filter(.data[[demographic]] == demo_val,
                 !is.na(.data[[variable]]), .data[[variable]] != "") %>%
          mutate(numeric_value = as.numeric(.data[[variable]])) %>%
          filter(!is.na(numeric_value))
        
        # Apply the same demographic filtering that was applied to filtered_data
        # Get the demographic filters that were applied to create filtered_data
        if(!is.null(demographic_filters) && length(demographic_filters) > 0) {
          # Parse the selected demographic filters
          selected_filters <- demographic_filters
          allowed_values <- list()
          
          for(filter_item in selected_filters) {
            parts <- strsplit(filter_item, "_", fixed = TRUE)[[1]]
            if(length(parts) >= 2) {
              demo_var <- parts[1]
              demo_val_filter <- paste(parts[-1], collapse = "_")
              
              if(!demo_var %in% names(allowed_values)) {
                allowed_values[[demo_var]] <- c()
              }
              allowed_values[[demo_var]] <- c(allowed_values[[demo_var]], demo_val_filter)
            }
          }
          
          # Apply filters to consolidated data
          for(demo_var in names(allowed_values)) {
            if(demo_var %in% names(demo_raw_data)) {
              demo_raw_data <- demo_raw_data %>%
                filter(.data[[demo_var]] %in% allowed_values[[demo_var]])
            }
          }
        }
        
        if(nrow(demo_raw_data) > 0) {
          all_data_count <- nrow(demo_raw_data)
          all_data_avg <- round(mean(demo_raw_data$numeric_value), 2)
          all_data_sd <- if(all_data_count > 1) round(sd(demo_raw_data$numeric_value), 2) else NA
        }
      }
      
      # Format averages with SD in parentheses
      task_avg_display <- if(!is.na(task_avg) && !is.na(task_sd)) {
        paste0(task_avg, " (", task_sd, ")")
      } else if(!is.na(task_avg)) {
        as.character(task_avg)
      } else {
        NA
      }
      
      all_data_avg_display <- if(!is.na(all_data_avg) && !is.na(all_data_sd)) {
        paste0(all_data_avg, " (", all_data_sd, ")")
      } else if(!is.na(all_data_avg)) {
        as.character(all_data_avg)
      } else {
        NA
      }
      
      results <- rbind(results, data.frame(
        Demographic = demo_label,
        `Task N` = task_count,
        `Task Average (SD)` = task_avg_display,
        `Overall N` = all_data_count,
        `All Data Average (SD)` = all_data_avg_display,
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
      
      # Store raw values for plotting
      raw_values[[demo_label]] <- list(
        task_avg = task_avg,
        task_sd = task_sd,
        all_data_avg = all_data_avg,
        all_data_sd = all_data_sd
      )
    }
    
    # Add Overall Average row
    overall_task_avg_display <- if(!is.na(overall_task_avg) && !is.na(overall_task_sd)) {
      paste0(overall_task_avg, " (", overall_task_sd, ")")
    } else if(!is.na(overall_task_avg)) {
      as.character(overall_task_avg)
    } else {
      NA
    }
    
    overall_all_data_avg_display <- if(!is.na(overall_all_data_avg) && !is.na(overall_all_data_sd)) {
      paste0(overall_all_data_avg, " (", overall_all_data_sd, ")")
    } else if(!is.na(overall_all_data_avg)) {
      as.character(overall_all_data_avg)
    } else {
      NA
    }
    
    overall_row <- data.frame(
      Demographic = "Overall Average",
      `Task N` = overall_task_count,
      `Task Average (SD)` = overall_task_avg_display,
      `Overall N` = overall_all_data_count,
      `All Data Average (SD)` = overall_all_data_avg_display,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    results <- rbind(results, overall_row)
    
    # Store overall raw values for plotting
    raw_values[["Overall Average"]] <- list(
      task_avg = overall_task_avg,
      task_sd = overall_task_sd,
      all_data_avg = overall_all_data_avg,
      all_data_sd = overall_all_data_sd
    )
    
    # Attach raw values as attribute for use in plotting
    attr(results, "raw_values") <- raw_values
    return(results)
  }
  
  # Process open-ended data
  process_open_ended_data <- function(filtered_data, variable, demographic) {
    results <- data.frame()
    
    for(i in 1:nrow(filtered_data)) {
      demo_val <- filtered_data[[demographic]][i]
      demo_label <- map_demographic_value(demographic, demo_val)
      response <- filtered_data[[variable]][i]
      student_id <- if("Student_ID" %in% names(filtered_data)) filtered_data[["Student_ID"]][i] else NA
      
      if(!is.na(response) && response != "") {
        # Check if this is a DT variable with URLs
        if(endsWith(variable, "DT") && grepl("http", response, ignore.case = TRUE)) {
          # Extract URLs for DT variables
          url_pattern <- "https?://[^\\s<>\"'{}|\\^`\\[\\]]+"
          urls <- regmatches(response, gregexpr(url_pattern, response, perl = TRUE))[[1]]
          
          if(length(urls) > 0) {
            # For DT variables, create separate Link and Preview columns
            for(url in urls) {
              link_html <- paste0('<a href="', url, '" target="_blank" class="clickable-link">', url, '</a>')
              preview_html <- paste0('<button class="btn btn-sm btn-primary image-preview" data-url="', url, '" style="background: #4a90e2; border: none; color: white; padding: 4px 8px; border-radius: 3px; cursor: pointer; font-size: 12px;">🖼️ Preview</button>')
              
              results <- rbind(results, data.frame(
                Student_ID = if(!is.na(student_id)) student_id else "",
                Demographic = demo_label,
                `Link to Picture` = link_html,
                Preview = preview_html,
                stringsAsFactors = FALSE,
                check.names = FALSE
              ))
            }
          }
        } else {
          # For regular OE variables, just show the response
          results <- rbind(results, data.frame(
            Student_ID = if(!is.na(student_id)) student_id else "",
            Demographic = demo_label,
            Response = as.character(response),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    return(results)
  }
  
  # Process categorical data
  # Results table output
  output$results_table <- DT::renderDataTable({
    data <- processed_data()
    if(is.null(data)) {
      return(DT::datatable(data.frame(Message = "No data available with current selection")))
    }
    
    dt <- DT::datatable(data, 
                        selection = 'none',
                        options = list(
                          pageLength = 15,
                          dom = 'Blfrtip',
                          lengthMenu = list(c(15, 50, 100, -1), c('15', '50', '100', 'All')),
                          buttons = c('copy', 'csv', 'excel'),
                          columnDefs = list(list(className = 'dt-center', targets = '_all'))
                        ),
                        rownames = FALSE,
                        class = 'cell-border stripe hover', 
                        escape = FALSE  # Allow HTML content (for clickable links)
    )
    
    # Apply basic styling with larger font
    dt <- dt %>% DT::formatStyle(
      columns = 1:ncol(data),
      backgroundColor = 'white',
      borderColor = '#ddd',
      fontSize = '14px'
    )
    
    # Highlight Overall Average row if it exists
    if(is_average_variable(input$variable_select) && "Overall Average" %in% data$Demographic) {
      overall_row_index <- which(data$Demographic == "Overall Average")
      
      # Style the specific row
      dt <- dt %>% DT::formatStyle(
        "Demographic",
        target = "row",
        backgroundColor = DT::styleEqual("Overall Average", '#f0f8ff'),
        fontWeight = DT::styleEqual("Overall Average", 'bold')
      )
    }
    
    dt
  })
  
  # UI for chart controls (e.g., chart type selector)
  output$chart_controls_ui <- renderUI({
    req(input$variable_select)
    
    if(is_likert_variable(input$variable_select)) {
      # Chart type selector for Likert scale (F_L) variables
      div(style="display: flex; align-items: center; gap: 10px; font-size: 14px; justify-content: flex-end;",
          span("Select Chart Type:", style="white-space: nowrap; line-height: 1.5; display: flex; align-items: center;"),
          div(style="min-width: 180px;",
              selectInput("chart_type", NULL, 
                          choices = c("Current Task" = "likert", "Current vs Overall" = "comparison"), 
                          selected = "likert",
                          width = "100%")
          )
      )
    } else if(is_categorical_chartable(input$variable_select)) {
      # No chart selector needed for categorical variables - only bar chart available
      NULL
    } else if(is_average_variable(input$variable_select)) {
      # Chart type selector for average variables
      div(style="display: flex; align-items: center; gap: 10px; font-size: 14px; justify-content: flex-end;",
          span("Select Chart Type:", style="white-space: nowrap; line-height: 1.5; display: flex; align-items: center;"),
          div(style="min-width: 160px;",
              selectInput("chart_type", NULL, 
                          choices = c("Task Average" = "task", "All Data Average" = "all_data", "Both" = "both"), 
                          selected = "task",
                          width = "100%")
          )
      )
    } else {
      NULL
    }
  })
  
  # Chart output
  output$results_chart <- renderPlot({
    data <- processed_data()
    variable <- input$variable_select
    
    req(data, variable)
    
    if(is_average_variable(variable)) {
      req(input$chart_type)
      
      # Get raw values from data attributes
      raw_values <- attr(data, "raw_values")
      if(is.null(raw_values)) {
        return(ggplot() + annotate("text", x = 1, y = 1, size=6, label = "Chart data not available.") + theme_void())
      }
      
      # Create plotting data frame
      demo_names <- names(raw_values)
      plot_df <- data.frame()
      
      for(demo in demo_names) {
        vals <- raw_values[[demo]]
        if(input$chart_type %in% c("task", "both")) {
          # Get N count from the data table
          task_n <- data[data$Demographic == demo, "Task N"][1]
          plot_df <- rbind(plot_df, data.frame(
            Demographic = demo,
            Average = vals$task_avg,
            SD = vals$task_sd,
            N = task_n,
            Type = "Task Average",
            stringsAsFactors = FALSE
          ))
        }
        if(input$chart_type %in% c("all_data", "both")) {
          # Get N count from the data table
          overall_n <- data[data$Demographic == demo, "Overall N"][1]
          plot_df <- rbind(plot_df, data.frame(
            Demographic = demo,
            Average = vals$all_data_avg,
            SD = vals$all_data_sd,
            N = overall_n,
            Type = "All Data Average",
            stringsAsFactors = FALSE
          ))
        }
      }
      
      # Filter out rows with NA values
      plot_df <- plot_df[!is.na(plot_df$Average), ]
      
      if(nrow(plot_df) == 0) {
        return(ggplot() + annotate("text", x = 1, y = 1, size=6, label = "No data available for selected chart type.") + theme_void())
      }
      
      # Set factor levels to control order
      plot_df$Demographic <- factor(plot_df$Demographic, levels = demo_names)
      
      if(input$chart_type == "both") {
        # Side-by-side bars for both types
        p <- ggplot(plot_df, aes(x = Demographic, y = Average, fill = Type)) +
          geom_col(position = "dodge", alpha = 0.8) +
          geom_errorbar(aes(ymin = Average - SD, ymax = Average + SD), 
                        position = position_dodge(width = 0.9), width = 0.2, size = 0.5, color = "black") +
          geom_text(aes(label = round(Average, 2), y = 0.1 * max(Average, na.rm = TRUE)), 
                    position = position_dodge(width = 0.9), 
                    vjust = 0, size = 5, color = "black", fontface = "bold") +
          geom_text(aes(label = paste0("N = ", N), y = 0.05 * max(Average, na.rm = TRUE)), 
                    position = position_dodge(width = 0.9), 
                    vjust = 0, size = 3.5, color = "black") +
          labs(
            title = paste("Average", create_w_variable_name(variable)),
            subtitle = paste("by", input$demographic_select, "(with error bars showing ±1 SD)"),
            x = input$demographic_select,
            y = "Average",
            fill = "Data Type"
          ) +
          theme_minimal(base_size = 16) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                plot.title = element_text(face="bold"),
                legend.position = "bottom")
      } else {
        # Single type chart
        chart_title <- if(input$chart_type == "task") "Task Average" else "All Data Average"
        
        p <- ggplot(plot_df, aes(x = Demographic, y = Average, fill = Demographic)) +
          geom_col(show.legend = FALSE, alpha = 0.8) +
          geom_errorbar(aes(ymin = Average - SD, ymax = Average + SD), 
                        width = 0.2, size = 0.5, color = "black") +
          geom_text(aes(label = round(Average, 2), y = 0.1 * max(Average, na.rm = TRUE)), 
                    vjust = 0, size = 5, color = "black", fontface = "bold") +
          geom_text(aes(label = paste0("N = ", N), y = 0.05 * max(Average, na.rm = TRUE)), 
                    vjust = 0, size = 3.5, color = "black") +
          labs(
            title = paste(chart_title, "-", create_w_variable_name(variable)),
            subtitle = paste("by", input$demographic_select, "(with error bars showing ±1 SD)"),
            x = input$demographic_select,
            y = chart_title
          ) +
          theme_minimal(base_size = 16) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(face="bold"))
      }
      
      p
      
    } else if(is_likert_variable(variable)) {
      # Likert-style chart for F_L variables
      req(input$chart_type)
      
      if(input$chart_type == "likert") {
        # Create centered likert-style chart for current task only
        # Prepare data for likert visualization
        likert_data <- data %>%
          group_by(Demographic) %>%
          mutate(Total = sum(Count),
                 Percentage = round((Count / Total) * 100, 1)) %>%
          ungroup()
        
        # Define response order (Disagree on left, Agree on right)
        response_levels <- c("Disagree", "Somewhat disagree", "Somewhat agree", "Agree")
        likert_data$Variable <- factor(likert_data$Variable, levels = response_levels)
        
        # Create demographic labels with N counts
        demo_totals <- likert_data %>%
          group_by(Demographic) %>%
          summarise(N = first(Total), .groups = 'drop')
        
        likert_data <- likert_data %>%
          left_join(demo_totals, by = "Demographic") %>%
          mutate(Demo_Label = paste0(Demographic, " (N=", N, ")"))
        
        # Calculate positions for centered chart
        likert_data <- likert_data %>%
          arrange(Demo_Label, Variable) %>%
          group_by(Demo_Label) %>%
          mutate(
            # Negative responses (left side) - make negative
            neg_percent = ifelse(Variable %in% c("Disagree", "Somewhat disagree"), -Percentage, 0),
            # Positive responses (right side) - keep positive  
            pos_percent = ifelse(Variable %in% c("Somewhat agree", "Agree"), Percentage, 0),
            # Calculate cumulative positions from center
            neg_cumsum = cumsum(neg_percent),
            pos_cumsum = cumsum(pos_percent),
            # Final position for each bar
            plot_value = ifelse(Variable %in% c("Disagree", "Somewhat disagree"), 
                                neg_cumsum - neg_percent/2, 
                                pos_cumsum - pos_percent/2)
          ) %>%
          ungroup()
        
        # Create colors for responses
        likert_colors <- c("Disagree" = "#d8b365", "Somewhat disagree" = "#ebd9b2",
                           "Somewhat agree" = "#acd9d5", "Agree" = "#5ab4ac")
        
        # Create the centered chart
        p <- ggplot(likert_data, aes(x = Demo_Label, fill = Variable)) +
          geom_col(aes(y = neg_percent), alpha = 0.9) +
          geom_col(aes(y = pos_percent), alpha = 0.9) +
          geom_text(aes(y = plot_value, label = ifelse(abs(Percentage) > 5, paste0(Percentage, "%"), "")), 
                    size = 4.5, color = "black", fontface = "bold") +
          scale_fill_manual(values = likert_colors) +
          coord_flip() +
          geom_hline(yintercept = 0, color = "black", size = 0.5) +
          labs(
            title = paste("Distribution of", input$variable_select, "Responses"),
            subtitle = paste("by", input$demographic_select, "(Centered percentages)"),
            x = input$demographic_select,
            y = "Percentage",
            fill = "Response Level"
          ) +
          theme_minimal(base_size = 16) +
          theme(legend.position = "bottom", 
                plot.title = element_text(face="bold"),
                axis.text.y = element_text(size = 14)) +
          guides(fill = guide_legend(nrow = 2)) +
          scale_y_continuous(labels = function(x) paste0(abs(x), "%"))
        
        p
        
      } else if(input$chart_type == "comparison") {
        # Create comparison chart showing current task vs overall data
        # Only compute when this chart type is explicitly selected
        req(input$chart_type == "comparison")
        
        # Prepare current task data
        current_data <- data %>%
          group_by(Demographic) %>%
          mutate(Total = sum(Count),
                 Percentage = round((Count / Total) * 100, 1)) %>%
          ungroup() %>%
          mutate(Source = "Current Task")
        
        # Calculate overall distribution across all data (cached and optimized)
        if(!is.null(combined_data_cache)) {
          
          if(input$variable_select %in% names(combined_data_cache) && input$demographic_select %in% names(combined_data_cache)) {
            # Pre-filter to reduce data size before grouping operations
            filtered_cache <- combined_data_cache[
              !is.na(combined_data_cache[[input$variable_select]]) & 
                combined_data_cache[[input$variable_select]] != "" &
                !is.na(combined_data_cache[[input$demographic_select]]) & 
                combined_data_cache[[input$demographic_select]] != "",
              c(input$variable_select, input$demographic_select)
            ]
            
            if(nrow(filtered_cache) > 0) {
              overall_data <- filtered_cache %>%
                group_by(dem_group = .data[[input$demographic_select]], var_value = .data[[input$variable_select]]) %>%
                summarise(count = n(), .groups = 'drop') %>%
                group_by(dem_group) %>%
                mutate(total = sum(count),
                       percentage = round((count / total) * 100, 1)) %>%
                ungroup() %>%
                mutate(
                  Demographic = map_demographic_value(input$demographic_select, dem_group),
                  Variable = map_variable_value(input$variable_select, var_value),
                  Count = count,
                  Percentage = percentage,
                  Total = total,
                  Source = "Overall"
                ) %>%
                select(Demographic, Variable, Count, Percentage, Total, Source)
            } else {
              overall_data <- data.frame()
            }
            
            # Combine both datasets
            comparison_data <- bind_rows(current_data, overall_data)
            
            # Define response order and create labels
            response_levels <- c("Disagree", "Somewhat disagree", "Somewhat agree", "Agree")
            comparison_data$Variable <- factor(comparison_data$Variable, levels = response_levels)
            
            # Create labels with N counts
            comparison_data <- comparison_data %>%
              mutate(Demo_Source_Label = paste0(Demographic, " - ", Source, " (N=", Total, ")"))
            
            # Create colors for responses
            likert_colors <- c("Disagree" = "#d8b365", "Somewhat disagree" = "#ebd9b2",
                               "Somewhat agree" = "#acd9d5", "Agree" = "#5ab4ac")
            
            # Calculate positions for centered chart
            comparison_data <- comparison_data %>%
              arrange(Demo_Source_Label, Variable) %>%
              group_by(Demo_Source_Label) %>%
              mutate(
                # Negative responses (left side) - make negative
                neg_percent = ifelse(Variable %in% c("Disagree", "Somewhat disagree"), -Percentage, 0),
                # Positive responses (right side) - keep positive  
                pos_percent = ifelse(Variable %in% c("Somewhat agree", "Agree"), Percentage, 0),
                # Calculate cumulative positions from center
                neg_cumsum = cumsum(neg_percent),
                pos_cumsum = cumsum(pos_percent),
                # Final position for each bar
                plot_value = ifelse(Variable %in% c("Disagree", "Somewhat disagree"), 
                                    neg_cumsum - neg_percent/2, 
                                    pos_cumsum - pos_percent/2)
              ) %>%
              ungroup()
            
            # Create the centered comparison chart
            ggplot(comparison_data, aes(x = Demo_Source_Label, fill = Variable)) +
              geom_col(aes(y = neg_percent), alpha = 0.9) +
              geom_col(aes(y = pos_percent), alpha = 0.9) +
              geom_text(aes(y = plot_value, label = ifelse(abs(Percentage) > 5, paste0(Percentage, "%"), "")), 
                        size = 4.5, color = "black", fontface = "bold") +
              scale_fill_manual(values = likert_colors) +
              coord_flip() +
              geom_hline(yintercept = 0, color = "black", size = 0.5) +
              labs(
                title = paste("Comparison:", input$variable_select, "Responses"),
                subtitle = paste("Current Task vs Overall Data by", input$demographic_select, "(Centered percentages)"),
                x = paste(input$demographic_select, "Groups"),
                y = "Percentage", 
                fill = "Response Level"
              ) +
              theme_minimal(base_size = 16) +
              theme(legend.position = "bottom", 
                    plot.title = element_text(face="bold"),
                    axis.text.y = element_text(size = 14)) +
              guides(fill = guide_legend(nrow = 2)) +
              scale_y_continuous(labels = function(x) paste0(abs(x), "%"))
          } else {
            ggplot() + annotate("text", x = 1, y = 1, size=6, label = "Overall data not available for comparison.") + theme_void()
          }
        } else {
          ggplot() + annotate("text", x = 1, y = 1, size=6, label = "No overall data available for comparison.") + theme_void()
        }
      }
      
    } else if(is_categorical_chartable(variable)) {
      # Chart for other categorical data
      
      # Create N counts for each demographic group
      demo_totals <- data %>%
        group_by(Demographic) %>%
        summarise(N = sum(Count, na.rm = TRUE), .groups = 'drop') %>%
        mutate(Demo_Label = paste0(Demographic, " (N=", N, ")"))
      
      # Add N labels to the data
      chart_data <- data %>%
        left_join(demo_totals, by = "Demographic") %>%
        mutate(Demographic_Label = Demo_Label)
      
      # Create colors for _SC variables (flip so green=correct, red=incorrect)
      if(endsWith(variable, "_SC")) {
        sc_colors <- c("Incorrect" = "#e74c3c", "Correct" = "#27ae60")  # Red for incorrect, green for correct
        
        ggplot(chart_data, aes(x = Demographic_Label, y = Count, fill = Variable)) +
          geom_col(position = "dodge") +
          geom_text(aes(label = Count, y = Count + max(Count) * 0.02), 
                    position = position_dodge(width = 0.9), 
                    vjust = 0, size = 4, fontface = "bold") +
          scale_fill_manual(values = sc_colors) +
          labs(
            title = paste("Distribution of", input$variable_select),
            subtitle = paste("by", input$demographic_select),
            x = input$demographic_select,
            y = "Count",
            fill = "Response"
          ) +
          theme_minimal(base_size = 16) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                legend.position = "bottom", 
                plot.title = element_text(face="bold"))
      } else {
        # Default colors for other categorical variables
        ggplot(chart_data, aes(x = Demographic_Label, y = Count, fill = Variable)) +
          geom_col(position = "dodge") +
          geom_text(aes(label = Count, y = Count + max(Count) * 0.02), 
                    position = position_dodge(width = 0.9), 
                    vjust = 0, size = 4, fontface = "bold") +
          labs(
            title = paste("Distribution of", input$variable_select),
            subtitle = paste("by", input$demographic_select),
            x = input$demographic_select,
            y = "Count",
            fill = "Response"
          ) +
          theme_minimal(base_size = 16) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                legend.position = "bottom", 
                plot.title = element_text(face="bold"))
      }
    } else {
      # Message for non-chartable data (like open-ended)
      ggplot() + 
        annotate("text", x = 1, y = 1, size=6, label = "Chart not available for this variable type.") + 
        theme_void()
    }
  })
  
  # Summary output
  output$summary_output <- renderUI({
    req(input$task_select, input$variable_select, input$demographic_select)
    
    data <- processed_data()
    if(is.null(data)) {
      return(HTML("<div class='summary-box'><h4>Summary</h4><p>No data available for current selection</p></div>"))
    }
    
    task_name <- if(input$task_select %in% names(task_mapping)) {
      task_mapping[[input$task_select]] 
    } else { 
      input$task_select 
    }
    
    # Get display name for the variable
    variable_display_name <- create_w_variable_name(input$variable_select)
    
    total_responses <- if(is_average_variable(input$variable_select)) {
      # Exclude Overall Average row from count
      data_without_overall <- data[data$Demographic != "Overall Average", ]
      sum(data_without_overall$`Task N`, na.rm = TRUE)
    } else if(is_open_ended_variable(input$variable_select)) {
      nrow(data)
    } else {
      sum(data$Count, na.rm = TRUE)
    }
    
    unique_demographics <- if(is_average_variable(input$variable_select)) {
      length(unique(data$Demographic[data$Demographic != "Overall Average"]))
    } else {
      length(unique(data$Demographic))
    }
    
    variable_type <- if(is_average_variable(input$variable_select)) {
      "Average"
    } else if(is_open_ended_variable(input$variable_select)) {
      "Open-ended"
    } else {
      "Categorical"
    }
    
    HTML(paste0(
      "<div class='summary-box'>",
      "<h4>Analysis Summary</h4>",
      "<p><strong>Task:</strong> ", task_name, "</p>",
      "<p><strong>Variable:</strong> ", variable_display_name, " <span style='color: #666;'>(", variable_type, ")</span></p>",
      "<p><strong>Demographic:</strong> ", input$demographic_select, "</p>",
      "<p><strong>Total Responses:</strong> ", total_responses, "</p>",
      "<p><strong>Unique ", input$demographic_select, " categories:</strong> ", unique_demographics, "</p>",
      "</div>"
    ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)