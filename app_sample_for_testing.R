library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(munsell)

# --- Data Loading and Pre-processing ---

# Function to read all CSVs from the 'data' subfolder
load_data <- function() {
    data_path <- "data"
    csv_files <- list.files(data_path, pattern = "\\.csv$", full.names = TRUE)
    if (length(csv_files) == 0) {
        # Create a dummy data frame if no CSVs are found to prevent app from crashing
        return(list(empty_data = data.frame(Message = "No CSV files found in the 'data' folder.")))
    }
    # 
    data_list <- lapply(csv_files, read.csv, stringsAsFactors = FALSE, colClasses = "character")
    
    # Use a clean key for the list names (e.g., 'task_a_clean')
    file_keys <- gsub("\\.csv$", "", basename(csv_files))
    names(data_list) <- file_keys
    data_list
}

# Function to create readable task names from filenames
create_task_name <- function(filename) {
    clean_name <- gsub("\\.csv$|_clean$", "", filename)
    clean_name <- gsub("_", " ", clean_name)
    tools::toTitleCase(clean_name)
}

# Load the data when the app starts
all_data <- load_data()
task_mapping <- setNames(names(all_data), sapply(names(all_data), create_task_name))

# Define columns to exclude from variable dropdown and demographics
excluded_columns <- c(
    "Student_ID", "TID", "Test_Label", "Task_Position", "Start", "End", "Time_Spent"
)
demographic_columns <- c("Grade", "Gender", "Race", "English")

# Mappings from your JS code
demographic_mappings <- list(
    "Grade" = c("1" = "9th", "2" = "10th", "3" = "11th", "4" = "12th"),
    "Gender" = c("1" = "Man/Boy", "2" = "Woman/Girl", "3" = "Not listed", "4" = "Prefer not to say"),
    "Race" = c("1" = "American Indian/Alaskan Native", "2" = "African American/Black", "3" = "Asian American/Asian", "4" = "Hispanic/Latinx", "5" = "Hawaiian/Pacific Islander", "6" = "White/European", "7" = "Multiracial/multiethnic", "8" = "Not listed", "9" = "Prefer not to say"),
    "English" = c("1" = "English primary", "2" = "Difficulty with English", "3" = "Somewhat Comfortable", "4" = "Very Comfortable")
)
sc_item_mappings <- c("0" = "Incorrect", "1" = "Correct")

# --- UI Definition ---
ui <- fluidPage(
    titlePanel("Interactive Data Dashboard"),
    sidebarLayout(
        sidebarPanel(
            # Use the human-readable names for the choices
            selectInput("fileSelect", "Select Task:", choices = names(task_mapping)),
            selectInput("variableSelect", "Select Variable:", choices = NULL),
            selectInput("demographicSelect", "Select Demographic:", choices = demographic_columns),
            hr(),
            helpText("This dashboard analyzes task data across different demographic groups.")
        ),
        mainPanel(
            tabsetPanel(
                id = "main_tabs",
                tabPanel("Table View", DTOutput("tableOutput")),
                tabPanel("Chart View", 
                         plotOutput("chartOutput"),
                         hr(),
                         # Simple chart type selector for bar/pie
                         selectInput("chartType", "Chart Type:", choices = c("Bar Chart" = "bar", "Pie Chart" = "pie"), selected = "bar")
                )
            )
        )
    )
)

# --- Server Logic ---
server <- function(input, output, session) {
    
    # Dynamically update variable choices based on selected file
    observeEvent(input$fileSelect, {
        req(input$fileSelect)
        
        # Get the actual file key from the human-readable name
        file_key <- task_mapping[input$fileSelect]
        
        df <- all_data[[file_key]]
        
        # Get valid column names for the variable dropdown
        available_vars <- setdiff(names(df), c(excluded_columns, demographic_columns))
        
        updateSelectInput(session, "variableSelect", choices = available_vars)
    })
    
    # Reactive expression to process data based on user selections
    processed_data <- reactive({
        req(input$fileSelect, input$variableSelect, input$demographicSelect)
        
        # Get the file key from the readable name
        file_key <- task_mapping[input$fileSelect]
        
        df <- all_data[[file_key]]
        
        # Ensure selected columns exist in the dataframe
        if (!all(c(input$variableSelect, input$demographicSelect) %in% names(df))) {
            return(NULL)
        }
        
        # Filter out rows with missing values in the selected columns
        df_filtered <- df %>%
            filter(!is.na(.data[[input$variableSelect]]) & .data[[input$variableSelect]] != "",
                   !is.na(.data[[input$demographicSelect]]) & .data[[input$demographicSelect]] != "")
        
        # Map demographic codes to labels
        if (input$demographicSelect %in% names(demographic_mappings)) {
            mapping_vec <- demographic_mappings[[input$demographicSelect]]
            df_filtered[[input$demographicSelect]] <- sapply(df_filtered[[input$demographicSelect]], function(x) ifelse(x %in% names(mapping_vec), mapping_vec[x], x))
        }
        
        # Handle numeric (average) vs. categorical variables
        if (startsWith(input$variableSelect, "w_")) {
            # For numeric 'w_' variables, calculate the average
            df_filtered %>%
                mutate(VariableValue = as.numeric(.data[[input$variableSelect]])) %>%
                group_by(Group = .data[[input$demographicSelect]]) %>%
                summarise(
                    N = n(),
                    Average = round(mean(VariableValue, na.rm = TRUE), 2)
                ) %>%
                rename(!!input$demographicSelect := Group)
            
        } else {
            # For categorical variables, create a cross-tabulation (counts)
            
            # Apply mappings if it's a score column
            if (endsWith(input$variableSelect, "_SC")) {
                df_filtered[[input$variableSelect]] <- sapply(df_filtered[[input$variableSelect]], function(x) ifelse(x %in% names(sc_item_mappings), sc_item_mappings[x], x))
            }
            
            df_filtered %>%
                count(Group = .data[[input$demographicSelect]], Variable = .data[[input$variableSelect]]) %>%
                rename(!!input$demographicSelect := Group, !!input$variableSelect := Variable)
        }
    })
    
    # Render the data table
    output$tableOutput <- renderDT({
        datatable(processed_data(), options = list(pageLength = 10), rownames = FALSE)
    })
    
    # Render the plot
    output$chartOutput <- renderPlot({
        df_plot <- processed_data()
        req(df_plot)
        
        # Handle plotting for numeric (average) vs. categorical variables
        if ("Average" %in% names(df_plot)) { # Plot for numeric 'w_' variables
            ggplot(df_plot, aes(x = .data[[input$demographicSelect]], y = Average, fill = .data[[input$demographicSelect]])) +
                geom_col() +
                geom_text(aes(label = Average), vjust = -0.5) +
                labs(title = paste("Average", input$variableSelect, "by", input$demographicSelect),
                     x = input$demographicSelect, y = "Average Value") +
                theme_minimal() +
                theme(legend.position = "none")
            
        } else { # Plot for categorical variables
            
            if(input$chartType == "bar") {
                ggplot(df_plot, aes(x = .data[[input$demographicSelect]], y = n, fill = .data[[input$variableSelect]])) +
                    geom_col(position = "dodge") +
                    labs(title = paste("Distribution of", input$variableSelect, "by", input$demographicSelect),
                         x = input$demographicSelect, y = "Count", fill = input$variableSelect) +
                    theme_minimal()
            } else { # Pie Chart
                # Pie chart shows proportions of the demographic groups
                df_pie <- df_plot %>% group_by(.data[[input$demographicSelect]]) %>% summarise(Total = sum(n))
                
                ggplot(df_pie, aes(x = "", y = Total, fill = .data[[input$demographicSelect]])) +
                    geom_bar(stat = "identity", width = 1) +
                    coord_polar("y", start = 0) +
                    theme_void() +
                    labs(fill = input$demographicSelect, title = paste("Proportion of Responses by", input$demographicSelect))
            }
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)