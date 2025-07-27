# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an R Shiny dashboard for analyzing HSBA (High School Biology Assessment) data. The application is designed to be deployed as a static site using Shinylive on GitHub Pages, allowing it to run entirely in the browser without requiring an R server.

## Key Commands

### Local Development
```r
# Run the app locally
shiny::runApp("app.R")

# Or open in RStudio and click "Run App"
```

### Build and Deploy
```r
# Export the app for GitHub Pages deployment
source("export.R")

# Or manually:
library(shinylive)
shinylive::export(appdir = ".", destdir = "docs", template = "shinylive-template.html")

# Serve locally to test the exported version
httpuv::runStaticServer("docs")
```

## Data Architecture

### Data Structure
The application uses a **consolidated data approach** where all task data is stored in a single file:

- **Primary Data Source**: `data/all_tasks_long_by_task.csv` - Contains all tasks in long format
- **Legacy Data**: Individual task files (e.g., `planting_trees_clean.csv`) - Still present but not used
- **Key Columns**:
  - `Task_Name`: Identifies the task (e.g., "planting_trees")
  - `Task_Prefix`: Short code for task-specific variables (e.g., "PT")
  - Demographic variables: `Grade`, `Gender`, `Race`, `English`, `Test_Position`, `OSE_BSCS`
  - Weighted variables: `w_Connection`, `w_Preparedness`, `w_Interest`, `w_Relevance`, `w_Comprehension`
  - Task-specific variables: Prefixed with task code (e.g., `PT_Q1_T3_1`)
  - Common variables: `F_L*` (Likert responses), `F_OE*` (open-ended), `*_SC` (scored items)

### Data Processing Pipeline
1. **Loading**: `load_data_and_process()` reads consolidated CSV and creates task-specific datasets
2. **Filtering**: Data is filtered by `Task_Name` to create separate datasets per task
3. **Caching**: Pre-calculates averages and caches combined data for performance
4. **Variable Classification**: Automatically categorizes variables as average, categorical, or open-ended

## Application Architecture

### Core Components

#### Data Layer (`app.R` lines 13-109)
- `load_data_and_process()`: Main data loading function
- `combined_data_cache`: Performance optimization for cross-task analysis
- `all_data_averages`: Pre-calculated statistics for overall comparisons

#### Variable Classification System
- **Average Variables**: `w_*` variables (weighted factors)
- **Categorical Variables**: `F_L*`, `*_SC`, `*_MC`, `*_CH` 
- **Open-ended Variables**: `*_OE`, `*_DT` (including image URLs)

#### Processing Functions
- `process_average_data()`: Handles weighted variables with task vs. overall comparisons
- `process_categorical_data()`: Processes discrete choice responses
- `process_open_ended_data()`: Handles text responses and image previews

#### UI/UX Features
- **Three-tier selection**: Task → Variable → Demographic
- **Dynamic filtering**: Variables filtered by task prefix
- **Demographic filtering**: Multi-select filtering within selected demographic
- **Dual view modes**: Table view and Chart view
- **Chart types**: Bar charts, Likert scales, comparison charts

### Key Reactive Elements
- `processed_data()`: Main reactive that processes filtered data based on user selections
- Dynamic variable dropdown that shows only task-relevant variables
- Demographic filter that updates based on selected demographic category

## Data Mappings and Classifications

### Demographic Mappings
- Grade: 1-4 → 9th-12th grade
- Gender: 1-4 → Man/Boy, Woman/Girl, Not listed, Prefer not to say
- Race: 1-9 → Various ethnic categories
- English: 1-4 → Comfort levels with English

### Variable Types
- **Weighted Variables (`w_*`)**: Show averages with standard deviations, compare task vs. overall data
- **Likert Variables (`F_L*`)**: 4-point scale responses with centered chart visualization
- **Scored Variables (`*_SC`)**: Correct/Incorrect with color coding (green/red)
- **Open-ended (`*_OE`, `*_DT`)**: Text responses, DT variables may contain image URLs

### Chart Behavior
- **Average variables**: Bar charts with error bars, option to compare task vs. overall data
- **Likert variables**: Centered horizontal bar charts, comparison mode available
- **Categorical variables**: Standard bar charts with counts
- **Open-ended variables**: Table-only display with image preview for DT variables

## Critical Implementation Notes

### Task-Variable Relationship
Variables are filtered to show only those relevant to the selected task:
- Task-specific variables (prefixed with `Task_Prefix`)
- Common variables (`F_L*`, `F_OE*`, `w_*`)
- Excluded variables are defined in `excluded_columns`

### Demographic Filtering Impact
When demographic filters are applied, both task-specific and overall statistics are recalculated to reflect only the filtered population. This ensures that "Overall N" and "All Data Average" columns update appropriately.

### Performance Considerations
- Uses cached consolidated data (`combined_data_cache`) for cross-task analysis
- Pre-calculates averages at startup for faster rendering
- Applies demographic filtering consistently across task and overall calculations

## Shinylive Deployment

The app is designed for GitHub Pages deployment using Shinylive:
- `docs/` directory contains the exported static site
- `export.R` handles the build process and title customization
- No server-side R required in production
- All dependencies must be web-compatible R packages