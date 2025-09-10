# Install required packages if not already installed
required_packages <- c("shiny", "shinydashboard", "ggplot2", "plotly", "dplyr",
                       "tidyr", "RColorBrewer", "viridis", "networkD3", "treemap",
                       "gganimate", "gifski", "scales", "shinyBS")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(viridis)
library(networkD3) # Assuming this was intended for some older plot, kept for completeness
library(treemap)
library(gganimate)
library(gifski)
library(scales)
library(shinyBS) # For tooltips

# Sample data generation (replace this with your actual data loading)
set.seed(123)
data <- data.frame(
  Age = sample(18:65, 1470, replace = TRUE),
  Attrition = sample(c("Yes", "No"), 1470, replace = TRUE, prob = c(0.16, 0.84)),
  Department = sample(c("Sales", "Research & Development", "Human Resources"), 1470, replace = TRUE),
  EducationField = sample(c("Life Sciences", "Medical", "Marketing", "Technical Degree", "Other"), 1470, replace = TRUE),
  JobRole = sample(c("Sales Executive", "Research Scientist", "Laboratory Technician",
                     "Manufacturing Director", "Healthcare Representative", "Manager",
                     "Sales Representative", "Research Director", "Human Resources"), 1470, replace = TRUE),
  MaritalStatus = sample(c("Single", "Married", "Divorced"), 1470, replace = TRUE),
  MonthlyIncome = round(runif(1470, 1000, 20000)),
  YearsAtCompany = sample(0:40, 1470, replace = TRUE),
  JobSatisfaction = sample(1:4, replace = TRUE, size = 1470),
  EnvironmentSatisfaction = sample(1:4, replace = TRUE, size = 1470),
  WorkLifeBalance = sample(1:4, replace = TRUE, size = 1470),
  DistanceFromHome = sample(1:30, 1470, replace = TRUE)
)

# Add a 'Year' column for animated plots, or ensure it exists if you have real time-series data
data <- data %>%
  mutate(Year = sample(2010:2020, nrow(data), replace = TRUE)) # Added Year for animated plots

# Data preprocessing
data <- data %>%
  mutate(
    Attrition = factor(Attrition, levels = c("Yes", "No")),
    Department = factor(Department),
    EducationField = factor(EducationField),
    JobRole = factor(JobRole), # Factorize JobRole
    MaritalStatus = factor(MaritalStatus),
    AgeGroup = cut(Age, breaks = c(18, 25, 35, 45, 55, 65),
                   labels = c("18-25", "26-35", "36-45", "46-55", "56-65"), right = FALSE),
    IncomeGroup = cut(MonthlyIncome, breaks = c(0, 3000, 6000, 9000, 12000, 15000, 20000),
                      labels = c("0-3k", "3-6k", "6-9k", "9-12k", "12-15k", "15k+"), right = FALSE)
  )

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "IBM HR Analytics Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Overall Summary", tabName = "summary", icon = icon("info-circle")), # New Summary Tab
      menuItem("Animated Trends", tabName = "animated", icon = icon("chart-line")),
      menuItem("Education Field Details", tabName = "edu_details", icon = icon("graduation-cap")),
      selectInput("color_palette", "Color Palette (Graphs):",
                  choices = c("Corporate Blue & Grey" = "blues",
                              "Vibrant Gradients" = "viridis",
                              "Traffic Light System" = "RdYlGn",
                              "Pastel Colors" = "Pastel1",
                              "Dark Theme" = "Dark2",
                              "Rainbow" = "Spectral"), # Added 'Spectral' for rainbow-like
                  selected = "blues"),
      
      # NEW: Background Color Selector
      selectInput("background_color", "Background Theme:",
                  choices = c("Default Light Theme" = "#f0f0f0", # Very light grey
                              "Modern Blue" = "#e8f0f8", # Light blue
                              "Warm Grey" = "#D8D3CD", # Light warm grey
                              "Subtle Dark Grey" = "#36454F", # Dark charcoal
                              "Deep Indigo" = "#1a2d42", # Dark blue
                              "Gradient Sunset" = "gradient_sunset",
                              "Gradient Ocean" = "gradient_ocean"),
                  selected = "#f0f0f0"),
      
      # Existing selectInput for EducationField (used for both image and table filtering)
      selectInput("edu_field_selector", "Select Education Field:",
                  choices = unique(data$EducationField),
                  selected = "Life Sciences"),
      
      # Clickable graph categories
      h4("Click a category to view related graphs:"),
      actionLink("attrition_link", "Attrition Analysis"),
      br(),
      actionLink("income_link", "Income Analysis"),
      br(),
      actionLink("satisfaction_link", "Satisfaction Analysis"),
      br(),
      actionLink("demographic_link", "Demographic Analysis"),
      br(),
      actionLink("department_link", "Department Analysis"),
      div(style = "padding: 10px;",
          actionButton("clear_filters", "Clear All Filters", icon = icon("broom"))
      )
    )
  ),
  
  dashboardBody(
    # Add shinyBS dependencies
    tags$head(
      singleton(tags$script(src = "https://cdn.jsdelivr.net/npm/shinyBS@0.6.1/js/shinyBS.js"))
    ),
    # NEW: Dynamic CSS for background color
    uiOutput("dynamic_background_css"),
    
    tabItems(
      # New Overall Summary Tab
      tabItem(tabName = "summary",
              fluidRow(
                box(width = 12,
                    h1(strong("Submitted by Zainab Mufti")),
                    h2(strong("Final Project: IBM HR Analytics Data Visualization")),
                    hr(),
                    h3("Overall Summary of Attributes and Dashboard"),
                    p("This interactive dashboard provides a comprehensive analysis of the IBM HR Analytics dataset, focusing on key factors influencing employee attrition and various aspects of the workforce. The dataset includes a rich set of attributes such as Age, Attrition, Department, Education Field, Job Role, Marital Status, Monthly Income, Years at Company, Job Satisfaction, Environment Satisfaction, Work-Life Balance, and Distance From Home."),
                    p("The dashboard is structured into several sections, each offering distinct insights:"),
                    tags$ul(
                      tags$li(strong("Dashboard (Main Analysis):"), " This dynamic section allows users to explore different facets of the HR data (Attrition, Income, Satisfaction, Demographics, Department) through interactive graphs. Filters applied through plot clicks enable deep dives into specific segments of the data."),
                      tags$li(strong("Overall Summary:"), " (You are here!) Provides a high-level overview of the data attributes and the dashboard's capabilities."),
                      tags$li(strong("Animated Trends:"), " Visualizes changes over time using animated plots (GIFs), showing dynamic shifts in department performance, income distribution, and employee counts."),
                      tags$li(strong("Education Field Details:"), " Offers a specialized view into specific education fields, combining illustrative images with detailed employee data tables."),
                      tags$li(strong("Interactivity:"), " The dashboard is highly interactive, allowing users to select color palettes, filter data by clicking on specific plot elements, and navigate between different analysis categories. Tooltips provide additional context and guidance for exploration."),
                      tags$li(strong("Key Attributes Overview:")),
                      tags$ul(
                        tags$li(strong("Attrition:"), " The core target variable, indicating whether an employee left the company. Crucial for retention analysis."),
                        tags$li(strong("Demographics (Age, Marital Status):"), " Provides insights into the age profile and marital status distribution of employees."),
                        tags$li(strong("Employment Details (Department, Job Role, Years at Company):"), " Essential for understanding organizational structure, career progression, and tenure."),
                        tags$li(strong("Compensation (Monthly Income):"), " A direct measure of employee earnings, vital for income distribution analysis."),
                        tags$li(strong("Satisfaction Metrics (Job Satisfaction, Environment Satisfaction, Work-Life Balance):"), " Subjective measures reflecting employee sentiment and well-being, often linked to retention."),
                        tags$li(strong("Distance From Home:"), " A unique geographic factor that might influence employee convenience and attrition."),
                        tags$li(strong("Education Field:"), " Categorizes the academic background, offering insights into the skillsets within the organization.")
                      )
                    ),
                    p("This dashboard serves as a powerful tool for HR professionals and analysts to identify trends, pinpoint problem areas, and make data-driven decisions to improve employee satisfaction, retention, and overall workforce management."),
                    hr(),
                    h3("Conclusion"),
                    p("This project, 'IBM HR Analytics Data Visualization,' successfully demonstrates the power of R Shiny in transforming raw HR data into actionable insights. By leveraging interactive dashboards, dynamic plots, and multi-dimensional analyses, it enables users to explore complex relationships within the data, identify key drivers of attrition, and understand employee behavior patterns. This dashboard stands as a comprehensive tool designed to empower HR professionals in making informed, strategic decisions to foster a more engaged and retained workforce. It's truly a culmination of data visualization techniques applied to a critical business challenge.")
                )
              )
      ),
      
      # Dashboard tab (modified to include new outputs and KPIs)
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_employees_kpi", width = 3),
                valueBoxOutput("attrition_rate_kpi", width = 3),
                valueBoxOutput("avg_monthly_income_kpi", width = 3),
                valueBoxOutput("avg_years_at_company_kpi", width = 3)
              ),
              uiOutput("graph_grid")
      ),
      
      # Animated Trends tab (UPDATED - kept as is per instructions)
      tabItem(tabName = "animated",
              fluidRow(
                box(title = div(id = "animated_dept_performance_scatter_title", "Animated Department Performance Trajectories"), width = 6, imageOutput("animated_dept_performance_scatter")),
                bsTooltip(id = "animated_dept_performance_scatter_title", title = "Tracks the movement of departments through a 2D space of average income and job satisfaction over years, showing their performance trajectories.", placement = "right", trigger = "hover"),
                box(title = div(id = "animated_dept_income_boxplot_title", "Animated Monthly Income Distribution by Department"), width = 6, imageOutput("animated_dept_income_boxplot")),
                bsTooltip(id = "animated_dept_income_boxplot_title", title = "Displays the evolving distribution (median, quartiles, range) of monthly income within each department year by year.", placement = "left", trigger = "hover")
              ),
              fluidRow(
                box(title = div(id = "animated_dept_bar_race_title", "Animated Departmental Employee Count Race"), width = 6, imageOutput("animated_dept_bar_race")),
                bsTooltip(id = "animated_dept_bar_race_title", title = "Shows the dynamic ranking of departments by their employee count over time, with bars growing and reordering.", placement = "right", trigger = "hover"),
                box(title = div(id = "animated_dept_demographics_scatter_title", "Animated Age vs. Years at Company by Department"), width = 6, imageOutput("animated_dept_demographics_scatter")),
                bsTooltip(id = "animated_dept_demographics_scatter_title", title = "Visualizes how the age and tenure profile of employees within different departments changes across the years.", placement = "left", trigger = "hover")
              )
      ),
      
      # New Tab Item for Education Field Details (Renamed)
      tabItem(tabName = "edu_details",
              h2("Education Field Visuals and Employee Data"),
              fluidRow(
                box(title = "Image for Selected Education Field", width = 12,
                    uiOutput("education_field_image")) # Output for the dynamic image
              ),
              fluidRow(
                box(title = "Employee Data by Selected Education Field", width = 12,
                    dataTableOutput("education_field_data_table")) # New output for the table
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Define the image URL mapping for EducationField
  education_field_images <- c(
    "Life Sciences" = "https://www.wirz-partners.ch/hubfs/ref._66421080p-ezgif.com-video-to-gif-converter.gif",
    "Medical" = "https://i.pinimg.com/originals/71/ff/80/71ff803fbe504006b29e71bbe69779f6.gif",
    "Marketing" = "https://thegrowthhustlers.com/wp-content/uploads/2021/12/growth-marketing-gif.gif",
    "Technical Degree" = "https://miro.medium.com/v2/resize:fit:1280/1*fS-yC5h43SENhha32uRpBQ.gif",
    "Other" = "https://cdn.dribbble.com/users/156826/screenshots/2568603/media/4c023d8c1143c7b579540b6151770e06.gif?resize=400x300&vertical=center" # Added an image for 'Other'
  )
  
  # Reactive color palette for graphs
  selected_palette <- reactive({
    switch(input$color_palette,
           "blues" = brewer.pal(9, "Blues")[3:9],
           "viridis" = viridis(7),
           "RdYlGn" = brewer.pal(9, "RdYlGn")[2:8],
           "Pastel1" = brewer.pal(9, "Pastel1")[1:7],
           "Dark2" = brewer.pal(8, "Dark2")[1:7],
           "Spectral" = brewer.pal(11, "Spectral")[1:7]
    )
  })
  
  # NEW: Reactive CSS for background color and panel adjustments
  output$dynamic_background_css <- renderUI({
    bg_choice <- input$background_color
    css_style <- ""
    
    if (bg_choice == "gradient_sunset") {
      css_style <- "
        body {
          background: linear-gradient(to right, #FF7E5F, #FEB47B); /* Orange to Red */
          color: #f8f9fa; /* Light text for dark background */
        }
        .content-wrapper {
          background-color: transparent !important; /* Make wrapper transparent */
        }
        .box, .value-box {
          background-color: rgba(255, 255, 255, 0.15) !important; /* Slightly transparent boxes */
          color: #f8f9fa;
          border: 1px solid rgba(255, 255, 255, 0.2);
          box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
        }
        .box-header .box-title, .small-box h3, .small-box p {
          color: #f8f9fa !important;
        }
        .small-box .icon-large {
          color: rgba(255, 255, 255, 0.4) !important;
        }
        .dataTables_wrapper .dataTables_length,
        .dataTables_wrapper .dataTables_filter,
        .dataTables_wrapper .dataTables_info,
        .dataTables_wrapper .dataTables_paginate .paginate_button {
          color: #f8f9fa !important;
        }
        .dataTables_wrapper .dataTables_filter input {
          background-color: rgba(255, 255, 255, 0.1);
          border: 1px solid rgba(255, 255, 255, 0.3);
          color: #f8f9fa;
        }
        table.dataTable thead th, table.dataTable tbody td {
          color: #f8f9fa;
        }
        table.dataTable.stripe tbody tr.odd {
          background-color: rgba(255, 255, 255, 0.05);
        }
        table.dataTable.hover tbody tr:hover {
          background-color: rgba(255, 255, 255, 0.2);
        }
        /* Adjust sidebar text color for better contrast if needed, default is dark */
        .sidebar-menu li a {
            color: #f8f9fa !important;
        }
        .sidebar-menu li.active > a {
            background-color: rgba(255, 255, 255, 0.2) !important;
        }
      "
    } else if (bg_choice == "gradient_ocean") {
      css_style <- "
        body {
          background: linear-gradient(to right, #002B4B, #005691); /* Deep to Light Blue */
          color: #f8f9fa; /* Light text */
        }
        .content-wrapper {
          background-color: transparent !important;
        }
        .box, .value-box {
          background-color: rgba(255, 255, 255, 0.15) !important;
          color: #f8f9fa;
          border: 1px solid rgba(255, 255, 255, 0.2);
          box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
        }
        .box-header .box-title, .small-box h3, .small-box p {
          color: #f8f9fa !important;
        }
        .small-box .icon-large {
          color: rgba(255, 255, 255, 0.4) !important;
        }
        .dataTables_wrapper .dataTables_length,
        .dataTables_wrapper .dataTables_filter,
        .dataTables_wrapper .dataTables_info,
        .dataTables_wrapper .dataTables_paginate .paginate_button {
          color: #f8f9fa !important;
        }
        .dataTables_wrapper .dataTables_filter input {
          background-color: rgba(255, 255, 255, 0.1);
          border: 1px solid rgba(255, 255, 255, 0.3);
          color: #f8f9fa;
        }
        table.dataTable thead th, table.dataTable tbody td {
          color: #f8f9fa;
        }
        table.dataTable.stripe tbody tr.odd {
          background-color: rgba(255, 255, 255, 0.05);
        }
        table.dataTable.hover tbody tr:hover {
          background-color: rgba(255, 255, 255, 0.2);
        }
        .sidebar-menu li a {
            color: #f8f9fa !important;
        }
        .sidebar-menu li.active > a {
            background-color: rgba(255, 255, 255, 0.2) !important;
        }
      "
    } else if (bg_choice %in% c("#36454F", "#1a2d42")) { # Dark solid colors
      css_style <- paste0("
        body {
          background-color: ", bg_choice, ";
          color: #f8f9fa; /* Light text for dark background */
        }
        .content-wrapper {
          background-color: transparent !important;
        }
        .box, .value-box {
          background-color: rgba(255, 255, 255, 0.1) !important; /* Slightly transparent boxes */
          color: #f8f9fa;
          border: 1px solid rgba(255, 255, 255, 0.2);
          box-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);
        }
        .box-header .box-title, .small-box h3, .small-box p {
          color: #f8f9fa !important;
        }
        .small-box .icon-large {
          color: rgba(255, 255, 255, 0.3) !important;
        }
        .dataTables_wrapper .dataTables_length,
        .dataTables_wrapper .dataTables_filter,
        .dataTables_wrapper .dataTables_info,
        .dataTables_wrapper .dataTables_paginate .paginate_button {
          color: #f8f9fa !important;
        }
        .dataTables_wrapper .dataTables_filter input {
          background-color: rgba(255, 255, 255, 0.1);
          border: 1px solid rgba(255, 255, 255, 0.3);
          color: #f8f9fa;
        }
        table.dataTable thead th, table.dataTable tbody td {
          color: #f8f9fa;
        }
        table.dataTable.stripe tbody tr.odd {
          background-color: rgba(255, 255, 255, 0.05);
        }
        table.dataTable.hover tbody tr:hover {
          background-color: rgba(255, 255, 255, 0.2);
        }
        .sidebar-menu li a {
            color: #f8f9fa !important;
        }
        .sidebar-menu li.active > a {
            background-color: rgba(255, 255, 255, 0.2) !important;
        }
      ")
    } else { # Light solid colors (e.g., #f0f0f0, #e8f0f8, #D8D3CD)
      css_style <- paste0("
        body {
          background-color: ", bg_choice, ";
          color: #333; /* Default text color */
        }
        .content-wrapper {
          background-color: transparent !important; /* Keep it transparent to show body bg */
        }
        .box, .value-box {
          background-color: #ffffff !important; /* Default white */
          color: #333;
          border: none;
          box-shadow: 0 1px 1px rgba(0,0,0,.1); /* Standard box shadow */
        }
        .box-header .box-title, .small-box h3, .small-box p {
          color: #333 !important;
        }
        .small-box .icon-large {
          color: inherit !important; /* Reset icon color */
        }
        .dataTables_wrapper .dataTables_length,
        .dataTables_wrapper .dataTables_filter,
        .dataTables_wrapper .dataTables_info,
        .dataTables_wrapper .dataTables_paginate .paginate_button {
          color: #333 !important;
        }
        .dataTables_wrapper .dataTables_filter input {
          background-color: #fff;
          border: 1px solid #ddd;
          color: #333;
        }
        table.dataTable thead th, table.dataTable tbody td {
          color: #333;
        }
        table.dataTable.stripe tbody tr.odd {
          background-color: #f9f9f9;
        }
        table.dataTable.hover tbody tr:hover {
          background-color: #f0f0f0;
        }
        .sidebar-menu li a {
            color: #b8c7ce !important; /* Default sidebar menu item color */
        }
        .sidebar-menu li.active > a {
            background-color: #2e353c !important; /* Default active sidebar menu item background */
        }
      ")
    }
    
    tags$style(HTML(css_style))
  })
  
  # Reactive value to track which category is selected
  selected_category <- reactiveVal("attrition")
  
  # Reactive values for storing clicked data points for cross-filtering
  # Initialize with NULL to indicate no filter is active
  rv <- reactiveValues(
    filtered_attrition = NULL,
    filtered_department = NULL,
    filtered_age_group = NULL,
    filtered_marital_status = NULL,
    filtered_education_field = NULL,
    filtered_job_satisfaction = NULL,
    filtered_job_role = NULL
  )
  
  # Function to reset all filters
  reset_all_filters <- function() {
    rv$filtered_attrition <- NULL
    rv$filtered_department <- NULL
    rv$filtered_age_group <- NULL
    rv$filtered_marital_status <- NULL
    rv$filtered_education_field <- NULL
    rv$filtered_job_satisfaction <- NULL
    rv$filtered_job_role <- NULL
  }
  
  # Observe clicks on the sidebar links (reset filters when changing category)
  observeEvent(input$attrition_link, {
    selected_category("attrition")
    reset_all_filters()
  })
  observeEvent(input$income_link, {
    selected_category("income")
    reset_all_filters()
  })
  observeEvent(input$satisfaction_link, {
    selected_category("satisfaction")
    reset_all_filters()
  })
  observeEvent(input$demographic_link, {
    selected_category("demographic")
    reset_all_filters()
  })
  observeEvent(input$department_link, {
    selected_category("department")
    reset_all_filters()
  })
  
  # Observe click on "Clear All Filters" button
  observeEvent(input$clear_filters, {
    reset_all_filters()
  })
  
  # Reactive expression for FILTERED DATA
  filtered_data <- reactive({
    current_data <- data
    
    if (!is.null(rv$filtered_attrition)) {
      current_data <- current_data %>%
        filter(Attrition == rv$filtered_attrition)
    }
    if (!is.null(rv$filtered_department)) {
      current_data <- current_data %>%
        filter(Department == rv$filtered_department)
    }
    if (!is.null(rv$filtered_age_group)) {
      current_data <- current_data %>%
        filter(AgeGroup == rv$filtered_age_group)
    }
    if (!is.null(rv$filtered_marital_status)) {
      current_data <- current_data %>%
        filter(MaritalStatus == rv$filtered_marital_status)
    }
    if (!is.null(rv$filtered_education_field)) {
      current_data <- current_data %>%
        filter(EducationField == rv$filtered_education_field)
    }
    if (!is.null(rv$filtered_job_satisfaction)) {
      current_data <- current_data %>%
        filter(JobSatisfaction == rv$filtered_job_satisfaction)
    }
    if (!is.null(rv$filtered_job_role)) { # ADDED FILTER FOR JOB ROLE
      current_data <- current_data %>%
        filter(JobRole == rv$filtered_job_role)
    }
    
    current_data
  })
  
  # --- KPIs for Dashboard ---
  output$total_employees_kpi <- renderValueBox({
    num_employees <- nrow(filtered_data())
    valueBox(
      value = formatC(num_employees, format = "d", big.mark = ","),
      subtitle = "Total Employees",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$attrition_rate_kpi <- renderValueBox({
    total_employees <- nrow(filtered_data())
    attrited_employees <- sum(filtered_data()$Attrition == "Yes")
    attrition_rate <- ifelse(total_employees > 0, (attrited_employees / total_employees) * 100, 0)
    valueBox(
      value = paste0(round(attrition_rate, 2), "%"),
      subtitle = "Overall Attrition Rate",
      icon = icon("user-minus"),
      color = if(attrition_rate > 15) "red" else "green"
    )
  })
  
  output$avg_monthly_income_kpi <- renderValueBox({
    avg_income <- mean(filtered_data()$MonthlyIncome, na.rm = TRUE)
    valueBox(
      value = scales::dollar(round(avg_income)),
      subtitle = "Average Monthly Income",
      icon = icon("money-bill-wave"),
      color = "purple"
    )
  })
  
  output$avg_years_at_company_kpi <- renderValueBox({
    avg_years <- mean(filtered_data()$YearsAtCompany, na.rm = TRUE)
    valueBox(
      value = round(avg_years, 1),
      subtitle = "Average Years at Company",
      icon = icon("building"),
      color = "teal"
    )
  })
  
  # Create the grid of 4 graphs based on selected category
  output$graph_grid <- renderUI({
    if (selected_category() == "attrition") {
      fluidRow(
        # NEW ATTRITION PLOTS
        box(title = div(id = "attrition_contour_density_title", "Attrition Density Contour: Age vs. Monthly Income"), width = 6, plotlyOutput("attrition_contour_density")),
        bsTooltip(id = "attrition_contour_density_title", title = "Visualizes the density of employees across Age and Monthly Income, with contour lines indicating concentrations. Color by Attrition.", placement = "right", trigger = "hover"),
        
        box(title = div(id = "attrition_cubemesh_rainbow_title", "3D Scatter: Satisfaction Levels with Monthly Income (Rainbow)"), width = 6, plotlyOutput("attrition_cubemesh_rainbow")),
        bsTooltip(id = "attrition_cubemesh_rainbow_title", title = "A 3D scatter plot of satisfaction levels, with points colored by monthly income using a rainbow palette.", placement = "left", trigger = "hover"),
        
        box(title = div(id = "globe_contour_plot_title", "Interactive Globe Contours"), width = 12, plotlyOutput("globe_contour_plot")), # Globe plot, wider
        bsTooltip(id = "globe_contour_plot_title", title = "An interactive 3D globe with contour lines, allowing for rotation and projection changes.", placement = "bottom", trigger = "hover")
      )
    } else if (selected_category() == "income") {
      fluidRow(
        # Note: The income_bubble was set as imageOutput, but your data doesn't have a direct 'Year' column for gganimate yet.
        # This will still require a proper 'Year' column in your 'data' object for it to work.
        # I'll keep it as imageOutput and assume you'll either add 'Year' to 'data' or use another anim_data for it.
        # I've added a dummy Year column at the top of the script.
        box(title = div(id = "income_bubble_title", "Income Bubble Chart by Department Over Time"), width = 6, imageOutput("income_bubble")),
        bsTooltip(id = "income_bubble_title", title = "Animated bubble chart showing average income, employee count, and attrition rate by department over time. Size represents employee count, color shows attrition rate.", placement = "left", trigger = "hover"),
        box(title = div(id = "income_density_age_title", "Income Density by Age Group (Click Age Group to Filter)"), width = 6, plotlyOutput("income_density_age")),
        bsTooltip(id = "income_density_age_title", title = "Displays the density distribution of monthly income across different age groups, revealing income patterns. Click on a density curve to filter all plots by that Age Group.", placement = "right", trigger = "hover"),
        # REPLACED age_income_2d with income_forest_plot
        box(title = div(id = "income_forest_plot_title", "Average Monthly Income by Job Role (with 95% CI)"), width = 6, plotlyOutput("income_forest_plot")),
        bsTooltip(id = "income_forest_plot_title", title = "Shows the average monthly income for each Job Role along with their 95% confidence intervals, highlighting income disparities.", placement = "left", trigger = "hover"),
        box(title = div(id = "income_distance_2d_title", "2D Density: Income vs. Distance From Home"), width = 6, plotlyOutput("income_distance_2d")),
        bsTooltip(id = "income_distance_2d_title", title = "Visualizes the density of employees based on their monthly income and distance from home, highlighting clusters. This chart reflects selected filters.", placement = "right", trigger = "hover")
      )
    } else if (selected_category() == "satisfaction") {
      fluidRow(
        # NEW SATISFACTION PLOTS (3D)
        box(title = div(id = "satisfaction_3d_scatter_title", "3D Scatter: Job Sat. vs. Env. Sat. vs. Work-Life Balance"), width = 6, plotlyOutput("satisfaction_3d_scatter")),
        bsTooltip(id = "satisfaction_3d_scatter_title", title = "Visualizes the relationship between different satisfaction metrics in a 3D space. Each point represents an employee. This chart reflects selected filters.", placement = "right", trigger = "hover"),
        box(title = div(id = "satisfaction_3d_density_title", "3D Density Plot: Job Satisfaction by Income and Age"), width = 6, plotlyOutput("satisfaction_3d_density")),
        bsTooltip(id = "satisfaction_3d_density_title", title = "Shows the concentration of job satisfaction across different income and age levels in 3D. This chart reflects selected filters.", placement = "left", trigger = "hover"),
        box(title = div(id = "satisfaction_3d_cuboid_title", "3D Cuboid: Average Satisfaction by Department and Attrition"), width = 6, plotlyOutput("satisfaction_3d_cuboid")),
        bsTooltip(id = "satisfaction_3d_cuboid_title", title = "Represents average satisfaction levels for departments as 3D bars, separated by attrition status. This chart reflects selected filters.", placement = "right", trigger = "hover"),
        box(title = div(id = "satisfaction_3d_random_walk_title", "3D Random Walk: Conceptual Employee Satisfaction Journey"), width = 6, plotlyOutput("satisfaction_3d_random_walk")),
        bsTooltip(id = "satisfaction_3d_random_walk_title", title = "A conceptual plot illustrating random movement in satisfaction scores, simulating employee journey variations. This chart reflects selected filters.", placement = "left", trigger = "hover")
      )
    } else if (selected_category() == "demographic") {
      fluidRow(
        box(title = div(id = "demographic_line_age_tenure_title", "Average Age vs. Years at Company by Department (Click Department to Filter)"), width = 6, plotlyOutput("demographic_line_age_tenure")),
        bsTooltip(id = "demographic_line_age_tenure_title", title = "Illustrates how the average age of employees changes with their tenure within each department. Click on a line to filter all plots by Department.", placement = "right", trigger = "hover"),
        box(title = div(id = "demographic_line_income_agegroup_title", "Average Income by Age Group by Marital Status (Click Marital Status to Filter)"), width = 6, plotlyOutput("demographic_line_income_agegroup")),
        bsTooltip(id = "demographic_line_income_agegroup_title", title = "Shows the average monthly income progression across different age groups, separated by marital status. Click on a line to filter all plots by Marital Status.", placement = "left", trigger = "hover"),
        box(title = div(id = "demographic_violin_tenure_agegroup_title", "Years at Company Distribution by Age Group (Click Age Group to Filter)"), width = 6, plotlyOutput("demographic_violin_tenure_agegroup")),
        bsTooltip(id = "demographic_violin_tenure_agegroup_title", title = "Compares the distribution of years spent at the company across different age groups. Click on a violin to filter all plots by Age Group.", placement = "right", trigger = "hover"),
        box(title = div(id = "demographic_timeseries_employee_count_title", "Employee Count Over Time by Department (Click Department to Filter)"), width = 6, plotlyOutput("demographic_timeseries_employee_count")),
        bsTooltip(id = "demographic_timeseries_employee_count_title", title = "Tracks the number of employees over the years for each department, revealing growth or decline patterns. Click on a line to filter all plots by Department.", placement = "left", trigger = "hover")
      )
    } else if (selected_category() == "department") {
      fluidRow(
        # NEW DEPARTMENT PLOTS
        box(title = div(id = "department_stacked_area_income_title", "Stacked Bar: Monthly Income by Department and Education Field"), width = 6, plotlyOutput("department_stacked_area_income")),
        bsTooltip(id = "department_stacked_area_income_title", title = "Visualizes the total monthly income by department, segmented by education field. This chart reflects selected filters.", placement = "right", trigger = "hover"),
        box(title = div(id = "department_funnel_attrition_title", "Attrition Funnel by Job Role"), width = 6, plotlyOutput("department_funnel_attrition")),
        bsTooltip(id = "department_funnel_attrition_title", title = "Shows the progression from initial employee count to attrition across job roles, highlighting points of drop-off. This chart reflects selected filters.", placement = "left", trigger = "hover"),
        box(title = div(id = "department_jobrole_treemap_title", "Job Role Distribution by Department"), width = 6, plotOutput("department_jobrole_treemap")),
        bsTooltip(id = "department_jobrole_treemap_title", title = "A treemap showing the proportion of employees in each job role within departments. This chart reflects selected filters.", placement = "right", trigger = "hover"),
        box(title = div(id = "department_facet_boxplot_yearsatcompany_title", "Years at Company Distribution by Department and Attrition"), width = 6, plotlyOutput("department_facet_boxplot_yearsatcompany")),
        bsTooltip(id = "department_facet_boxplot_yearsatcompany_title", title = "Compares the distribution of years at company across departments, faceted by attrition status. This chart reflects selected filters.", placement = "left", trigger = "hover")
      )
    }
  })
  
  # --- Observe click events and update filters ---
  
  # Attrition Analysis Clicks (rv$filtered_attrition, rv$filtered_job_role, rv$filtered_education_field)
  # No direct click filtering implemented for the new Attrition plots for now,
  # as their nature (contour, mesh, globe) makes direct single-click filtering less intuitive
  # without specific mapping logic. If desired, this can be added.
  
  
  # Income Analysis Clicks (rv$filtered_age_group, rv$filtered_department, rv$filtered_job_role)
  observeEvent(event_data("plotly_click", source = "income_density_age_plot"), {
    click_data <- event_data("plotly_click", source = "income_density_age_plot")
    if (!is.null(click_data) && !is.null(click_data$customdata)) { # Customdata holds AgeGroup
      clicked_value <- click_data$customdata[[1]]
      if (isTRUE(rv$filtered_age_group == clicked_value)) {
        rv$filtered_age_group <- NULL
      } else {
        rv$filtered_age_group <- clicked_value
      }
    }
  })
  
  # New click for Forest Plot (Job Role)
  observeEvent(event_data("plotly_click", source = "income_forest_plot"), {
    click_data <- event_data("plotly_click", source = "income_forest_plot")
    if (!is.null(click_data) && !is.null(click_data$y)) { # y is JobRole in this plot
      clicked_value <- click_data$y[[1]]
      if (isTRUE(rv$filtered_job_role == clicked_value)) {
        rv$filtered_job_role <- NULL
      } else {
        rv$filtered_job_role <- clicked_value
      }
    }
  })
  
  observeEvent(event_data("plotly_click", source = "income_distribution_violin_plot"), {
    click_data <- event_data("plotly_click", source = "income_distribution_violin_plot")
    if (!is.null(click_data) && !is.null(click_data$x)) { # x is Department in this plot
      clicked_value <- click_data$x[[1]]
      if (isTRUE(rv$filtered_department == clicked_value)) {
        rv$filtered_department <- NULL
      } else {
        rv$filtered_department <- clicked_value
      }
    }
  })
  
  # Satisfaction Analysis Clicks (rv$filtered_department, rv$filtered_job_satisfaction)
  # No direct filtering from 3D plots added as per "no other changes" outside plot definitions.
  # If 3D plot interaction for filtering is required, it would need more complex implementation
  # and additions to rv and observeEvent logic.
  
  # Demographic Analysis Clicks (rv$filtered_department, rv$filtered_marital_status, rv$filtered_age_group)
  observeEvent(event_data("plotly_click", source = "demographic_line_age_tenure_plot"), {
    click_data <- event_data("plotly_click", source = "demographic_line_age_tenure_plot")
    if (!is.null(click_data) && !is.null(click_data$customdata)) {
      clicked_value <- click_data$customdata[[1]] # Assuming customdata holds the Department value
      if (isTRUE(rv$filtered_department == clicked_value)) {
        rv$filtered_department <- NULL
      } else {
        rv$filtered_department <- clicked_value
      }
    }
  })
  
  observeEvent(event_data("plotly_click", source = "demographic_line_income_agegroup_plot"), {
    click_data <- event_data("plotly_click", source = "demographic_line_income_agegroup_plot")
    if (!is.null(click_data) && !is.null(click_data$customdata)) {
      clicked_value <- click_data$customdata[[1]] # Assuming customdata holds MaritalStatus
      if (isTRUE(rv$filtered_marital_status == clicked_value)) {
        rv$filtered_marital_status <- NULL
      } else {
        rv$filtered_marital_status <- clicked_value
      }
    }
  })
  
  observeEvent(event_data("plotly_click", source = "demographic_violin_tenure_agegroup_plot"), {
    click_data <- event_data("plotly_click", source = "demographic_violin_tenure_agegroup_plot")
    if (!is.null(click_data) && !is.null(click_data$x)) { # x is AgeGroup in this plot
      clicked_value <- click_data$x[[1]]
      if (isTRUE(rv$filtered_age_group == clicked_value)) {
        rv$filtered_age_group <- NULL
      } else {
        rv$filtered_age_group <- clicked_value
      }
    }
  })
  
  observeEvent(event_data("plotly_click", source = "demographic_timeseries_employee_count_plot"), {
    click_data <- event_data("plotly_click", source = "demographic_timeseries_employee_count_plot")
    if (!is.null(click_data) && !is.null(click_data$customdata)) {
      clicked_value <- click_data$customdata[[1]] # Assuming customdata holds Department
      if (isTRUE(rv$filtered_department == clicked_value)) {
        rv$filtered_department <- NULL
      } else {
        rv$filtered_department <- clicked_value
      }
    }
  })
  
  # Department Analysis Clicks (rv$filtered_department)
  # No direct filtering from newly added Department plots as per "no other changes" outside plot definitions.
  # If new Department plot interaction for filtering is required, it would need more complex implementation
  # and additions to rv and observeEvent logic.
  
  
  # --- NEW Attrition Analysis Plots (REPLACED) ---
  
  # 1. Contour Level Configuration Graph (Age vs. Monthly Income, colored by Attrition)
  output$attrition_contour_density <- renderPlotly({
    if (nrow(filtered_data()) == 0) {
      return(plotly_empty() %>% layout(title = "No data for selected filters"))
    }
    
    # Use density for contour levels, separated by Attrition
    p <- ggplot(filtered_data(), aes(x = Age, y = MonthlyIncome, color = Attrition)) +
      stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.5) +
      scale_fill_gradientn(colors = selected_palette()) + # Use selected palette for fill
      scale_color_manual(values = selected_palette()[c(2,1)]) + # Use selected palette for line colors
      labs(x = "Age", y = "Monthly Income", title = "Attrition Density Contour: Age vs. Monthly Income") +
      theme_minimal()
    
    ggplotly(p) %>% layout(
      xaxis = list(rangeslider = list(visible = T)),
      yaxis = list(rangeslider = list(visible = T))
    )
  })
  
  # 2. Cube Mesh Plot with Rainbow Color (Job Satisfaction, Environment Satisfaction, Work-Life Balance)
  # Changed to 3D Scatter with color representing MonthlyIncome to achieve "rainbow color" effect
  # as a true mesh of satisfaction distributions is complex and not directly supported for arbitrary 3D data in Plotly like this.
  output$attrition_cubemesh_rainbow <- renderPlotly({
    if (nrow(filtered_data()) == 0) {
      return(plotly_empty() %>% layout(title = "No data for selected filters"))
    }
    
    plot_ly(filtered_data(), x = ~JobSatisfaction, y = ~EnvironmentSatisfaction, z = ~WorkLifeBalance,
            color = ~MonthlyIncome, # Color by MonthlyIncome for rainbow effect
            colors = 'Spectral', # Use 'Spectral' for rainbow-like, or selected_palette() if it's continuous
            type = 'scatter3d', mode = 'markers',
            marker = list(size = 5, opacity = 0.7),
            hoverinfo = "text",
            text = ~paste('Job Sat: ', JobSatisfaction,
                          '<br>Env Sat: ', EnvironmentSatisfaction,
                          '<br>WLB: ', WorkLifeBalance,
                          '<br>Income: ', scales::dollar(MonthlyIncome))) %>%
      layout(scene = list(xaxis = list(title = 'Job Satisfaction'),
                          yaxis = list(title = 'Environment Satisfaction'),
                          zaxis = list(title = 'Work-Life Balance')),
             title = "3D Scatter: Satisfaction Levels with Monthly Income (Rainbow)")
  })
  
  # 3. Recreate Globe Contour Plot
  output$globe_contour_plot <- renderPlotly({
    # Load the specific globe contour data
    # This URL directly accesses the raw CSV from GitHub.
    # For robust deployment, you might want to download this file and include it in your app's 'www' folder.
    df_globe <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/globe_contours.csv')
    df_globe$id <- seq_len(nrow(df_globe))
    d_globe <- df_globe %>%
      gather(key, value, -id) %>%
      separate(key, c("l", "line"), "\\.") %>%
      spread(l, value)
    
    geo_globe <- list(
      showland = TRUE,
      showlakes = TRUE,
      showcountries = TRUE,
      showocean = TRUE,
      countrywidth = 0.5,
      landcolor = 'rgb(230, 145, 56)',
      lakecolor = 'rgb(0, 255, 255)',
      oceancolor = 'rgb(0, 255, 255)',
      projection = list(
        type = 'orthographic',
        rotation = list(
          lon = -100,
          lat = 40,
          roll = 0
        )
      ),
      lonaxis = list(
        showgrid = TRUE,
        gridcolor = toRGB("gray40"),
        gridwidth = 0.5
      ),
      lataxis = list(
        showgrid = TRUE,
        gridcolor = toRGB("gray40"),
        gridwidth = 0.5
      )
    )
    
    # Add custom events as in the original code (projection dropdowns and sliders)
    projections = data.frame(type = c("equirectangular", "mercator", "orthographic", "natural earth","kavrayskiy7",
                                      "miller", "robinson", "eckert4", "azimuthal equal area","azimuthal equidistant",
                                      "conic equal area", "conic conformal", "conic equidistant", "gnomonic", "stereographic",
                                      "mollweide", "hammer", "transverse mercator", "albers usa", "winkel tripel"))
    all_buttons <- list()
    for (i in 1:length(projections[,])) {
      all_buttons[[i]] <- list(method = "relayout",
                               args = list(list(geo.projection.type = projections$type[i])),
                               label = projections$type[i])
    }
    
    lon_range = data.frame(seq(-180, 180, 10))
    lat_range = data.frame(seq(-90, 90, 10))
    colnames(lon_range) <- "x"
    colnames(lat_range) <- "x"
    
    all_lat <- list()
    for (i in 1:length(lat_range[,])) {
      all_lat[[i]] <- list(method = "relayout",
                           args = list(list(geo.projection.rotation.lat = lat_range$x[i])),
                           label = lat_range$x[i])
    }
    all_lon <- list()
    for (i in 1:length(lon_range[,])) {
      all_lon[[i]] <- list(method = "relayout",
                           args = list(list(geo.projection.rotation.lon = lon_range$x[i])),
                           label = lon_range$x[i])
    }
    
    annot <- list(x = 0, y=0.8, text = "Projection", yanchor = 'bottom',
                  xref = 'paper', xanchor = 'right',
                  showarrow = FALSE)
    
    fig_globe <- plot_geo(d_globe)
    fig_globe <- fig_globe %>% group_by(line)
    fig_globe <- fig_globe %>% add_lines(x = ~lon, y = ~lat, color = ~line, colors = 'Reds')
    fig_globe <- fig_globe %>% layout(
      showlegend = FALSE, geo = geo_globe,
      annotations = annot,
      updatemenus = list(list(active = 2, x = 0, y = 0.8,
                              buttons=all_buttons)),
      sliders = list(
        list(
          active = (length(lon_range[,])-1)/2,
          currentvalue = list(prefix = "Longitude: "),
          pad = list(t = 20),
          steps = all_lon),
        list(
          active = (length(lat_range[,])-1)/2,
          currentvalue = list(prefix = "Latitude: "),
          pad = list(t = 100),
          steps = all_lat))
    )
    fig_globe
  })
  
  # --- Income Analysis Plots (Modified to include Forest Plot) ---
  output$income_density_age <- renderPlotly({
    if (nrow(filtered_data()) == 0) {
      return(plotly_empty() %>% layout(title = "No data for selected filters"))
    }
    p <- ggplot(filtered_data(), aes(x = MonthlyIncome, fill = AgeGroup, color = AgeGroup, customdata = AgeGroup)) +
      geom_density(alpha = 0.5) +
      facet_wrap(~ AgeGroup, scales = "free_y") +
      scale_fill_manual(values = selected_palette()) +
      scale_color_manual(values = selected_palette()) +
      scale_x_continuous(labels = scales::dollar) +
      labs(x = "Monthly Income", y = "Density", title = "Monthly Income Distribution by Age Group") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p, source = "income_density_age_plot", tooltip = c("x", "fill", "customdata")) %>% layout(
      xaxis = list(rangeslider = list(visible = T)),
      yaxis = list(rangeslider = list(visible = T))
    )
  })
  
  # NEW: Forest Plot for Monthly Income by Job Role
  output$income_forest_plot <- renderPlotly({
    if (nrow(filtered_data()) == 0) {
      return(plotly_empty() %>% layout(title = "No data for selected filters"))
    }
    
    # Calculate mean, standard deviation, and count for each JobRole
    forest_data <- filtered_data() %>%
      group_by(JobRole) %>%
      summarise(
        MeanIncome = mean(MonthlyIncome, na.rm = TRUE),
        StdDevIncome = sd(MonthlyIncome, na.rm = TRUE),
        Count = n(),
        .groups = 'drop'
      ) %>%
      filter(Count > 1) %>% # Ensure enough data to calculate CI
      mutate(
        SE = StdDevIncome / sqrt(Count),
        LowerCI = MeanIncome - (1.96 * SE),
        UpperCI = MeanIncome + (1.96 * SE)
      ) %>%
      arrange(MeanIncome) # Order for better visualization in forest plot
    
    if(nrow(forest_data) == 0){
      return(plotly_empty() %>% layout(title = "Not enough data for Forest Plot after filtering (need > 1 employee per Job Role)"))
    }
    
    p <- ggplot(forest_data, aes(x = MeanIncome, y = JobRole, customdata = JobRole)) +
      geom_point(color = selected_palette()[1], size = 3) +
      geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2, color = selected_palette()[2]) +
      scale_x_continuous(labels = scales::dollar) +
      labs(x = "Average Monthly Income (with 95% CI)", y = "Job Role",
           title = "Average Monthly Income by Job Role (with 95% CI)") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 9))
    
    ggplotly(p, source = "income_forest_plot", tooltip = c("y", "x", "xmin", "xmax", "customdata")) %>% layout(
      xaxis = list(rangeslider = list(visible = T))
    )
  })
  
  output$income_distance_2d <- renderPlotly({
    if (nrow(filtered_data()) == 0) {
      return(plotly_empty() %>% layout(title = "No data for selected filters"))
    }
    p <- ggplot(filtered_data(), aes(x = DistanceFromHome, y = MonthlyIncome)) +
      geom_bin2d(bins = 50) +
      scale_fill_gradientn(colors = selected_palette()) +
      scale_y_continuous(labels = scales::dollar) +
      labs(x = "Distance From Home", y = "Monthly Income", fill = "Count") +
      theme_minimal()
    
    ggplotly(p) %>% layout(
      xaxis = list(rangeslider = list(visible = T)),
      yaxis = list(rangeslider = list(visible = T))
    )
  })
  
  output$income_distribution_violin <- renderPlotly({
    if (nrow(filtered_data()) == 0) {
      return(plotly_empty() %>% layout(title = "No data for selected filters"))
    }
    p <- ggplot(filtered_data(), aes(x = Department, y = MonthlyIncome, fill = Department, customdata = Department)) + # Added customdata
      geom_violin(trim = FALSE) +
      geom_boxplot(width = 0.1, fill = "white") +
      scale_fill_manual(values = selected_palette()) +
      scale_y_continuous(labels = scales::dollar) +
      labs(x = "Department", y = "Monthly Income") +
      theme_minimal() +
      ggtitle("Income Distribution by Department (Violin Plot)")
    
    ggplotly(p, source = "income_distribution_violin_plot", tooltip = c("x", "y", "customdata")) %>% layout(showlegend = FALSE,
                                                                                                            yaxis = list(rangeslider = list(visible = T))
    )
  })
  
  # Animated Bubble Chart (Income Analysis) - This one needs a Year column in `data` to work
  output$income_bubble <- renderImage({
    # Ensure 'Year' column exists in your actual data or adapt this.
    # For demo, I'm using the `data` object with a dummy 'Year' created at the top.
    anim_data_income <- data %>% filter(Year >= 2010 & Year <= 2020) %>%
      group_by(Year, Department) %>%
      summarise(
        AverageMonthlyIncome = mean(MonthlyIncome, na.rm = TRUE),
        EmployeeCount = n(),
        AttritionRate = mean(Attrition == "Yes", na.rm = TRUE), # Calculate attrition rate
        .groups = 'drop'
      ) %>%
      ungroup()
    
    if (nrow(anim_data_income) == 0) {
      # Return a placeholder image if no data
      outfile <- tempfile(fileext = '.png')
      png(outfile, width=600, height=400)
      plot(NA, xlim=c(0,1), ylim=c(0,1), bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
      text(0.5, 0.5, "No data for selected filters or years for animation", cex = 1.2)
      dev.off()
      return(list(src = outfile, contentType = 'image/png', width = "100%", height = "100%"))
    }
    
    anim <- ggplot(anim_data_income, aes(x = AverageMonthlyIncome, y = AttritionRate, size = EmployeeCount, color = Department, frame = Year)) +
      geom_point(alpha = 0.7) +
      scale_size_area(max_size = 20) +
      scale_color_manual(values = selected_palette()) +
      labs(
        x = "Average Monthly Income",
        y = "Attrition Rate",
        title = "Department Income vs. Attrition Rate - Year: {frame_time}"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold")) +
      transition_states(Year, transition_length = 2, state_length = 1) +
      ease_aes('linear')
    
    gif_path <- file.path(tempdir(), "income_bubble.gif")
    anim_save(gif_path, animate(anim, duration = 15, fps = 10, renderer = gifski_renderer(loop = TRUE)))
    
    list(src = gif_path,
         contentType = 'image/gif',
         width = "100%",
         height = "100%",
         alt = "Animated Income Bubble Chart")
  }, deleteFile = TRUE)
  
  # --- NEW Satisfaction Analysis Plots (3D) ---
  output$satisfaction_3d_scatter <- renderPlotly({
    if (nrow(filtered_data()) == 0) {
      return(plotly_empty() %>% layout(title = "No data for selected filters"))
    }
    plot_ly(filtered_data(),
            x = ~JobSatisfaction, y = ~EnvironmentSatisfaction, z = ~WorkLifeBalance,
            type = "scatter3d", mode = "markers",
            marker = list(size = 5, color = ~Attrition == "Yes", colorscale = c('blue', 'red'), showscale = FALSE, opacity = 0.7),
            hoverinfo = "text",
            text = ~paste('Job Sat: ', JobSatisfaction, '<br>Env Sat: ', EnvironmentSatisfaction,
                          '<br>WLB: ', WorkLifeBalance, '<br>Attrition: ', Attrition)) %>%
      layout(scene = list(xaxis = list(title = 'Job Satisfaction'),
                          yaxis = list(title = 'Environment Satisfaction'),
                          zaxis = list(title = 'Work-Life Balance')),
             title = "3D Scatter: Job Sat. vs. Env. Sat. vs. Work-Life Balance")
  })
  
  output$satisfaction_3d_density <- renderPlotly({
    if (nrow(filtered_data()) == 0) {
      return(plotly_empty() %>% layout(title = "No data for selected filters"))
    }
    
    # Approximating 3D density: Binning data to create 'density'
    density_data <- filtered_data() %>%
      group_by(AgeGroup, IncomeGroup, JobSatisfaction) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      # Convert factor levels to numeric midpoints for plotting as continuous axes
      mutate(AgeMid = as.numeric(as.factor(AgeGroup)),
             IncomeMid = as.numeric(as.factor(IncomeGroup)))
    
    plot_ly(density_data,
            x = ~AgeMid, y = ~IncomeMid, z = ~JobSatisfaction,
            type = "scatter3d", mode = "markers",
            marker = list(size = ~log(Count + 1)*5, opacity = 0.8, color = ~Count, colorscale = selected_palette()),
            hoverinfo = "text",
            text = ~paste('Age Group: ', AgeGroup, '<br>Income Group: ', IncomeGroup,
                          '<br>Job Sat: ', JobSatisfaction, '<br>Count: ', Count)) %>%
      layout(scene = list(xaxis = list(title = 'Age Group (Mapped)', tickvals = 1:length(levels(data$AgeGroup)), ticktext = levels(data$AgeGroup)),
                          yaxis = list(title = 'Income Group (Mapped)', tickvals = 1:length(levels(data$IncomeGroup)), ticktext = levels(data$IncomeGroup)),
                          zaxis = list(title = 'Job Satisfaction')),
             title = "3D Density Plot: Job Satisfaction by Income and Age (Binned)")
  })
  
  output$satisfaction_3d_cuboid <- renderPlotly({
    if (nrow(filtered_data()) == 0) {
      return(plotly_empty() %>% layout(title = "No data for selected filters"))
    }
    # Calculate average satisfaction levels by department and attrition
    avg_sat_data <- filtered_data() %>%
      group_by(Department, Attrition) %>%
      summarise(
        AvgJobSatisfaction = mean(JobSatisfaction, na.rm = TRUE),
        AvgEnvironmentSatisfaction = mean(EnvironmentSatisfaction, na.rm = TRUE),
        AvgWorkLifeBalance = mean(WorkLifeBalance, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      pivot_longer(starts_with("Avg"), names_to = "Metric", values_to = "AverageSatisfaction") %>%
      mutate(Metric = factor(Metric, levels = c("AvgJobSatisfaction", "AvgEnvironmentSatisfaction", "AvgWorkLifeBalance"),
                             labels = c("Job Sat.", "Env. Sat.", "WLB")))
    
    # Using 3D bars as a visual representation
    plot_ly(
      data = avg_sat_data,
      x = ~Department, y = ~Metric, z = ~AverageSatisfaction,
      color = ~Attrition, colors = selected_palette()[c(2,1)],
      type = "bar"
    ) %>%
      layout(scene = list(xaxis = list(title = 'Department'),
                          yaxis = list(title = 'Satisfaction Metric'),
                          zaxis = list(title = 'Average Satisfaction (1-4)')),
             title = "3D Bars: Average Satisfaction by Department and Attrition")
  })
  
  output$satisfaction_3d_random_walk <- renderPlotly({
    if (nrow(filtered_data()) == 0) {
      return(plotly_empty() %>% layout(title = "No data for selected filters"))
    }
    # Create a conceptual random walk based on satisfaction scores
    # This isn't a true random walk over time for *each* employee, but rather
    # a visual representation of how different satisfaction scores could move
    # through a 3D space, showing variability.
    
    # Generate a simple random walk data for demonstration
    num_steps <- 100
    walk_data <- data.frame(
      step = 1:num_steps,
      x = cumsum(rnorm(num_steps, sd = 0.5)),
      y = cumsum(rnorm(num_steps, sd = 0.5)),
      z = cumsum(rnorm(num_steps, sd = 0.5))
    )
    # Scale to match satisfaction ranges
    walk_data$x <- scales::rescale(walk_data$x, to = c(1, 4))
    walk_data$y <- scales::rescale(walk_data$y, to = c(1, 4))
    walk_data$z <- scales::rescale(walk_data$z, to = c(1, 4))
    
    # Add a conceptual "Employee Group" to color by
    walk_data$Group <- sample(c("Group A", "Group B", "Group C"), num_steps, replace = TRUE)
    
    plot_ly(walk_data, type = 'scatter3d', mode = 'lines+markers') %>%
      add_trace(x = ~x, y = ~y, z = ~z, color = ~Group, colors = selected_palette(),
                line = list(width = 2), marker = list(size = 3),
                hoverinfo = "text",
                text = ~paste('Step: ', step, '<br>X: ', round(x, 2), '<br>Y: ', round(y, 2), '<br>Z: ', round(z, 2), '<br>Group: ', Group)) %>%
      layout(scene = list(xaxis = list(title = 'Conceptual Job Satisfaction', range = c(1,4)),
                          yaxis = list(title = 'Conceptual Environment Satisfaction', range = c(1,4)),
                          zaxis = list(title = 'Conceptual Work-Life Balance', range = c(1,4))),
             title = "3D Random Walk: Conceptual Employee Satisfaction Journey")
  })
  
  # --- Demographic Analysis Plots (kept as is) ---
  output$demographic_line_age_tenure <- renderPlotly({
    if (nrow(filtered_data()) == 0) {
      return(plotly_empty() %>% layout(title = "No data for selected filters"))
    }
    avg_data <- filtered_data() %>%
      group_by(Department, YearsAtCompany) %>%
      summarise(AverageAge = mean(Age, na.rm = TRUE), .groups = 'drop')
    
    p <- ggplot(avg_data, aes(x = YearsAtCompany, y = AverageAge, color = Department, group = Department, customdata = Department)) +
      geom_line() +
      geom_point() +
      scale_color_manual(values = selected_palette()) +
      labs(x = "Years at Company", y = "Average Age", title = "Average Age vs. Years at Company by Department") +
      theme_minimal()
    
    ggplotly(p, source = "demographic_line_age_tenure_plot", tooltip = c("x", "y", "color", "customdata")) %>% layout(
      xaxis = list(rangeslider = list(visible = T)),
      yaxis = list(rangeslider = list(visible = T))
    )
  })
  
  output$demographic_line_income_agegroup <- renderPlotly({
    if (nrow(filtered_data()) == 0) {
      return(plotly_empty() %>% layout(title = "No data for selected filters"))
    }
    avg_data <- filtered_data() %>%
      group_by(AgeGroup, MaritalStatus) %>%
      summarise(AverageIncome = mean(MonthlyIncome, na.rm = TRUE), .groups = 'drop')
    
    p <- ggplot(avg_data, aes(x = AgeGroup, y = AverageIncome, color = MaritalStatus, group = MaritalStatus, customdata = MaritalStatus)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      scale_color_manual(values = selected_palette()) +
      scale_y_continuous(labels = scales::dollar) +
      labs(x = "Age Group", y = "Average Monthly Income", title = "Average Income by Age Group by Marital Status") +
      theme_minimal()
    
    ggplotly(p, source = "demographic_line_income_agegroup_plot", tooltip = c("x", "y", "color", "customdata")) %>% layout(
      yaxis = list(rangeslider = list(visible = T))
    )
  })
  
  output$demographic_violin_tenure_agegroup <- renderPlotly({
    if (nrow(filtered_data()) == 0) {
      return(plotly_empty() %>% layout(title = "No data for selected filters"))
    }
    p <- ggplot(filtered_data(), aes(x = AgeGroup, y = YearsAtCompany, fill = AgeGroup, customdata = AgeGroup)) +
      geom_violin(trim = FALSE) +
      geom_boxplot(width = 0.1, fill = "white") +
      scale_fill_manual(values = selected_palette()) +
      labs(x = "Age Group", y = "Years at Company", title = "Years at Company Distribution by Age Group") +
      theme_minimal()
    
    ggplotly(p, source = "demographic_violin_tenure_agegroup_plot", tooltip = c("x", "y", "customdata")) %>% layout(showlegend = FALSE,
                                                                                                                    yaxis = list(rangeslider = list(visible = T))
    )
  })
  
  output$demographic_timeseries_employee_count <- renderPlotly({
    if (nrow(filtered_data()) == 0) {
      return(plotly_empty() %>% layout(title = "No data for selected filters"))
    }
    employee_count_ts <- filtered_data() %>%
      group_by(Year, Department) %>%
      summarise(EmployeeCount = n(), .groups = 'drop')
    
    p <- ggplot(employee_count_ts, aes(x = Year, y = EmployeeCount, color = Department, group = Department, customdata = Department)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      scale_color_manual(values = selected_palette()) +
      labs(x = "Year", y = "Employee Count", title = "Employee Count Over Time by Department") +
      theme_minimal()
    
    ggplotly(p, source = "demographic_timeseries_employee_count_plot", tooltip = c("x", "y", "color", "customdata")) %>% layout(
      xaxis = list(rangeslider = list(visible = T)),
      yaxis = list(rangeslider = list(visible = T))
    )
  })
  
  # --- NEW Department Analysis Plots ---
  output$department_stacked_area_income <- renderPlotly({
    if (nrow(filtered_data()) == 0) {
      return(plotly_empty() %>% layout(title = "No data for selected filters"))
    }
    # Let's pivot data to show income by education field within department.
    income_by_edu_dept <- filtered_data() %>%
      group_by(Department, EducationField) %>%
      summarise(TotalMonthlyIncome = sum(MonthlyIncome, na.rm = TRUE), .groups = 'drop')
    
    p <- ggplot(income_by_edu_dept, aes(x = Department, y = TotalMonthlyIncome, fill = EducationField)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = selected_palette()) +
      scale_y_continuous(labels = scales::dollar) +
      labs(x = "Department", y = "Total Monthly Income", fill = "Education Field",
           title = "Total Monthly Income by Department and Education Field (Stacked)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>% layout(
      yaxis = list(rangeslider = list(visible = T))
    )
  })
  
  output$department_funnel_attrition <- renderPlotly({
    if (nrow(filtered_data()) == 0) {
      return(plotly_empty() %>% layout(title = "No data for selected filters"))
    }
    
    # Funnel plot for attrition by Job Role
    # Step 1: Total employees in each Job Role
    # Step 2: Attrited employees in each Job Role
    # Need to order job roles meaningfully for a funnel.
    funnel_data <- filtered_data() %>%
      group_by(JobRole, Attrition) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      pivot_wider(names_from = Attrition, values_from = Count, values_fill = 0) %>%
      rename(Total = No, Attrited = Yes) %>% # 'No' means they stayed, so Total for that role
      mutate(Retained = Total - Attrited) %>%
      arrange(desc(Total)) # Order by total employees for funnel effect
    
    # Structure for Plotly funnel
    plot_ly(
      type = "funnel",
      y = funnel_data$JobRole,
      x = funnel_data$Total,
      name = "Total",
      textinfo = "value+percent initial",
      marker = list(color = selected_palette()[1])
    ) %>%
      add_trace(
        x = funnel_data$Retained,
        name = "Retained",
        marker = list(color = selected_palette()[3])
      ) %>%
      add_trace(
        x = funnel_data$Attrited,
        name = "Attrited",
        marker = list(color = selected_palette()[2])
      ) %>%
      layout(
        yaxis = list(title = "Job Role"),
        title = "Attrition Funnel by Job Role",
        showlegend = TRUE
      )
  })
  
  output$department_jobrole_treemap <- renderPlot({
    if (nrow(filtered_data()) == 0) {
      plot(NA, xlim=c(0,1), ylim=c(0,1), bty='n', type='n', xaxt='n', yaxt='n', xlab='', ylab='')
      text(0.5, 0.5, "No data for selected filters", cex = 1.5)
      return(NULL)
    }
    treemap_data_jobrole <- filtered_data() %>%
      group_by(Department, JobRole) %>%
      summarise(Count = n(), .groups = 'drop')
    
    treemap(treemap_data_jobrole,
            index = c("Department", "JobRole"),
            vSize = "Count",
            type = "index", # Colors by JobRole index
            palette = selected_palette(),
            title = "Job Role Distribution by Department",
            fontsize.title = 12)
  })
  
  output$department_facet_boxplot_yearsatcompany <- renderPlotly({
    if (nrow(filtered_data()) == 0) {
      return(plotly_empty() %>% layout(title = "No data for selected filters"))
    }
    p <- ggplot(filtered_data(), aes(x = Department, y = YearsAtCompany, fill = Department)) +
      geom_boxplot() +
      facet_wrap(~ Attrition) +
      scale_fill_manual(values = selected_palette()) +
      labs(x = "Department", y = "Years at Company", title = "Years at Company Distribution by Department (Faceted by Attrition)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    
    ggplotly(p) %>% layout(
      yaxis = list(rangeslider = list(visible = T))
    )
  })
  
  
  # --- Animated Plots ---
  # These outputs produce GIF files that are then rendered as images.
  
  # 1. Animated Scatter Plot (Department Performance Trajectories)
  output$animated_dept_performance_scatter <- renderImage({
    # Filter data for animated plots (only relevant years)
    anim_data <- data %>% filter(Year >= 2010 & Year <= 2020) %>%
      group_by(Year, Department) %>%
      summarise(
        AverageIncome = mean(MonthlyIncome, na.rm = TRUE),
        AverageJobSatisfaction = mean(JobSatisfaction, na.rm = TRUE),
        EmployeeCount = n(),
        .groups = 'drop'
      ) %>%
      ungroup() # Ensure no residual grouping
    
    if (nrow(anim_data) == 0) {
      # Return a placeholder image if no data
      outfile <- tempfile(fileext = '.png')
      png(outfile, width=600, height=400)
      plot(NA, xlim=c(0,1), ylim=c(0,1), bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
      text(0.5, 0.5, "No data for selected filters or years for animation", cex = 1.2)
      dev.off()
      return(list(src = outfile, contentType = 'image/png', width = "100%", height = "100%"))
    }
    
    anim <- ggplot(anim_data, aes(x = AverageIncome, y = AverageJobSatisfaction, size = EmployeeCount, color = Department, frame = Year)) +
      geom_point(alpha = 0.7) +
      scale_size(range = c(5, 20)) +
      scale_color_manual(values = selected_palette()) +
      labs(
        x = "Average Monthly Income",
        y = "Average Job Satisfaction",
        title = "Department Performance Trajectories - Year: {frame_time}"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold")) +
      transition_states(Year, transition_length = 2, state_length = 1) +
      ease_aes('linear')
    
    gif_path <- file.path(tempdir(), "animated_dept_performance_scatter.gif")
    # Using 'animate' from gganimate and 'anim_save'
    anim_save(gif_path, animate(anim, duration = 15, fps = 10, renderer = gifski_renderer(loop = TRUE)))
    
    list(src = gif_path,
         contentType = 'image/gif',
         width = "100%",
         height = "100%",
         alt = "Animated Department Performance Trajectories")
  }, deleteFile = TRUE) # deleteFile = TRUE ensures the GIF is cleaned up after being served
  
  # 2. Animated Box Plot (Monthly Income Distribution by Department)
  output$animated_dept_income_boxplot <- renderImage({
    anim_data_boxplot <- data %>% filter(Year >= 2010 & Year <= 2020)
    
    if (nrow(anim_data_boxplot) == 0) {
      # Return a placeholder image if no data
      outfile <- tempfile(fileext = '.png')
      png(outfile, width=600, height=400)
      plot(NA, xlim=c(0,1), ylim=c(0,1), bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
      text(0.5, 0.5, "No data for selected years for animation", cex = 1.2)
      dev.off()
      return(list(src = outfile, contentType = 'image/png', width = "100%", height = "100%"))
    }
    
    anim <- ggplot(anim_data_boxplot, aes(x = Department, y = MonthlyIncome, fill = Department)) +
      geom_boxplot() +
      scale_fill_manual(values = selected_palette()) +
      labs(
        x = "Department",
        y = "Monthly Income",
        title = "Monthly Income Distribution by Department - Year: {frame_time}"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold")) +
      transition_time(Year) +
      ease_aes('linear')
    
    gif_path <- file.path(tempdir(), "animated_dept_income_boxplot.gif")
    anim_save(gif_path, animate(anim, duration = 15, fps = 10, renderer = gifski_renderer(loop = TRUE)))
    
    list(src = gif_path,
         contentType = 'image/gif',
         width = "100%",
         height = "100%",
         alt = "Animated Monthly Income Distribution by Department")
  }, deleteFile = TRUE)
  
  # 3. Animated Bar Race (Departmental Employee Count)
  output$animated_dept_bar_race <- renderImage({
    department_counts_by_year <- data %>%
      group_by(Year, Department) %>%
      summarise(EmployeeCount = n(), .groups = 'drop') %>%
      group_by(Year) %>%
      mutate(rank = rank(-EmployeeCount)) %>%
      ungroup()
    
    if (nrow(department_counts_by_year) == 0) {
      # Return a placeholder image if no data
      outfile <- tempfile(fileext = '.png')
      png(outfile, width=600, height=400)
      plot(NA, xlim=c(0,1), ylim=c(0,1), bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
      text(0.5, 0.5, "No data for selected years for animation", cex = 1.2)
      dev.off()
      return(list(src = outfile, contentType = 'image/png', width = "100%", height = "100%"))
    }
    
    anim <- ggplot(department_counts_by_year, aes(x = rank, y = EmployeeCount, fill = Department)) +
      geom_bar(stat = "identity") +
      # Adjust aesthetics for bar race visualization
      aes(y = EmployeeCount, x = rank, fill = Department) +
      geom_text(aes(label = Department, x = rank, y = EmployeeCount), hjust = -0.1) +
      geom_text(aes(label = EmployeeCount, x = rank, y = EmployeeCount), hjust = 1.1) +
      scale_fill_manual(values = selected_palette()) +
      labs(title = "Departmental Employee Count Race - Year: {frame_time}") +
      theme_minimal() +
      theme(
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        legend.position = "none"
      ) +
      coord_flip(clip = "off", ylim = c(0, max(department_counts_by_year$EmployeeCount) * 1.2)) +
      transition_time(Year) +
      ease_aes('cubic-in-out')
    
    gif_path <- file.path(tempdir(), "animated_dept_bar_race.gif")
    anim_save(gif_path, animate(anim, duration = 15, fps = 10, renderer = gifski_renderer(loop = TRUE)))
    
    list(src = gif_path,
         contentType = 'image/gif',
         width = "100%",
         height = "100%",
         alt = "Animated Departmental Employee Count Race")
  }, deleteFile = TRUE)
  
  
  # 4. Animated Scatter Plot (Age vs. YearsAtCompany by Department)
  output$animated_dept_demographics_scatter <- renderImage({
    anim_data_demographics <- data %>% filter(Year >= 2010 & Year <= 2020)
    
    if (nrow(anim_data_demographics) == 0) {
      # Return a placeholder image if no data
      outfile <- tempfile(fileext = '.png')
      png(outfile, width=600, height=400)
      plot(NA, xlim=c(0,1), ylim=c(0,1), bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
      text(0.5, 0.5, "No data for selected years for animation", cex = 1.2)
      dev.off()
      return(list(src = outfile, contentType = 'image/png', width = "100%", height = "100%"))
    }
    
    anim <- ggplot(anim_data_demographics, aes(x = Age, y = YearsAtCompany, color = Department, group = Department)) +
      geom_point(alpha = 0.6) +
      scale_color_manual(values = selected_palette()) +
      labs(
        x = "Age",
        y = "Years at Company",
        title = "Employee Age vs. Years at Company by Department - Year: {frame_time}"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold")) +
      transition_time(Year) +
      ease_aes('linear')
    
    gif_path <- file.path(tempdir(), "animated_dept_demographics_scatter.gif")
    anim_save(gif_path, animate(anim, duration = 10, fps = 10, renderer = gifski_renderer(loop = TRUE)))
    
    list(src = gif_path,
         contentType = 'image/gif',
         width = "100%",
         height = "100%",
         alt = "Animated Departmental Employee Count Race")
  }, deleteFile = TRUE)
  
  # --- Render the dynamic Education Field Image ---
  output$education_field_image <- renderUI({
    selected_field <- input$edu_field_selector
    image_url <- education_field_images[selected_field]
    
    if (!is.null(image_url) && !is.na(image_url)) {
      tags$img(src = image_url, width = "100%", style = "max-width: 600px; height: auto; display: block; margin-left: auto; margin-right: auto;")
    } else {
      # Handle cases where no image is defined for the selected field
      tags$p(paste("No image available for '", selected_field, "' Education Field. Please select another."), style = "text-align: center; color: grey;")
    }
  })
  
  # --- Render the dynamic Education Field Data Table ---
  output$education_field_data_table <- renderDataTable({
    req(input$edu_field_selector) # Ensure an education field is selected
    
    filtered_edu_data <- data %>%
      filter(EducationField == input$edu_field_selector) %>%
      select(
        JobRole,
        Department,
        Age,
        MaritalStatus,
        MonthlyIncome,
        YearsAtCompany,
        Attrition,
        JobSatisfaction,
        EnvironmentSatisfaction,
        WorkLifeBalance
      )
    
    if (nrow(filtered_edu_data) == 0) {
      return(data.frame(Message = "No employee data for the selected Education Field."))
    }
    
    filtered_edu_data
  }, options = list(pageLength = 10, scrollX = TRUE)) # Add options for pagination and horizontal scroll
  
} # <<< END OF SERVER FUNCTION

shinyApp(ui = ui, server = server)
