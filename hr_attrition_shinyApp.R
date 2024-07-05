# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)

# Check if the file exists at the specified path
file_path <- "C:/Users/brsle/Desktop/hr_app/hr_attrition.csv"
if (!file.exists(file_path)) {
  stop("File not found at the specified path.")
}

# Load the data
df <- read.csv(file_path)

# Define generation based on Age
df <- df %>%
  mutate(Generation = case_when(
    Age < 25 ~ "Gen Z",
    Age >= 25 & Age < 40 ~ "Millennial",
    Age >= 40 & Age < 56 ~ "Gen X",
    Age >= 56 ~ "Boomer"
  ))

# Define UI for application
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .main-title {
        text-align: center;
        color: black;
      }
      body {
        background-color: #f0f0f0;
      }
      .summary-text {
        font-size: 16px;
        text-align: left;
        padding: 10px;
        background-color: #f9f9f9;
        border: 1px solid #ccc;
        border-radius: 5px;
      }
    "))
  ),
  titlePanel(div("Employee Attrition Analysis", class = "main-title")),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Gender", choices = unique(df$Gender), selected = unique(df$Gender), multiple = TRUE),
      selectInput("department", "Department", choices = unique(df$Department), selected = unique(df$Department), multiple = TRUE),
      selectInput("educationField", "Education Field", choices = unique(df$EducationField), selected = unique(df$EducationField), multiple = TRUE),
      selectInput("attrition", "Attrition", choices = unique(df$Attrition), selected = unique(df$Attrition), multiple = TRUE),
      selectInput("jobRole", "Job Role", choices = unique(df$JobRole), selected = unique(df$JobRole), multiple = TRUE),
      selectInput("maritalStatus", "Marital Status", choices = unique(df$MaritalStatus), selected = unique(df$MaritalStatus), multiple = TRUE),
      
      radioButtons(
        inputId = "session",
        label = "Select Analysis Session",
        choices = c("Gender Analysis", "Analysis by Generation and Education", "The Impact of Income towards Attrition", "Working Environment", "An In-Depth Look into Attrition", "Other Factors that could Influence Attrition", "Summary Session"),
        selected = "Gender Analysis"
      ),
      
      uiOutput("plot_selector")
    ),
    
    mainPanel(
      uiOutput("selected_output")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Filter data based on input selections
  filtered_data <- reactive({
    df %>%
      filter(Gender %in% input$gender,
             Department %in% input$department,
             EducationField %in% input$educationField,
             Attrition %in% input$attrition,
             JobRole %in% input$jobRole,
             MaritalStatus %in% input$maritalStatus)
  })
  
  # Dynamic UI for plot selection based on session
  output$plot_selector <- renderUI({
    session_plots <- switch(input$session,
                            "Gender Analysis" = c("Age Distribution", "Job Satisfaction", "Monthly Income", "Employees by Department"),
                            "Analysis by Generation and Education" = c("Companies Worked by Generation", "Attrition by Educational Level"),
                            "The Impact of Income towards Attrition" = c("Income by Department", "Income and Job Satisfaction", "Income and Performance Rating"),
                            "Working Environment" = c("Employees by Role", "Attrition by Role", "Satisfaction by Manager"),
                            "An In-Depth Look into Attrition" = c("Attrition by Department", "Distance from Work"),
                            "Other Factors that could Influence Attrition" = c("Business Travel", "Marital Status"),
                            "Summary Session" = c("Gender Analysis Summary", "Generation and Education Summary", "Income Impact Summary", "Working Environment Summary")
    )
    
    radioButtons(
      inputId = "plot",
      label = "Select Visualization",
      choices = session_plots,
      selected = session_plots[1]
    )
  })
  
  # Render selected plot or summary
  output$selected_output <- renderUI({
    plot_or_text <- switch(input$plot,
                           "Age Distribution" = plotOutput("age_distribution"),
                           "Job Satisfaction" = plotOutput("job_satisfaction"),
                           "Monthly Income" = plotOutput("monthly_income"),
                           "Employees by Department" = plotOutput("employees_by_department"),
                           "Companies Worked by Generation" = plotOutput("companies_worked_generation"),
                           "Attrition by Educational Level" = plotOutput("attrition_educational_level"),
                           "Income by Department" = plotOutput("income_department"),
                           "Income and Job Satisfaction" = plotOutput("income_job_satisfaction"),
                           "Income and Performance Rating" = plotOutput("income_performance_rating"),
                           "Employees by Role" = plotOutput("employees_by_role"),
                           "Attrition by Role" = plotOutput("attrition_by_role"),
                           "Satisfaction by Manager" = plotOutput("satisfaction_manager"),
                           "Attrition by Department" = plotOutput("attrition_department"),
                           "Distance from Work" = plotOutput("distance_work"),
                           "Business Travel" = plotOutput("business_travel"),
                           "Marital Status" = plotOutput("marital_status"),
                           
                           "Gender Analysis Summary" = {
                             plot_text <- "<div class='summary-text'>
                               <h3>Gender Analysis Summary</h3>
                               <ul>
                                 <li>Age: Females average 37.33 years and males 36.65.</li>
                                 <li>Job Satisfaction: Females who left had lower satisfaction than males.</li>
                                 <li>Salaries: Males average $6,380.51 and females $6,686.57.</li>
                                 <li>Departments: Males predominant in three, females in Research and Development.</li>
                               </ul>
                             </div>"
                             HTML(plot_text)
                           },
                           
                           "Generation and Education Summary" = {
                             plot_text <- "<div class='summary-text'>
                               <h3>Generation and Education Summary</h3>
                               <ul>
                                 <li>Boomers had more previous employers.</li>
                                 <li>Millennials show fewer companies due to younger age.</li>
                                 <li>Attrition: Millennials and boomers have highest turnover rates.</li>
                               </ul>
                             </div>"
                             HTML(plot_text)
                           },
                           
                           "Income Impact Summary" = {
                             plot_text <- "<div class='summary-text'>
                               <h3>Income Impact Summary</h3>
                               <ul>
                                 <li>Income by Departments: Significant differences by attrition status.</li>
                                 <li>Job Satisfaction: Lower satisfaction correlates with wider income gaps.</li>
                                 <li>Attrition Sample: Majority had <15% salary increase and <$7,000 monthly income.</li>
                               </ul>
                             </div>"
                             HTML(plot_text)
                           },
                           
                           "Working Environment Summary" = {
                             plot_text <- "<div class='summary-text'>
                               <h3>Working Environment Summary</h3>
                               <ul>
                                 <li>Employees by Role: Sales and Research Scientist have most employees.</li>
                                 <li>Salary by Role: Managers and Research Directors earn highest salaries.</li>
                                 <li>Attrition: Sales Reps, Healthcare Reps, and Managers have highest rates.</li>
                               </ul>
                             </div>"
                             HTML(plot_text)
                           }
    )
    
    plot_or_text
  })
  
  # Render plots
  output$age_distribution <- renderPlot({
    ggplot(filtered_data(), aes(x = Age, fill = Gender)) + 
      geom_density(alpha = 0.6) + 
      labs(title = "Age Distribution by Gender")
  })
  
  output$job_satisfaction <- renderPlot({
    ggplot(filtered_data(), aes(x = JobSatisfaction, fill = Gender)) + 
      geom_bar(position = "dodge", alpha = 0.7) + 
      labs(title = "Distribution of Job Satisfaction by Gender")
  })
  
  output$monthly_income <- renderPlot({
    ggplot(filtered_data(), aes(x = MonthlyIncome, fill = Gender)) + 
      geom_density(alpha = 0.6) + 
      labs(title = "Density Plot of Monthly Income by Gender", x = "Monthly Income", y = "Density")
  })
  
  output$employees_by_department <- renderPlot({
    ggplot(filtered_data(), aes(x = Department, fill = Gender)) + 
      geom_bar(position = "dodge") + 
      labs(title = "Number of Employees by Gender in Each Department")
  })
  
  output$companies_worked_generation <- renderPlot({
    ggplot(filtered_data(), aes(x = Generation, y = NumCompaniesWorked, fill = Generation)) + 
      geom_violin(trim = FALSE, scale = "width") + 
      geom_jitter(aes(color = Generation), width = 0.2, alpha = 0.5) + 
      stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "black", fill = "black") +
      labs(title = "Number of Companies Worked by Generation")
  })
  
  output$attrition_educational_level <- renderPlot({
    ggplot(filtered_data(), aes(x = EducationField, fill = Attrition)) + 
      geom_bar(position = "fill") + 
      labs(title = "Attrition Rate by Education Field")
  })
  
  output$income_department <- renderPlot({
    ggplot(filtered_data(), aes(x = Department, y = MonthlyIncome, fill = Attrition)) + 
      geom_boxplot(alpha = 0.6) + 
      labs(title = "Income by Department and Attrition Status")
  })
  
  output$income_job_satisfaction <- renderPlot({
    ggplot(filtered_data(), aes(x = JobSatisfaction, y = MonthlyIncome, color = Attrition)) + 
      geom_point() + 
      labs(title = "Income and Job Satisfaction by Attrition")
  })
  
  output$income_performance_rating <- renderPlot({
    ggplot(filtered_data(), aes(x = PerformanceRating, y = MonthlyIncome, color = Attrition)) + 
      geom_boxplot() + 
      labs(title = "Income and Performance Rating by Attrition Status")
  })
  
  output$employees_by_role <- renderPlot({
    ggplot(filtered_data(), aes(x = JobRole, fill = JobRole)) + 
      geom_bar() + 
      labs(title = "Number of Employees by Job Role")
  })
  
  output$attrition_by_role <- renderPlot({
    ggplot(filtered_data(), aes(x = JobRole, fill = Attrition)) + 
      geom_bar(position = "fill") + 
      labs(title = "Attrition Percentage by Job Role")
  })
  
  output$satisfaction_manager <- renderPlot({
    ggplot(filtered_data(), aes(x = YearsWithCurrManager, y = JobSatisfaction, color = YearsWithCurrManager)) + 
      geom_point() + 
      labs(title = "Satisfaction by Years with Current Manager")
  })
  
  output$attrition_department <- renderPlot({
    ggplot(filtered_data(), aes(x = Department, fill = Attrition)) + 
      geom_bar() + 
      labs(title = "Attrition Count by Department")
  })
  
  output$distance_work <- renderPlot({
    ggplot(filtered_data(), aes(x = DistanceFromHome, fill = Attrition)) + 
      geom_density(alpha = 0.5) + 
      labs(title = "Distance from Work and Attrition")
  })
  
  output$business_travel <- renderPlot({
    ggplot(filtered_data(), aes(x = BusinessTravel, fill = Attrition)) + 
      geom_bar(position = "fill") + 
      labs(title = "Influence of Business Travel on Attrition")
  })
  
  output$marital_status <- renderPlot({
    ggplot(filtered_data(), aes(x = MaritalStatus, fill = Attrition)) + 
      geom_bar(position = "fill") + 
      labs(title = "Marital Status and Attrition")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
