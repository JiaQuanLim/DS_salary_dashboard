library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(scales)  

data <- read.csv("../data/raw/Salary_Data.csv") |> 
  na.omit() |> filter(Education.Level != "") |> 
  mutate(Education.Level = case_when(
    Education.Level == "Bachelor's" ~ "Bachelor's Degree",
    Education.Level == "Master's" ~ "Master's Degree",
    Education.Level == "phD" ~ "PhD",
    TRUE ~ Education.Level
  ), Salary = round(Salary * 0.01635))

# Layout
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f4f4f4; 
        font-family: Arial, sans-serif;
      }

      .title {
        text-align: center;
        font-size: 28px;
        font-weight: bold;
        padding: 15px;
      }

      .sidebar {
        background-color: #ddeeff; 
        padding: 15px;
        border-radius: 10px;
      }

      .plot-container {
        background-color: white;
        padding: 15px;
        border-radius: 10px;
        box-shadow: 2px 2px 10px rgba(0,0,0,0.1);
        margin-bottom: 20px; 
      }

      .plot-title {
        font-size: 18px;
        font-weight: bold;
        text-align: center;
        margin-bottom: 10px;
      }

      .stat-card {
        background-color: white;
        padding: 15px;
        border-radius: 10px;
        box-shadow: 2px 2px 10px rgba(0,0,0,0.1);
        text-align: center;
        font-size: 16px;
        font-weight: bold;
        margin-bottom: 15px;
      }

      .stat-section {
        margin-top: 20px;
        padding: 10px;
      }
      
      .sidebar h4,
      .stat-section h4 {
        font-size: 22px;
        font-weight: bold;
        text-align: center;
      }
    "))
  ),

  div(class = "title", "Salary Board - Know Your Worth"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class = "sidebar",
        h4("Filters"),
        sliderInput("ageInput", "Age Range:",
                    min = min(data$Age),
                    max = max(data$Age),
                    value = c(min(data$Age), max(data$Age)),
                    step = 1, ticks = FALSE),
        selectInput("genderInput", "Gender:", 
                    choices = c("All", unique(data$Gender)), selected = "All"),
        selectInput("jobInput", "Job Title:", 
                    choices = c("All", sort(unique(data$Job.Title))), selected = "All")
      ),
      
      div(class = "stat-section",
          h4("Salary Statistics"),
          div(class = "stat-card", h5("Minimum Salary"), textOutput("minSalary")),
          div(class = "stat-card", h5("Average Salary"), textOutput("avgSalary")),
          div(class = "stat-card", h5("Maximum Salary"), textOutput("maxSalary"))
      )
    ),
    
    mainPanel(
      fluidRow(
        div(class = "plot-container",
            h4(class = "plot-title", "Monthly Salary Distribution"),
            plotlyOutput("salaryPlot", height = "300px")
        ),
        div(class = "plot-container",
            h4(class = "plot-title", "Average Salary by Education Level"),
            plotlyOutput("educationSalaryPlot", height = "300px")
        )
      )
    )
  )
)


# Server side callbacks/reactivity
server <- function(input, output) {
  
  filtered_data <- reactive({
    data |>
      filter(Age >= input$ageInput[1] & Age <= input$ageInput[2],
             (Gender == input$genderInput | input$genderInput == "All"),
             (Job.Title == input$jobInput | input$jobInput == "All"))
  })
  
  # Salary histogram plot
  output$salaryPlot <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      ggplot() +
        annotate("text", x = 1, y = 1, label = "No data available for selected filters", size = 6, hjust = 0.5) +
        theme_void()
    } else {
      plot_salary_dist <- df |> 
        ggplot(aes(x = Salary)) +
        geom_histogram(binwidth = 70, fill = "skyblue", color = "black") +
        labs(x = "Monthly Salary (CAD)", y = "Count") +
        theme_minimal() +
        scale_x_continuous(labels = comma) +
        theme(text = element_text(size = 12))  
      
      plot_salary_dist <- ggplotly(plot_salary_dist, tooltip = "text") |>  
        layout(xaxis=list(fixedrange=TRUE)) |> 
        layout(yaxis=list(fixedrange=TRUE, title=list(standoff=15)))
    }
  })
  
  # Education Level vs. Salary bar plot
  output$educationSalaryPlot <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      ggplot() +
        annotate("text", x = 1, y = 1, label = "No data available for selected filters", size = 6, hjust = 0.5) +
        theme_void()
    } else {
      plot_edu_salary <- df |>
        group_by(Education.Level) |>
        summarize(average_salary = mean(Salary, na.rm = TRUE)) |>
        ggplot(aes(x = reorder(Education.Level, -average_salary), y = average_salary, fill = Education.Level, text = paste0("Avg Salary: $", formatC(average_salary, format = "f", big.mark = ",", digits = 0)))) +
        geom_bar(stat = "identity") +
        labs(x = "Education Level", y = "Average Monthly Salary (CAD)") +
        theme_minimal() +
        scale_y_continuous(labels = scales::comma) +
        theme(text = element_text(size = 12), legend.position = "none") +
        coord_flip()
      
      plot_edu_salary <- ggplotly(plot_edu_salary, tooltip = "text") |>  
        layout(xaxis=list(fixedrange=TRUE)) |> 
        layout(yaxis=list(fixedrange=TRUE, title=list(standoff=15)))
    }
  })
  
  # Salary Stats
  output$minSalary <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("No Data")
    paste0("$", formatC(min(df$Salary), format = "f", big.mark = ",", digits = 0))
  })
  
  output$avgSalary <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("No Data")
    paste0("$", formatC(mean(df$Salary, na.rm = TRUE), format = "f", big.mark = ",", digits = 0))
  })
  
  output$maxSalary <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("No Data")
    paste0("$", formatC(max(df$Salary), format = "f", big.mark = ",", digits = 0))
  })
}

# Run the app/dashboard
shinyApp(ui = ui, server = server)
