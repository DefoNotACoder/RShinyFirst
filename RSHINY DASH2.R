library(shiny)
library(ggplot2)
library(readxl)
library(readr)
library(dplyr)
library(scales)

# Define UI
ui <- fluidPage(
  titlePanel("COVID-19 Dashboard"),
  
  fluidRow(
    column(6, plotOutput("linePlot")),
    column(6, plotOutput("barPlot"))
  ),
  
  fluidRow(
    column(12, plotOutput("facetWrapPlot"))
  )
)

# Define server logic
server <- function(input, output) {
  
  # Read the Excel file
  COVID_data <- read_excel("C:/Users/frost/Downloads/COVID-CasesDeathsHosp(Europe and Americas).xlsx")
  
  # Create a column to show month and year
  COVID_data$month <- as.Date(cut(COVID_data$Date, breaks = 'month'))
  
  # Filter data by UK only
  COVID_dataUK <- COVID_data %>% filter(Location == "United Kingdom")
  
  # Group data by month and year with summing of new cases
  COVID_dataUKgrouped <- COVID_dataUK %>% 
    group_by(month) %>% 
    summarize(NewCases = sum(NewCases))
  
  # Line Chart
  output$linePlot <- renderPlot({
    ggplot(COVID_dataUKgrouped, aes(x = month, y = NewCases, label = NewCases)) +
      geom_line(color = "blue", size = 2) +
      scale_y_continuous(labels = comma, breaks = seq(0, 1500000, 100000)) +
      scale_x_date(date_labels = "%b-%y", breaks = "1 month") + 
      theme(axis.text.x = element_text(size = 10, angle = 90)) +  
      labs(x = "Month", y = "New Cases") +
      geom_point(color = "blue", size = 3) +
      geom_text(aes(label = NewCases), vjust = -0.5, hjust = 0, nudge_x = -0.2)
  })
  
  # Bar Chart
  output$barPlot <- renderPlot({
    ggplot(COVID_dataUK, aes(x = month, y = NewCases)) +
      geom_col(fill = "darkblue") +
      scale_y_continuous(labels = comma, breaks = seq(0, 1800000, 100000)) +
      scale_x_date(date_labels = "%b-%y", breaks = "1 month") +
      theme(axis.text.x = element_text(size = 10, angle = 90)) +
      labs(x = "Date", y = "New Cases")
  })
  
  # Read the CSV file
  df <- read_csv("C:/Users/frost/Downloads/COVID Facet Wrap Data.csv")
  
  # Line Graph with facet_wrap
  output$facetWrapPlot <- renderPlot({
    ggplot(df, aes(x = MonthYear, y = NewCases, group = Location)) +
      geom_line(aes(color = Location), size = 2) +
      geom_point(aes(color = Location), size = 3) +
      scale_y_continuous(labels = comma, breaks = seq(0, 10000000, 1000000)) +
      scale_x_date(date_labels = "%b-%y", breaks = "1 month") +
      theme(axis.text.x = element_text(size = 9, angle = 90), axis.line = element_line(colour = "grey")) +
      labs(x = "Month Year", y = "New Cases") +
      facet_wrap(~Location)
  })
}

         
         # Run the application
         shinyApp(ui = ui, server = server)
         