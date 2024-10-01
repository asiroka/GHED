# Install the necessary packages if not already installed
install.packages(c("shiny", "ggplot2", "dplyr", "plotly", "tidyverse"))

# Load the required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyverse)

# Define the UI
ui <- fluidPage(
  titlePanel("Scatterplot with Year Filters and Tooltips"),
  
  sidebarLayout(
    sidebarPanel(
      # Sliders for filtering years
      sliderInput("yearRange", "Select Year Range:",
                  min = 2000, max = 2022,
                  value = c(2000, 2022),
                  sep = "",           # Remove commas
                  step = 1,           # Step is 1 for finer control
                  ticks = TRUE,      # Turn off default ticks
                  animate = TRUE),   # Turn off animation
      
      # Dropdowns for selecting X and Y axis variables
      selectInput("xVar", "Select X-axis variable:",
                  choices = c("GDP per capita" = "GDP_per_capita", 
                              "Out-of-pocket spending as % of health spending" = "Out_of_pocket_spending", 
                              "Government spending as % of health spending" = "Government_spending", 
                              "Population" = "Population")),
      
      selectInput("yVar", "Select Y-axis variable:",
                  choices = c("GDP per capita" = "GDP_per_capita", 
                              "Out-of-pocket spending as % of health spending" = "Out_of_pocket_spending", 
                              "Government spending as % of health spending" = "Government_spending", 
                              "Population" = "Population")),
    ),
    
    mainPanel(
      # Show the scatter plot
      plotlyOutput("scatterPlot")
    )
  )
)

# Define the server
server <- function(input, output) {
  # Sample data for demonstration (replace with your actual data)
  data <- tibble(
    year = rep(2000:2022, each = 10),
    country = rep(letters[1:10], times = 23),
    GDP_per_capita = runif(230, 1000, 50000),
    Out_of_pocket_spending = runif(230, 0, 50),
    Government_spending = runif(230, 20, 80),
    Population = runif(230, 1e6, 1e9)
  )
  
  # A lookup list to map variable names to user-friendly labels
  variable_labels <- list(
    GDP_per_capita = "GDP per capita",
    Out_of_pocket_spending = "Out-of-pocket spending as % of health spending",
    Government_spending = "Government spending as % of health spending",
    Population = "Population"
  )
  
  # Render the scatter plot
  output$scatterPlot <- renderPlotly({
    # Filter data based on the year range selected
    filtered_data <- data %>%
      filter(year >= input$yearRange[1], year <= input$yearRange[2])
    
    # Get the user-friendly labels for the selected variables
    x_label <- variable_labels[[input$xVar]]
    y_label <- variable_labels[[input$yVar]]
    
    # Create dynamic title based on selected x and y variables
    plot_title <- paste("Scatterplot of", y_label, "vs", x_label)
    
    # Create ggplot scatter plot
    p <- ggplot(filtered_data, aes_string(x = input$xVar, y = input$yVar)) +
      geom_point(aes(text = paste("Country:", country, 
                                  "<br>", x_label, ":", round(filtered_data[[input$xVar]], 2), 
                                  "<br>", y_label, ":", round(filtered_data[[input$yVar]], 2)))) +
      labs(x = x_label, y = y_label) +
      theme_minimal()
    
    # Convert ggplot to plotly and include hover text
    ggplotly(p, tooltip = "text") %>%
      layout(title = list(text = plot_title))  # Only pass the title to layout
  })
}

# Run the app
shinyApp(ui = ui, server = server)
