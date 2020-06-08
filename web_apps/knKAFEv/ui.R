library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Total Cost Simulation"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
        sliderInput("numberUsers", "Number of users:", 1, 1000, 50),
        sliderInput("workWeek", "Work week:", 1, 168, 40),
        sliderInput("memReq", "Mean memory required (gb):", 1, 500, 10),
        sliderInput("stdDevMemReq", "Std dev memory required (gb):", 1, 500, 20),
        sliderInput("todWorkWeek", "Tod work week (%):", min = 0, max = 100, post  = " %", value = 40),
        sliderInput("todOffHours", "Tod off hours (%):", min = 0, max = 100, post  = " %", value = 5),
        textInput("hourCostPerServ", "Hour Cost per server ($):", 0.384),
        textInput("serverSize", "Server Size (gb):", 32)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      verbatimTextOutput("tc"),
      verbatimTextOutput("max_servers"),
      verbatimTextOutput("mean_servers"),
      verbatimTextOutput("min_servers"),
      verbatimTextOutput("max_memory"),
      verbatimTextOutput("mean_memory"),
      verbatimTextOutput("median_memory"),
      verbatimTextOutput("min_memory")
    )
  )
))