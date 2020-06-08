library(shiny)




# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  dashboardHeader(title = "Total Cost Simulation"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      shinydashboard::box(plotOutput("plot1", height = 250)),
        #submitButton("Update View", icon("refresh"))

      shinydashboard::box(
        title = "Company Controls",
        sliderInput("numberUsers", "Number of users:", 1, 300, 50),
        sliderInput("workWeek", "Work week:", 1, 52, 50),
        sliderInput("memReq", "Mean memory required (gb):", 1, 1000, 250),
        sliderInput("stdDevMemReq", "Std dev memory required (gb):", 1, 200, 100),
        sliderInput("todWorkWeek", "Tod work week (%):", min = 0, max = 100, post  = " %", value = 40),
        sliderInput("todOffHours", "Tod off hours (%):", min = 0, max = 100, post  = " %", value = 5)
      ),
     shinydashboard::box(title = "Server Controls",
        textInput("hourCostPerServ", "Hour Cost per server ($):", 0.384),
        sliderInput("serverSize", "Server Size (gb):", 1, 200, 32)
      )
    )
  )
))