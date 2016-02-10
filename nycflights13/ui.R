
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("model.R")

shinyUI(fluidPage(

  # Application title
  titlePanel("Airline on-time data for all flights departing NYC in 2013"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date",
                "Date",
                min = min(flights$date),
                max = max(flights$date),
                start = mean(flights$date) - 1,
                end = mean(flights$date) + 1),
      sliderInput("distance",
                  "Distance (miles)",
                  min = min(flights$distance),
                  max = max(flights$distance),
                  value = max(flights$distance))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Plots", 
                 h2("Diatance and Airtime"), plotOutput("distance_airtime"),
                 h2("Distance and Arrival Delay"), plotOutput("distance_arr_delay")),
        tabPanel("Boxplots", 
                 h2("Distances"), plotOutput("distance"),
                 h2("Air Times"), plotOutput("air_time"),
                 h2("Arrival Delays"), plotOutput("arr_dellay")),
        tabPanel("Tables", 
                 h2("Carriers"), dataTableOutput("carriers"),
                 h2("Origins"), dataTableOutput("origins"),
                 h2("Destinationss"), dataTableOutput("dests"))
      )
    )
  )
))
