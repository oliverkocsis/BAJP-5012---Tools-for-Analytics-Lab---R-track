
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("complexity",
                  "Complexity",
                  min = 1,
                  max = 16,
                  value = 1),
      selectInput("x",
                  "Variable (X)",
                  choices = names(mtcars),
                  selected = "wt"),
      selectInput("y",
                  "Variable (Y)",
                  choices = names(mtcars),
                  selected = "hp"),
      selectInput("color",
                  "Color",
                  choices = names(mtcars),
                  selected = "am")
      
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("distPlot")),
        tabPanel("GGplot", plotOutput("ggplot")),
        tabPanel("Model", verbatimTextOutput("model")),
        tabPanel("Coefficients", tableOutput("coeff"))
      )
    )
  )
))
