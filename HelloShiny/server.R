
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({
    plot(mtcars[,input$x], mtcars[,input$y], col = mtcars[,input$color] + 1)
    fit <- lm(mtcars[,input$y] ~ poly(mtcars[,input$x], degree = input$complexity))
    predict <- data.frame(x = mtcars[,input$x], y = predict(fit))
    points(predict$x, predict$y, col = "orange")
  })
  
  output$ggplot <- renderPlot({
    ggplot(mtcars, aes_string(input$x, input$y)) + 
      geom_point(aes_string(color = input$color)) + 
      geom_smooth(method = "lm", formula = y ~ poly(x, input$complexity), se = FALSE)
  })
  
  output$model <- renderPrint({
    fit <- lm(mtcars[,input$y] ~ poly(mtcars[,input$x], degree = input$complexity))
    summary(fit)
  })
  
  output$coeff <- renderTable({
    fit <- lm(mtcars[,input$y] ~ poly(mtcars[,input$x], degree = input$complexity))
    summary(fit)$coeff
  })
})

