
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
source("model.R")

shinyServer(function(input, output) {
  
  output$distance_airtime <- renderPlot({
    ggplot(flights[date >= input$date[1] & date <= input$date[2] & distance <= input$distance], aes(distance, air_time)) + 
      geom_point(aes(color = carrier)) + 
      theme_minimal()
  })
  
  output$distance_arr_delay <- renderPlot({
    ggplot(flights[date >= input$date[1] & date <= input$date[2] & distance <= input$distance], aes(distance, arr_delay)) + 
      geom_point(aes(color = carrier)) + 
      theme_minimal()
  })
  
  output$distance <- renderPlot({
    ggplot(flights[date == input$date & distance <= input$distance], aes(carrier, distance)) + 
      geom_boxplot() +
      theme_minimal()
  })
  
  output$air_time <- renderPlot({
    ggplot(flights[date == input$date & distance <= input$distance], aes(carrier, air_time)) + 
      geom_boxplot() +
      theme_minimal()
  })
  
  output$arr_dellay <- renderPlot({
    ggplot(flights[date == input$date & distance <= input$distance], aes(carrier, arr_delay)) + 
      geom_boxplot() +
      theme_minimal()
  })
  
  output$carriers <- renderTable({
    flights[date == input$date & distance <= input$distance, .( Count = .N, "Average Distance" = mean(distance, na.rm = TRUE), "Average Air Time" = mean(air_time, na.rm = TRUE), "Average Delay" = mean(arr_delay, na.rm = TRUE), "Max Delay" = max(arr_delay, na.rm = TRUE)), by = carrier]
  })
})
