
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(data.table)
library(ggplot2)
source("model.R")

shinyServer(function(input, output) {
  
  getFlights <- reactive({
    flights[date >= input$date[1] & date <= input$date[2] & distance <= input$distance]
  })
  
  output$distance_airtime <- renderPlot({
    ggplot(getFlights(), aes(distance, air_time)) + 
      geom_point(aes(color = carrier)) + 
      theme_minimal()
  })
  
  output$distance_arr_delay <- renderPlot({
    ggplot(getFlights(), aes(distance, arr_delay)) + 
      geom_point(aes(color = carrier)) + 
      theme_minimal()
  })
  
  output$distance <- renderPlot({
    ggplot(getFlights(), aes(carrier, distance)) + 
      geom_boxplot() +
      theme_minimal()
  })
  
  output$air_time <- renderPlot({
    ggplot(getFlights(), aes(carrier, air_time)) + 
      geom_boxplot() +
      theme_minimal()
  })
  
  output$arr_dellay <- renderPlot({
    ggplot(getFlights(), aes(carrier, arr_delay)) + 
      geom_boxplot() +
      theme_minimal()
  })
  
  output$carriers <- renderDataTable({
    getFlights()[, .( Count = .N, "Average Distance" = mean(distance, na.rm = TRUE), "Average Air Time" = mean(air_time, na.rm = TRUE), "Average Delay" = mean(arr_delay, na.rm = TRUE), "Max Delay" = max(arr_delay, na.rm = TRUE)), by = carrier.name]
  })
  
  output$origins <- renderDataTable({
    getFlights()[, .( Count = .N, "Average Distance" = mean(distance, na.rm = TRUE), "Average Air Time" = mean(air_time, na.rm = TRUE), "Average Delay" = mean(arr_delay, na.rm = TRUE), "Max Delay" = max(arr_delay, na.rm = TRUE)), by = origin.name]
  })
  
  output$dests <- renderDataTable({
    flights[date == input$date & distance <= input$distance, .( Count = .N, "Average Distance" = mean(distance, na.rm = TRUE), "Average Air Time" = mean(air_time, na.rm = TRUE), "Average Delay" = mean(arr_delay, na.rm = TRUE), "Max Delay" = max(arr_delay, na.rm = TRUE)), by = dest.name]
  })
})
