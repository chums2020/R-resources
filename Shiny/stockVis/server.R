
library(quantmod)
source("helpers.R")

shinyServer(function(input, output) {
  dataInput <- reactive({
    getSymbols(input$symb, src = "yahoo", 
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  adjust_dataInput <- reactive({
    if (input$adjust){
      return(adjust(dataInput()))
    }
    dataInput()
  })
  output$plot <- renderPlot({
    chartSeries(adjust_dataInput(), theme = chartTheme("white"), 
      type = "line", log.scale = input$log, TA = NULL)
  })
  
})
