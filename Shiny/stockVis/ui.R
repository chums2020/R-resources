
library(shiny)

shinyUI(fluidPage(
  titlePanel("stockVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Type a stock symbol.
        Information will be collected from Yahoo Finance."),
    
      textInput("symb", "Symbol", "YHOO"),
    
      dateRangeInput("dates", 
        "Date range",
        start = "2013-01-01", 
        end = as.character(Sys.Date())),
      
      br(),
      br(),
      
      checkboxInput("log", "Plot y axis on log scale", 
        value = FALSE),
      
      checkboxInput("adjust", 
        "Adjust prices for inflation", value = TRUE)
    ),
    
    mainPanel(plotOutput("plot"))
  )
))
