library(shiny)

shinyUI(fluidPage(
  titlePanel("Cultural Actities in New Taipei City, updated daily"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select type of activities and date."),
      
      radioButtons("type", label = h3("Please choose the type of activities you are interested in."),
                   choices = list("Cultural Events Promotion", "exhibition info","Exhibition and Performing Arts", "call for submission/application",
                                  "ticket/registration", "street artist", "community", "cultural development", "null"),
                   selected = "null"),
      
      dateInput("date", 
                label = h3("Please choose the date of your trip."), 
                value = as.character(Sys.Date())),
      
      br(),
      br(),
      
      checkboxInput("period", "Do you allow for flexible dates? (+/- 7 days)", 
                    value = TRUE)
    ),
    
    mainPanel(
      tableOutput('table')
      
    )
  )
))