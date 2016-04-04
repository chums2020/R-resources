#data guide: http://data.ntpc.gov.tw/ab/guide
#City activity map
#given day, time, region, the app can look for activities that are going to take place


library(jsonlite)
library(curl)
url = "http://data.ntpc.gov.tw/od/data/api/A97AEE33-4109-457B-9FB1-DB754A0BB100?$format=json"

shinyServer(function(input, output) {
  jsonData <- fromJSON(url)
  names(jsonData) <- c("公告機關","活動類型","開始時間","結束時間","活動名稱","網址","細節","公佈日期")
  
  activityInput <- reactive({
    switch(input$type,
           "Cultural Events Promotion" = which(jsonData$type =="藝文推廣"),
           "exhibition info" = which(jsonData$type == "展覽訊息"),
           "Exhibition and Performing Arts"= which(jsonData$type =="藝術展演"),
           "call for submission/application"= which(jsonData$type == "徵件/比賽/申請"),
           "ticket/registration"= which(jsonData$type =="索票/報名"),
           "street artist"= which(jsonData$type =="街頭藝人"),
           "community"= which(jsonData$type =="社區營造"),
           "cultural development"= which(jsonData$type =="文化發展"),
           "null"= dim(jsonData)[1]
    )
  })

  adjust_dateInput <- reactive({
    if (input$period){
      return(seq.Date(as.Date(input$date)-7, as.Date(input$date)+7, by = "day"))
    }
    as.Date(input$date)
  })

  
  output$table <- renderTable({
    jsonData <- jsonData[activityInput(),]
    jsonData <- jsonData[which(jsonData$startdata < max(adjust_dateInput())),]
    jsonData <- jsonData[which(jsonData$enddata > min(adjust_dateInput())),]
  })
})

