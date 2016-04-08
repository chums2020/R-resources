#data guide: http://data.ntpc.gov.tw/ab/guide
#City activity map
#given day, time, region, the app can look for activities that are going to take place


#active url link
#transform startdata, enddata


library(jsonlite)
library(curl)
library(DT)
url = "http://data.ntpc.gov.tw/od/data/api/A97AEE33-4109-457B-9FB1-DB754A0BB100?$format=json"

shinyServer(function(input, output) {
  jsonData <- fromJSON(url)
  jsonData[,6] <- paste0("<a href='",jsonData[,6],"'target='_blank'>",jsonData[,6],"</a>")
  activityInput <- reactive({
    switch(input$type,
           "藝文推廣" = which(jsonData$type =="藝文推廣"),
           "展覽訊息" = which(jsonData$type == "展覽訊息"),
           "藝術展演"= which(jsonData$type =="藝術展演"),
           "徵件/比賽/申請"= which(jsonData$type == "徵件/比賽/申請"),
           "索票/報名"= which(jsonData$type =="索票/報名"),
           "街頭藝人"= which(jsonData$type =="街頭藝人"),
           "社區營造"= which(jsonData$type =="社區營造"),
           "文化發展"= which(jsonData$type =="文化發展"),
           "全部"= seq(1,dim(jsonData)[1])
    )
  })

  
  
  output$table <- DT::renderDataTable({
    jsonData <- jsonData[activityInput(),1:7]
    jsonData <- jsonData[which(as.Date(jsonData$startdata) < as.Date(input$dates[2])),]
    jsonData <- jsonData[which(as.Date(jsonData$enddata) > as.Date(input$dates[1])),]    
    names(jsonData) <- c("公告機關","活動類型","開始時間","結束時間","活動名稱","網址","細節")
    jsonData <-data.frame(jsonData)
    DT::datatable(jsonData,rownames = FALSE, escape =FALSE)
  })
})
