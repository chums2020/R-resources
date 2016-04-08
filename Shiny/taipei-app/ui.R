library(shiny)

shinyUI(fluidPage(
  titlePanel("新北市政府文化局藝文活動 （每日更新）"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("輸入活動類型與日期後，右方表格會自動更新"),
      
      radioButtons("type", label = h3("請選擇您感興趣的活動類型："),
                   choices = list("藝文推廣", "藝術展演", "徵件/比賽/申請",
                                  "索票/報名", "街頭藝人", "社區營造", "文化發展", "全部"),
                   selected = "全部"),
      
      dateInput("date", 
                label = h3("請選擇日期"), 
                value = as.character(Sys.Date())),
      
      br(),
      br(),
      
      checkboxInput("period", "彈性時間 (包含選擇日期的前後7天)", 
                    value = TRUE)
    ),
    
    mainPanel(
       DT::dataTableOutput('table')
      
    )
  )
))
