# 完整正确的 app.R - 星图地球版本
library(shiny)
library(DT)
library(leaflet)

source("poi_function.R")

ui <- fluidPage(
  titlePanel("POI搜索与下载工具 - 星图地球版"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("搜索参数"),
      textInput("keywords", "关键词:", value = "酒店"),
      textInput("city", "城市:", value = "北京市"),
      actionButton("search_btn", "开始搜索", class = "btn-primary"),
      br(), br(),
      downloadButton("download_btn", "下载CSV文件", class = "btn-success")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("数据表格",
                 verbatimTextOutput("status_text"),
                 DTOutput("poi_table")
        ),
        tabPanel("地图显示",
                 leafletOutput("map", height = "600px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  poi_data <- reactiveVal(NULL)
  
  output$status_text <- renderText({"请输入参数并点击搜索"})
  
  observeEvent(input$search_btn, {
    if (input$keywords == "" || input$city == "") {
      output$status_text <- renderText({"错误：请填写关键词和城市！"})
      return()
    }
    
    output$status_text <- renderText({"正在搜索..."})
    
    result <- tryCatch({
      my_api_key <- "8feeabfecbfe183c158c8d5f5b4842c5"
      data <- get_poi_data(input$keywords, input$city, my_api_key)
      data
    }, error = function(e) {
      return(paste("错误:", e$message))
    })
    
    if (is.character(result)) {
      output$status_text <- renderText({result})
      poi_data(NULL)
    } else {
      output$status_text <- renderText({paste("完成！共", nrow(result), "条数据")})
      poi_data(result)
    }
  })
  
  output$poi_table <- renderDT({
    req(poi_data())
    datatable(poi_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$map <- renderLeaflet({
    req(poi_data())
    data <- poi_data()
    data$经度 <- as.numeric(data$经度)
    data$纬度 <- as.numeric(data$纬度)
    
    # 使用星图地球地图源
    leaflet(data) %>%
      addTiles(
        urlTemplate = "https://tiles.gooblog.cn/earth/{z}/{x}/{y}.png",
        attribution = '© 星图地球'
      ) %>%
      addMarkers(
        lng = ~经度, lat = ~纬度,
        popup = ~paste("<b>", 名称, "</b><br>", 地址)
      )
  })
  
  output$download_btn <- downloadHandler(
    filename = function() {
      paste0(input$keywords, "_", input$city, "_POI数据.csv")
    },
    content = function(file) {
      req(poi_data())
      write.csv(poi_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui = ui, server = server)