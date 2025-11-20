# 文件名: poi_function.R
# 这是一个独立的R脚本，用于存放核心函数 (修正版)

get_poi_data <- function(keywords, city, api_key) {
  library(httr)
  library(jsonlite)
  library(dplyr)
  
  all_pois <- list()
  page <- 1
  cat("开始获取数据...\n")
  
  repeat {
    # 1. 构造请求URL
    url <- sprintf(
      "https://restapi.amap.com/v3/place/text?key=%s&keywords=%s&city=%s&offset=20&page=%d&extensions=all",
      api_key, keywords, city, page
    )
    
    # 2. 发送GET请求
    response <- tryCatch({
      GET(url)
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(response)) {
      stop("网络请求失败，请检查网络连接。")
    }
    
    # 3. 解析JSON内容
    content <- content(response, "text", encoding = "UTF-8")
    data <- fromJSON(content)
    
    # 4. 检查请求是否成功
    if (data$status != "1") {
      stop(paste("API请求失败:", data$info))
    }
    
    # 5. 获取当前页的POI列表
    pois <- data$pois
    if (length(pois) == 0) {
      break
    }
    
    # 6. 【核心修正】安全地提取信息，处理字段格式不一致的问题
    poi_subset <- data.frame(
      名称 = sapply(pois$name, function(x) ifelse(is.null(x), NA, x)),
      类型 = sapply(pois$type, function(x) ifelse(is.null(x), NA, x)),
      地址 = sapply(pois$address, function(x) ifelse(is.null(x), NA, x)),
      电话 = sapply(pois$tel, function(x) ifelse(is.null(x), NA, x)),
      经度 = sapply(strsplit(pois$location, ","), function(x) x[1]),
      纬度 = sapply(strsplit(pois$location, ","), function(x) x[2]),
      stringsAsFactors = FALSE
    )
    
    # 将当前页数据添加到总列表
    all_pois[[page]] <- poi_subset
    
    cat("第", page, "页数据获取成功。\n")
    page <- page + 1
  }
  
  # 7. 合并所有数据并返回
  final_poi_df <- bind_rows(all_pois)
  cat("所有数据获取完成！共", nrow(final_poi_df), "条记录。\n")
  return(final_poi_df)
}
