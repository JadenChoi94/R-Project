#KFC delibery menu
library(rvest)
library(dplyr)
library(stringr)
library(openxlsx)

cat_names = c('추천메뉴','치킨세트', '버거세트', '스낵_사이드', '음료')
categories = c('recommend','chicken', 'burger', 'snack', 'drink')

base_url <- 'https://www.kfckorea.com/delivery/'

ch <- createWorkbook()

  df_menu <- data.frame(name=c(), msg=c(), price=c())
  print(1)

  url <- paste0(base_url, categories[1])
  html <- read_html(url)

  html %>%
    html_nodes('ul') %>%
    html_nodes('li') ->lis
  lis

  price <- c()
  name <- c()
  msg <- c()
    for (li in lis) {
      pr <- html_node(li, '.price') %>% html_text()
      pr <- gsub("\\\\", "", pr)
      price <- c(price, pr)
      name <- c(name, html_text(li, 'h3') %>% html_text())
      msg <- c(msg, html_node(li, '.msg') %>% html_text())
    }
  
  menu <- data.frame(name=name, msg=msg, price=price)
  df_menu <- rbind.data.frame(df_menu, menu)

#   addWorksheet(ch, cat_names[cat_no])
#   writeDataTable(ch, cat_names[cat_no], df_menu)
# }
# 
# saveWorkbook(ch, file="D:/Workspace/R_Project/kfc_delibery_menu.xlsx")
