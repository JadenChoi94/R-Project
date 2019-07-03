library(rvest)
library(dplyr)
library(stringr)
library(openxlsx)
trim <- function(x) gsub("^\\s+|\\s+$", "", x)
news <- createWorkbook()

cat_names = c('정치','경제','사회','생활_문화','세계','IT_과학')
base_url <- 'https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&sectionId='
category<-c(100:105)
dates <- c('&date=20190703')

#dfs=data.frame(headline=c(), content=c(), newspaper=c())
for(i in 1:6){
  headline <- c()
  content <- c()
  newspaper <- c()
  url <- paste0(base_url, category[i], dates)
  html <- read_html(url)
  
  html %>%
    html_nodes('.ranking_headline') %>% html_text ->hl
  trim(hl)->hl
  headline <- c(headline, hl)
    
  html %>%
    html_nodes('.ranking_lede') %>% html_text ->ct
  trim(ct)->ct
  content <- c(content, ct)
      
  html %>%
    html_nodes('.ranking_office') %>% html_text ->np
  newspaper <- c(newspaper, np) 
  
  rank_news<-data.frame(headline=headline, content=content, newspaper=newspaper)
  #dfs<-rbind.data.frame(dfs, rank_news)
  addWorksheet(news, cat_names[i])
  writeDataTable(news, cat_names[i], rank_news)
}
saveWorkbook(news, file="D:/Workspace/R-Project/TopNews_each.xlsx")
