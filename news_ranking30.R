#IT/과학 뉴스 1위~30위
library(rvest)
library(dplyr)
library(stringr)
library(openxlsx)

tar<-'https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&sectionId=105&date=20190702'
read_html(tar)

headline <- c()
content <- c()
newspaper <- c()
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

read_html(tar) %>%
  html_nodes('.ranking_headline') %>% html_text ->hl
trim(hl)->headline

read_html(tar) %>%
  html_nodes('.ranking_lede') %>% html_text ->ct
trim(ct)->content

read_html(tar) %>%
  html_nodes('.ranking_office') %>% html_text ->newspaper

ranking <- data.frame(headline=headline, content=content, newspaper=newspaper)
setwd("D:/Workspace/R-Project")
write.csv(ranking, "ITnews_top30.csv", row.names = FALSE)

