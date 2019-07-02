#IT/과학 뉴스 1위~30위
library(rvest)
library(dplyr)
library(stringr)
library(openxlsx)

tar<-'https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&sectionId=105&date=20190702'
read_html(tar)
news <- createWorkbook()
headline <- c()
content <- c()
newspaper <- c()
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

read_html(tar) %>%
  html_nodes('.ranking_headline') %>% html_text ->headline
trim(headline)
read_html(tar) %>%
  html_nodes('.ranking_lede') %>% html_text ->content
trim(content)
read_html(tar) %>%
  html_nodes('.ranking_office') %>% html_text ->newspaper

ranking <- data.frame(headline=headline, content=content, newspaper=newspaper)
ranking

news
