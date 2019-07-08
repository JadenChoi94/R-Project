#SpiderMan Far from home, crawling
setwd('D:/Workspace/R-Project/SpiderMan')
library(rvest)
library(stringr)
library(dplyr)
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

main_url<-'https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=173123&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page='
html<-read_html(main_url)
tmp<-html%>%
  html_node('.score_total') %>% 
  html_text('em') %>% 
  trim()

str_locate(tmp, "총")
pages <- str_sub(tmp, 9, -2)
pages<-gsub(',', '', pages)
end_page<-ceiling(as.numeric(pages)/10)
#데이터를 저장할 데이터프레임 초기화
dfs <- data.frame(score=c(), review=c(), writer=c(), datetime=c())

for(total_pages in 1:end_page){
  if(total_pages %% 100 == 0)
    print(total_pages)
  url <- paste0(main_url, total_pages)
  html <- read_html(url) %>%
    html_node('div.score_result') %>%
    html_nodes('li') -> lis

   score <- c()
   review <- c()
   writer <- c()
   datetime <- c()
  
    for (li in lis) {
      score <- c(score, html_node(li,'.star_score') %>% 
                html_text('em') %>% trim())
      li %>%
        html_node('.score_reple') %>%
        html_text('p') %>%
        trim() -> comments
      
      idx <- str_locate(comments, "\r")
      review <- c(review, str_sub(comments, 1, idx[1]-1))
      
      comments <- trim(str_sub(comments, idx[1], -1))
      idx <- str_locate(comments, "\r")
      writer <- c(writer, str_sub(comments, 1, idx[1]-1))
      
      comments <- trim(str_sub(comments, idx[1], -1))
      idx <- str_locate(comments, "\r")
      datetime <- c(datetime, str_sub(comments, 1, idx[1]-1))
    }
  total_review <- data.frame(score=score, review=review, writer=writer, datetime=datetime)
  dfs <- rbind.data.frame(dfs, total_review)
}
write.csv(dfs, "SpiderMan_review.csv" , row.names = FALSE)
