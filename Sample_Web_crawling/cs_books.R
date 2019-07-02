# 한빛아카데미 도서 크롤링
install.packages('rvest')
library(rvest)
library(dplyr)
library(stringr)

trim<-function(x) gsub('^\\s+|\\s+$', '', x)

base_url<-'http://www.hanbit.co.kr/academy/books/category_list.html?'
page<-'page='
category<-'cate_cd=004007&srt=p_pup_date'
computer_books<-data.frame(title=c(), writer=c(), price=c())
for(i in 1:6){
  url<-paste0(base_url, page, 'i', category)
  html<-read_html(base_url)
  #html
  #book_list<- html_node(html, '.sub_book_list_area')
  #lis <-html_nodes(book_list, 'li')
  
  html %>%
    html_node('.sub_book_list_area') %>%
    html_nodes('li') ->lis
  lis
  
  price<-c()
  title<-c()
  writer<-c()
  
  for (li in lis) {
    pr <- html_node(li, '.price') %>% html_text()
    pr <- gsub('\\\\','', pr)
    price<-c(price, pr)
    title <-c(title, html_node(li, '.book_tit') %>% html_text())
    writer <-c(writer, html_node(li, '.book_writer') %>% html_text())
    #cat(title, writer, price, '\n')
  }
  books <- data.frame(title=title, writer=writer, price=price)
  computer_books<- rbind.data.frame(computer_books, books)
}
computer_books
