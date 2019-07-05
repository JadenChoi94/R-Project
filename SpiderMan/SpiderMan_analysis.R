#일자별/시간대별 평점 분석을 할 것
library(ggthemes)
library(ggplot2)
library(tidyverse)
library(nycflights13)
library(dplyr)
library(lubridate)
library(plotly)
setwd('D:/Workspace/R-Project/SpiderMan')
data<-read.csv('SpiderMan_review.csv')
str(data)
#Lubridate
data2<-data%>%
  select(datetime, score) %>%
  mutate(date = substr(data$datetime,6, 10)) %>%
  mutate(time = substr(data$datetime,12, 13)) 


data3<-data2%>%
  group_by(date) %>%
  summarise(average=mean(score))

date_review<-ggplot(data3, aes(x=date, y=average)) +
  labs(x='날짜', y='평균 평점') +
  geom_bar(width=0.5, stat='identity') +
  ggtitle('스파이더맨 날짜별 평균 평점') +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"))
ggplotly(date_review)

data3<-data2[,-1]
perhour<-data3 %>% 
  group_by(time) %>% 
  summarise(avr=round(mean(score),1)) %>% 
  arrange(time)

hour_review<-ggplot(perhour, aes(x=time, y=avr))+
  labs(x='시간대', y='평균 평점') +
  geom_bar(width=0.5, stat='identity') +
  ggtitle('스파이더맨 시간대별 평균 평점') +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "darkblue"))
ggplotly(hour_review)

#기타
# morning_review<-data2 %>%
#   filter(time<='12:00') %>% 
#   summarise(average=mean(score))
# 
# night_review<-data2%>%
#   filter(time>'12:00' & time <='24:00') %>% 
#   summarise(average=mean(score))
# 평점 낮게 준사람들 키워드 워드 클라우드