#일자별/시간대별 평점 분석을 할 것
library(ggplot2)
library(dplyr)
setwd('D:/Workspace/R-Project/SpiderMan')
data<-read.csv('SpiderMan_review.csv')
str(data)
#Lubridate

data2<-cbind(data, perday = substr(data$time,6, 10))
data3<-cbind(data2, time2 = substr(data2$time,12, 17))
data4<-select(data3, score, perday, time2)

eachday<-data4 %>%
  group_by(perday)%>%
  summarise(average=mean(score))


str(data4)


# hist(data4$time2)
# ggplot(data3,aes(x=time))+
#   geom_histogram(aes(y=score),stat='bin')
