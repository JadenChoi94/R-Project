#일자별/시간대별 평점 분석을 할 것
library(lawstat)
library(ggthemes)
library(KoNLP)
library(extrafont)
library(ggplot2)
library(tidyverse)
library(nycflights13)
library(dplyr)
library(lubridate)
library(plotly)
windowsFonts()
windowsFonts(malgun = "맑은 고딕")
theme_update(text = element_text(family = "malgun"))

setwd('D:/Workspace/R-Project/SpiderMan')
data<-read.csv('SpiderMan_review.csv')

data2<-data%>%
  select(datetime, score) %>%
  mutate(date = substr(data$datetime,6, 10)) %>%
  mutate(time = substr(data$datetime,12, 13))

data3<-data2 %>% 
  group_by(date) %>%
  summarise(average=mean(score)) 
  
date_review<-ggplot(data3, aes(x=date, y=average, fill=average)) +
  labs(x='날짜', y='평균 평점') +
  geom_bar(width=0.5, stat='identity') +
  geom_hline(yintercept=seq(0,8,2.5), lty='dotted', size=0.1)+
  ggtitle('스파이더맨 날짜별 평균 평점') +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "red3"))
ggplotly(date_review)

hour_review<-data2[,-1] %>% 
  group_by(time) %>% 
  summarise(avr=round(mean(score),1)) %>% 
  arrange(time) %>% 
  ggplot(aes(x=time, y=avr, group=1))+
  labs(x='시간대', y='평균 평점') +
  geom_line(arrow=arrow(), size=2, colour='orange') +
  geom_point(size=3, shape=19, colour="darkred")+
  geom_hline(yintercept=seq(6,8,1), lty='dotted', size=0.1)+
  ggtitle('스파이더맨 시간대별 평균 평점') +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, 
                                  size = 15, color = "red3"))

ggplotly(hour_review)
#=================================================================================
#기타
#평점별로 얼마나 긴 평가를 남기는지 살펴보기 위해, 평점별 영화평 글자수의 box-plot을 그려보았습니다.
num_cha<-as.character(data$review)
scorenchar<-data %>% 
  select(score) %>% 
  mutate(nchar=nchar(num_cha))

ggplot(scorenchar, aes(factor(score), nchar))+
  geom_boxplot()
# 높은 평점라고 더 긴 평을 남기는건 아니고, 낮은 평점이라고 짧은 평을 남기는건 아닌거 같습니다. 
# 오히려 10점은 영화평이 매우 짧았고, 낮은 점수를 준사람이 영화에 대해 할말이 많았던 것 같습니다.
# 아마도 만족한 소비자는 굳이 영화에 대해 이리저리 설명할 필요성을 못 느끼는것 같다. 반면, 불만족하거나 
# 뭔가 아쉬웠던 사람은 이래저래 영화에 대해 할말이 많았던 것으로 보입니다.

#가설검정
#H0: 평점이 높으면 글자수도 많다. vs H1: 그렇지 않다. 
qqnorm(scorenchar$score)
qqnorm(scorenchar$nchar)
#둘다 정규성을 띄지않는다.
levene.test(scorenchar$score, scorenchar$nchar)
#등분산도 나타내지 않는다.

#만약 두집단 모두 정규성을 나타내고 등분산을 따른다고 가정하면
wow <- lm(score~nchar, data=scorenchar)
anova(wow)
#p-value 가 매우 작습니다. 영가설을 기각합니다. 

cov(scorenchar$score, scorenchar$nchar)#공분산
cor(scorenchar$score, scorenchar$nchar)#상관계수 -0.139
par(mfrow=c(1, 1), mar=c(5, 4, 1, 1))
plot(nchar~score, pch=16, data=scorenchar, xlab="평점", ylab="글자수")
abline(lm(nchar~score, data=scorenchar), col="red", lwd=2)
#두 변수는 연관이 없어 보입니다. 