#WordCloud from SpiderMan reviews
setwd('D:/Workspace/R-Project/SpiderMan')
library(rJava)
library(KoNLP)  
library(RColorBrewer)
library(wordcloud2)
library(plotrix)
library(extrafont)
library(tm)
library(fmsb)
useSejongDic() #한글 세종사전
only_rev<-read.csv('SpiderMan_review.csv')
only_rev<-only_rev[,2]
write.table(only_rev, "D:/Workspace/R-Project/SpiderMan/SM_only_rev.txt", sep = ",",
            row.names = FALSE, quote = FALSE, append = TRUE,  na = "NA") 
only_review<-readLines('SM_only_rev.txt')
head(only_review)

Nouns <- sapply(only_review, extractNoun, USE.NAMES=F) #각 라인마다 명사단어들만 남기기
Nouns2<-unlist(Nouns) #명사만 추출된 데이터
Nouns2<-Filter(function(x) {nchar(x)>=2}, Nouns2) #2글자 이상만
head(Nouns2, 50)
Nouns2<-gsub('영화','',Nouns2)
Nouns2<-gsub('스파이더맨','',Nouns2)
Nouns2<-gsub('관람객','',Nouns2)
Nouns2<-gsub('스파이더맨은','',Nouns2)
Nouns2<-gsub('스파이더맨을','',Nouns2)
Nouns2<-gsub('스파이더맨이','',Nouns2)
Nouns2<-gsub('진짜','',Nouns2)
Nouns2<- gsub('[~!/@#$%&*^^()"_+=?<>]','',Nouns2)
Nouns2 <- gsub('[ㄱ-ㅎ]','',Nouns2)
Nouns2<- gsub('(ㅜ|ㅠ)','',Nouns2)
Nouns2 <- gsub("\\d+","",Nouns2)

wordcount <- table(Nouns2)
wordcount_top <-head(sort(wordcount, decreasing = T),300)
set.seed(123)
wordcloud2(wordcount_top, size=4, col="random-light", backgroundColor="black")


# legend('top', "리뷰 댓글 속 단어 빈도", cex=1, fill=NA, border=NA, bg="white",
#        text.col="red",text.font=2,box.col="red")

# 최종 이미지파일로 저장
