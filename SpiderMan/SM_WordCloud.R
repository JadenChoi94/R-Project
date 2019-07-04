#WordCloud from SpiderMan reviews
install.packages('wordcloud2')
setwd('D:/Workspace/R-Project/SpiderMan')
library(rJava)
library(KoNLP)  
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)

useSejongDic() #한글 세종사전
only_review<-readLines('SpiderMan_only_review.txt')


Nouns <- sapply(only_review, extractNoun, USE.NAMES=F) #각 라인마다 명사단어들만 남기기
Nouns2<-unlist(Nouns); #명사만 추출된 데이터
Nouns2<-Filter(function(x) {nchar(x)>=2}, Nouns2) #2글자 이상만
head(Nouns2, 50)
Nouns2<-gsub('영화','',Nouns2)
Nouns2<-gsub('스파이더맨','',Nouns2)
Nouns2<-gsub('스파이더맨은','',Nouns2)
Nouns2<-gsub('스파이더맨을','',Nouns2)
Nouns2<-gsub('스파이더맨이','',Nouns2)
Nouns2<-gsub('진짜','',Nouns2)
Nouns2<- gsub('[~!@#$%&*^^()"_+=?<>]','',Nouns2)
Nouns2 <- gsub('[ㄱ-ㅎ]','',Nouns2)
Nouns2<- gsub('(ㅜ|ㅠ)','',Nouns2)
Nouns2 <- gsub("\\d+","",Nouns2)

wordcount <- table(Nouns2)
head(wordcount)
wordcount_top <-head(sort(wordcount, decreasing = T),300)
set.seed(123)
wordcloud2(wordcount_top, size=3, col="random-light", backgroundColor="black")



# set.seed(1234)
# SM_WC<-wordcloud(
#   names(wordcount_top),
#   freq=wordcount_top,
#   scale=c(8,0.5), #빈도가 가장 큰 단어와 가장 빈도가 작은단어 폰사 사이 크기
#   rot.per=0.1, #90도 회전해서 보여줄 단어 비율
#   min.freq=2, # 빈도 3이상
#   random.order=F, # True : 랜덤배치, False : 빈도수가 큰단어를 중앙에 배치
#   random.color=T, # True : 색랜덤, False : 빈도순
#   colors=brewer.pal(10,'Dark2'), #11은 사용할 색상개수, 두번째는 색상타입이름
#   family="font")

# 최종 이미지파일로 저장
