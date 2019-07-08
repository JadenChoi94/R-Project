library(rvest)
library(KoNLP)
library(stringi)
library(dplyr)
library(reshape2)
library(ggplot2)
library(wordcloud)
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
# 영화, 마션 140자평, 별점, 날짜 데이터 가져오기

urls<-paste0("https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=173123&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page=",c(1:end_page))
urls <- lapply(urls,read_html)



# 시간 데이터 처리
# 참고URL) http://www.stat.berkeley.edu/~s133/dates.html
plot(table(format(data$date,'%j')))
table(format(data$date,'%A'))

data<-data[complete.cases(data$review),]

# 문자열 데이터 처리
# 참고URL) http://gastonsanchez.com/Handling_and_Processing_Strings_in_R.pdf
data$text.clean<- gsub("\t", " ", data$text)
data$text.clean<- gsub("\n", " ", data$text.clean)
data$text.clean<- gsub("\r", " ", data$text.clean)
data$text.clean<- gsub("ㅋ", " ", data$text.clean)
data$text.clean<- gsub("ㅎ", " ", data$text.clean)

# KoNLP pacakges에서 띄어쓰기는 중요한 정보
data$text.clean<- gsub("[[:punct:]]", " ", data$text.clean)
data$text.clean<- gsub("\\W", " ", data$text.clean)
data$text.clean<- gsub("\\s+", " ", data$text.clean)

# 평점과 영화평 길이
data$nchar<- nchar(data$text.clean)
hist(data$nchar)
ggplot(data, aes(factor(star), nchar))+geom_boxplot()

# 명사추출, 그러나 비교적 부정확
text.noun<- NA
for (i in 1:nrow(data) ) {
  text.noun[i]<-melt(extractNoun(data[i,6]))
}

# Pos22는 단순 명사추출 명령어 보다는 좋은 성능. 그러나 정확하지 않음. 탐색적 목적으로는 활용 가능할 것으로 보임
# 한글 전처리의 정확성 향상을 위해서는 R의 KoNLP보다 다른 프로그램 사용이 좋은 성능을 보여줄 수 도 있음
text.noun<- NA
for (i in 1:nrow(data) ) {
  text.noun[i]<-melt(SimplePos22(data[i,6]))
}
text.noun<-melt(text.noun)

text.noun$num<-stri_count_regex(text.noun$value, "/NC")
text.noun<- text.noun %>% filter(num>0)
text.noun$value<-gsub("(/NC)+[[:print:]]+$", "", text.noun$value)
text.noun$value<- gsub("/NC", "", text.noun$value)

# 빈도와 함께 명사 목록 확인
text.noun.table<-table(text.noun$value)
head(sort(-text.noun.table),10)

# 워드클라우드 만들기 예제
pal <- brewer.pal(12,"Paired")
# windowsFonts(malgun=windowsFont(""))
wordcloud(names(text.noun.table),freq=text.noun.table, scale=c(5,.5),rot.per=0.25,min.freq=10, random.order=F,random.color=T,colors=pal)

# 추출된 명사와 별정 정보
names(text.noun)[1:2]<-c('noun','id')
text.noun.star<-merge(text.noun,data, c('id'),all.x=T)

# 특정 별점 구간별로 추출된 명사 확인
text.noun.star %>% filter(star<=2) %>% count(noun) %>% arrange(-n) %>% mutate(n/sum(n)*100) %>% head(20)
text.noun.star %>% filter(star>2&star<=4) %>% count(noun) %>% arrange(-n) %>% mutate(n/sum(n)*100) %>% head(20)
text.noun.star %>% filter(star==5) %>% count(noun) %>% arrange(-n) %>% mutate(n/sum(n)*100) %>% head(20)

# 다른 영화와 비교 언급 (ex, 인터스텔라, 그래비티)
data$movie.name<-stri_count_regex(data$text.clean, "인터스텔라|그래비티|그레비티|그라비티|인터스탤라")
table(data$movie.name)/nrow(data)

# 배우, 감독 언급 (ex, 맷데이먼, 리들리 스콧)
data$act.drt.name<-stri_count_regex(data$text.clean, "리들리|스콧|맷|데이먼|멧|대이먼")
table(data$act.drt.name)/nrow(data)

# 영화 내용에 대한 언급 (ex, 이야기, 스토리, 내용)
data$contents<-stri_count_regex(data$text.clean, "이야기|스토리|내용")
table(data$contents)/nrow(data)

# 위의 주제를 평점별로 비교
data %>% filter(movie.name>0) %>% summarise(mean(star), sd(star))
data %>% filter(act.drt.name>0) %>% summarise(mean(star), sd(star))
data %>% filter(contents>0) %>% summarise(mean(star), sd(star))

# Density로 보기 (별점)
ggplot(data, aes(star)) + 
  geom_density(data = subset(data,movie.name > 0), fill = "red", alpha = 0.2) + 
  geom_density(data = subset(data,act.drt.name > 0), fill = "blue", alpha = 0.2) +
  geom_density(data = subset(data,contents > 0), fill = "green", alpha = 0.2)

# 위의 주제를 날짜별로 비교
data %>% filter(movie.name>0) %>% summarise(mean(date))
data %>% filter(act.drt.name>0) %>% summarise(mean(date))
data %>% filter(contents>0) %>% summarise(mean(date))

# Density로 보기 (날짜)
ggplot(data, aes(date)) + 
  geom_density(data = subset(data,movie.name > 0), fill = "red", alpha = 0.2) + 
  geom_density(data = subset(data,act.drt.name > 0), fill = "blue", alpha = 0.2) +
  geom_density(data = subset(data,contents > 0), fill = "green", alpha = 0.2)