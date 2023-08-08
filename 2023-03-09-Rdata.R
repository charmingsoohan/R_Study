####2023-03-09####
###1교시###
library(readxl)
exam <- read_excel("excel_exam.xlsx")

#무작위로 5명을 뽑아주세요

exam[sample(nrow(exam),5),]
exam[sample(1:20,5),]
set.seed(1111) #씨앗의 값을 세팅 #난수=랜덤'수'#무작위로 표현되는 확률의 재현되는 값을 같게하기 위해

#위 5명의 수학 평균은?

a <- exam[sample(1:20,5),]
mean(a$math)

#A,B 중에서 A와 B가 3:1나오도록 10번 뽑기.

sample(c("A","b"),10,replace = T,prob = c(3,1))

#크롤링
library(rvest)
library(stringr)
setwd("C:/Rdata")
install.packages("rvest")
title=c()
body=c()

for(i in 1:5){
  url_1<-'
https://www.pressian.com/pages/news-world-list?page=
'
  url<-paste(url_1,i,sep='')
  print(url)
  
  hdoc<-read_html(url)
  
  title_part<-hdoc%>%html_nodes('.box')%>%html_text()
  title<-c(title,title_part)
  
}  


news=data.frame(제목=title)
news=cbind(title)
write.csv(news,'프레시안.csv')


library(multilinguer)
library(KoNLP)
txt<-readLines('프레시안.csv')
txt

library(wordcloud2)
library(RColorBrewer)
library(dplyr)

nouns<-extractNoun(txt)
wordcount<-table(unlist(nouns))
wordcount
df_word<-as.data.frame(wordcount,stringsAsFactors = F)
df_word<-rename(df_word,word=Var1)
df_word
df_word<-filter(df_word,nchar(word)>=2)
df_word<-df_word%>%
  arrange(desc(Freq))%>%
  head(20)
df_word
library(ggplot2)
wordcloud2(df_word)
ggplot(data=df_word,aes(x=reorder(word,Freq),y=Freq,fill=word))+geom_col()+coord_flip() 



###2교시###
#수강설문조사정리

interview <- read_excel("수강설문조사정리.xlsx")              
names(interview)
names(interview) <-  c("num", "subject", "sati1", "sati2", "sati3", "sati4", "sati5", "sati6", "sati7", "ect", "field")
names(interview)
unique(interview$sati5)
interview$sati5 <- ifelse(interview$sati5 == "매우 동의함", 4, ifelse(interview$sati5 == "동의함", 3, ifelse(interview$sati5 == "보통", 2, 1)))
interview$sati7 <- ifelse(interview$sati7 == "매우 동의함", 4, ifelse(interview$sati7 == "동의함", 3, ifelse(interview$sati7 == "보통", 2, 1)))
interview %>% group_by(subject) %>% summarise(avg1 = mean(sati5), avg2 = mean(sati7)) 


