###2023-03-08###
###1교시###
setwd("C:/Rdata")
df_csv_exam <- read.csv("csv_exam.csv")
raw_welfare <- read.spss("Koweps_hpc10_2015_beta1.sav")
list_job<- read_excel("Koweps_Codebook.xlsx",sheet=2)
install.packages("ggplot2")
install.packages("readxl")
install.packages("dplyr")
install.packages("foreign")
install.packages("multilinguer")
install_jdk()
install.packages("KoNLP", 
                 repos = "https://forkonlp.r-universe.dev",
                 dependencies = TRUE,
                 INSTALL_opts = c("--no-multiarch"))
install.packages(c("stringr","hash","tau","Sejong","RSQLite","devtools"),type="binary")
install.packages("KoNLP", repos = "https://forkonlp.r-universe.dev", INSTALL_opts = c("--no-multiarch"))
install.packages("remotes")
remotes::install_github("haven-jeon/KoNLP",upgrade='never',INSTALL_opts=c("--no-multiarch"))
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", force = TRUE, INSTALL_opts=c("--no-multiarch"))
library(readxl)
library(ggplot2)
library(dplyr)
library(foreign)
library(multilinguer)
library(KoNLP)
welfare <- as.data.frame(raw_welfare)
welfare<-welfare %>% 
  rename(gender=h10_g3, birth=h10_g4,
         marriage=h10_g10, religion=h10_g11,
         income=p1002_8aq1, job=h10_eco9,
         region=h10_reg7) %>% 
  select(gender, birth, marriage, religion,
         income, job, region)
welfare <- as_tibble(welfare)


#marriage 열이1일 경우"기혼",3일 경우"미혼" 나머지는 결측치로
welfare$group_marriage <- ifelse(welfare$marriage==1,"기혼",
                                 ifelse(welfare$marriage==3,"미혼",NA))


#직업과 결혼여부가 결측치가 아닌 행만을 추출
welfare %>% filter(!is.na(job) & !is.na(group_marriage))


#welfare$group_marriage칼럼안의 "미혼"을 "이혼"으로 바꾸기
welfare$group_marriage <-ifelse(welfare$group_marriage == "미혼", "이혼", welfare$group_marriage) 


#직업에 따른 이혼율 구하기
df <- welfare %>%  group_by(job,group_marriage) %>% 
  summarise(n=n()) %>% 
  mutate(total=sum(n),ratio=round(n/total*100,1)) %>% 
  filter(group_marriage=="이혼") %>% select(job,ratio)


#이혼율이 높은 직업 상위 10개로 그래프 그리시오
ggplot(aes(x=factor(job),y=ratio,fill=factor(job)))+geom_col()




###2교시###
#크롤링:웹 페이지를 그대로 가져와서 데이터를 추출하는 행위
install.packages("rvest")
install.packages("stringr")
library(rvest)
library(stringr)
hdoc <- read_html("https://search.daum.net/search?w=news&nil_search=btn&DA=NTB&enc=utf8&cluster=y&cluster_page=1&q=%ED%8A%B8%EC%99%80%EC%9D%B4%EC%8A%A4")
temp <- html_nodes(hdoc,".tit_main.fn_tit_u" )
paste(temp[1])
html_text(temp[1])
for(i in 1:3){print(i)}


###3교시###
#기사 본문 따오기
temp2 <- html_nodes(hdoc,".desc" )
html_text(temp2[1])
title <- c()
title <- c(title,"a")
title <- c(title,"b")
title <- c(title,"c")


hdoc <- read_html("https://search.daum.net/search?w=news&nil_search=btn&DA=NTB&enc=utf8&cluster=y&cluster_page=1&q=%ED%8A%B8%EC%99%80%EC%9D%B4%EC%8A%A4")
title <- hdoc %>% html_nodes(".tit_main.fn_tit_u") %>% html_text()
body <- hdoc %>% html_nodes(".desc") %>% html_text()

#2페이지
hdoc <- read_html("https://search.daum.net/search?w=news&DA=PGD&enc=utf8&cluster=y&cluster_page=1&q=%ED%8A%B8%EC%99%80%EC%9D%B4%EC%8A%A4&p=2")
title_temp <- hdoc %>% html_nodes(".tit_main.fn_tit_u") %>% html_text()
title <- c(title,title_temp)

body_temp <- hdoc %>% html_nodes(".desc") %>% html_text()
body <- c(body,body_temp)


url_1 <- "https://search.daum.net/search?w=news&DA=PGD&enc=utf8&cluster=y&cluster_page=1&q=%ED%8A%B8%EC%99%80%EC%9D%B4%EC%8A%A4&p="
for(i in 1:3){url=paste(url_1,i,sep = "")
  print(url)}



for(i in 1:3){
  url_1 <-"https://search.daum.net/search?w=news&nil_search=btn&DA=NTB&enc=utf8&cluster=y&cluster_page=1&q=%ED%8A%B8%EC%99%80%EC%9D%B4%EC%8A%A4=" 
  url <- paste(url_1,i,sep="")
  print(url)
  
  hdoc <- read_html(url)
  title_part <- hdoc %>% html_nodes(".tit_main.fn_tit_u") %>% html_text()
  print(title_part)
  title <- c(title,title_part)
  
  body_part <-  hdoc %>% html_nodes(".desc") %>% html_text()
  print(body_part)
  body <- c(body,body_part) 
}



###4교시###
#뉴진스
title = c()
body = c()

url_1 <- "https://search.daum.net/search?nil_suggest=btn&w=news&DA=PGD&q=%EB%89%B4%EC%A7%84%EC%8A%A4&p="


for(i in 1:10){
  url <- paste(url_1, i, sep = "")
  print(url)
  
  hdoc <- read_html(url)
  
  title_part <- hdoc %>% html_nodes(".tit_main.fn_tit_u") %>% html_text()
  body_part <- hdoc %>% html_nodes(".desc") %>% html_text()
  
  title <- c(title, title_part)
  body <- c(body, body_part)
}

#news = data.frame(제목 = title, 본문 = body)

news = cbind(title, body)

write.csv(news, "뉴진스.csv",fileEncoding = 'cp949') 


# 자바설치
install.packages("multilinguer")
multilinguer::install_jdk()



## 의존성 패키지 설치
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")


## 깃허브 통해 KoNLP 다운로드
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch")) 


###5교시###
#sample
sample

x <- 1:12
sample(x)

x <- 1:5
sample(x,3,replace = TRUE)
sample(x, replace = TRUE)

exam <- read.csv("csv_exam.csv")
exam <- exam[,2:ncol(exam)]
exam <- exam[,-1]

exam[1:10,]
exam[seq(1,20,2),]
exam[-seq(1,20,2),]

exam <- exam[,names(exam)!='id']
exam[sample(1:20,10),]
exam[sample(10:20,5),]

set.seed(1234)
sample(1:10,5)

set.seed(1234)
exam[sample(1:20,10),]

#5394 개 랜덤으로 추출하기
sample(1:53940,5394)
num <- sample(nrow(diamonds),nrow(diamonds)*.5)
num1 <- sample(nrow(temp),nrow(diamonds)*.3)

dia <- diamonds[num,]
dia_2 <- diamonds[-num,]

ggplot(dia,aes(x=carat,y=price,color=color))+
  geom_point()

#diamonds5:3:2로 서로다른 3개의 세트를 만드시오.
diamonds[sample(n,n*0.5)],

n <- nrow(diamonds)
train <- diamonds[sample(nrow(diamonds),nrow(diamonds)*.5)]
num1 <- sample(nrow(temp),nrow(diamonds)*.3)
train <- diamonds[num,]
temp <- diamonds[-num,]
val <- temp[num1,]
test <- temp[-num,]
nrow(train)
nrow(temp)
nrow(test)


###6교시###
sample_n(diamonds,nrow(diamonds)*0.5)
sample_frac(diamonds,0.3)
sample(c("a","b"),10,replace = T,prob = c(5,2))


ggplot(mpg,aes(x=drv,fill=drv))+
  geom_bar()+scale_fill_hue(c=50)

ggplot(mpg,aes(x=drv,fill=drv))+
  geom_bar()+scale_fill_brewer(palette = "Set2")

#미국 주별 강력 범죄율 단계 구분도 만들기
install.packages("mapproj")
install.packages("ggiraphExtra")
install.packages("maps")
library(ggiraphExtra)
library(tibble)
library(ggplot2)

crime <- rownames_to_column(USArrests,var="state")
crime$state <- tolower(crime$state)
states_map <- map_data("state")

ggChoropleth(crime,aes(fill=Murder,map_id=state),
             map=states_map,interactive = T)


