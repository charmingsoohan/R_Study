####2023-03-07####
###1교시###
setwd("C:/Rdata")
library(foreign)
library(dplyr)
library(ggplot2)
welfare <- read.spss(file="Koweps_hpc10_2015_beta1.sav")
welfare <- as.data.frame(raw_welfare)
welfare <- welfare %>% select(gender=h10_g3, birth=h10_g4, marriage=h10_g10, 
                              religion=h10_g11,income=p1002_8aq1,job=h10_eco9,
                              region=h10_reg7)
welfare<- as_tibble(welfare)


welfare$gender <- ifelse(welfare$gender==1, "male", "female")


#배기량에 따른 도시연비를 비교하여 산포도를 만드시오
ggplot(mpg, aes(displ,hwy))+geom_point()+geom_smooth()+
  labs(title="배기량에 따른 도시 연비 비교")


#economics를 이용하여 시간에 따른 실업률 그래프를 만드시오
library(ggthemes)
ggplot(economics, aes(date, unemploy))+geom_line()+theme_economist()


#제조사가 audi인 행만 뽑기
mpg[mpg$manufacturer =="audi",]


#displ이 2보다 작은 행 뽑기
mpg[mpg$displ < 2, ]


#welfare 이용해 남,여 평균 소득구하기
welfare %>% group_by(gender) %>% summarise(avg=mean(income,na.rm=T))


#위에 두 문제를 그래프로 표현하기
welfare %>% group_by(gender) %>% summarise(avg=mean(income,na.rm=T)) %>% 
  ggplot(aes(gender,avg))+geom_col()


#welfare$gender 1은 'male' , 2는 'female' 로 바꾸기
welfare$gender <- ifelse(welfare$gender==1,'male','female')


#R에서 말하는 factor는 어떤 의미인가 : 범주(카테고리)


#dataframe을 tibble 형태로 바꿀 때 쓰는 명령어
as_tibble()


#marriage 컬럼에서 1은 기혼, 나머지는 미혼으로 바꾸시오
welfare$marriage <- ifelse(welfare$marriage==1,"기혼","미혼")


#20대의 혼인상태를 막대그래프로 그리시오
welfare$age <- 2015 - welfare$birth  
welfare %>% filter(age>=20 & age < 30) %>% ggplot(aes(x=age, fill=marriage))+
  geom_bar(position = 'dodge')



###2교시###

#연령대에 따른 월급차이
welfare <- welfare %>% filter(age>20) %>% 
  mutate(age_gen=ifelse(age<30, "young", ifelse(age<=50,"middle","old")))

welfare %>% filter(!is.na(income)) %>% 
  group_by(age_gen,gender) %>% summarise(mean_income=mean(income)) %>% 
  ggplot(aes(age_gen,mean_income,fill=gender))+geom_col(position = 'dodge') # filter(!is.na()) 권장 X


#ggplot() 은 막대를 알파벳 순으로 정렬-> 
#범주 순서 바꾸려면 scale_x_discrete(limits=c()) 에서 c()안에 범주 순서 지정
welfare %>% group_by(age,gender) %>% summarise(mean_income=mean(income,na.rm=T)) %>% 
  ggplot(aes(age,mean_income,color=gender))+geom_line() # na.rm=T 권장


#직업에 따른 소득
library(readxl)
list_job<- read_excel("Koweps_Codebook.xlsx",sheet=2)
list_job$code_job
names(list_job) <- c("job","직업")

welfare[ 1:3, c("job","income")]
welfare <- left_join(welfare,list_job,by="job")



###3교시###
#직업별 평균소득 
top10 <- welfare %>% group_by(직업) %>% summarise(평균소득=mean(income,na.rm=T)) %>% 
  arrange(desc(평균소득)) %>% head(10)

ggplot(top10,aes(reorder(직업,평균소득),평균소득,fill=직업))+geom_col() + 
  coord_flip() # coord_flip() : 막대 오른쪽 90도 회전

#지역별 인구 분포 분석하기
region_map <- data.frame(region=c(1:7),지역=
                           c("서울","수도권(인천/경기)","부산/경남/울산",
                             "대구/경북","대전/충남","강원/충북","광주/전남/전북/제주도"))

welfare <- left_join(welfare, region_map, by="region")

ggplot(welfare,aes(지역,region,fill=지역))+geom_col()

ggplot(welfare, aes(x=지역, fill = 지역))+geom_bar()+ylab("인구수") # 1차원

welfare %>% group_by(지역) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=지역, y=n, fill=지역))+
  geom_col()+
  labs(x="지역명",y="인구수") # 2차원


#반올림:round()
round(11/3,3)



###5교시###
#텍스트 마이닝
install.packages("multilinguer")
library(multilinguer)
install_jdk()
install.packages("KoNLP", 
                 repos = "https://forkonlp.r-universe.dev",
                 dependencies = TRUE,
                 INSTALL_opts = c("--no-multiarch"))
install.packages(c("stringr","hash","tau","Sejong","RSQLite","devtools"),type="binary")
install.packages("KoNLP", repos = "https://forkonlp.r-universe.dev", 
                 INSTALL_opts = c("--no-multiarch"))
install.packages("remotes")
remotes::install_github("haven-jeon/KoNLP",upgrade='never',
                        INSTALL_opts=c("--no-multiarch"))
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", 
                        force = TRUE, INSTALL_opts=c("--no-multiarch"))
library(KoNLP)
useNIADic()
useSejongDic()

text <- "안녕하세요, 좋은 아침입니다."
extractNoun(text)

text <- readLines("ahn.txt")
nouns <- extractNoun(text)
unlist(nouns)



###6교시###
words <- unlist(nouns)
df <- as.data.frame(table(words))
head(df)
words <- words[nchar(words)>=2]
filter(df,nchar(words)>=2)
install.packages("stringr")
library(stringr)
library(dplyr)
df <- df %>% arrange(desc(Freq)) %>% head(20)


#워드클라우드 만들기
install.packages("wordcloud2")
library(wordcloud2)
library(RColorBrewer)


# 막대그래프
g1 <- ggplot(data = df, aes(x=reorder(words,Freq),y=Freq,fill=words))+
  geom_col()+coord_flip()


#인터랙티브
library(ggplot2)
g <- ggplot(mpg, aes(x=displ, y=hwy, color=drv))+geom_point()

#패키지설치 install.packages("plotly")
library(plotly) 
ggplotly(g)

#인터랙티브 시계열 그래프 만들기

#패키지설치 install.packages("dygraphs")
library(dygraphs)
library(xts)

economics <- ggplot2::economics
eco <- xts(economics$unemploy, order.by = economics$date)
dygraph(eco)



###7교시###
library(KoNLP)
extractNoun() # 명사 추출 함수
extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다")

text <- readLines("ahn.txt") # 텍스트 파일 읽어오기
nouns <-extractNoun(text) # "text"에서 명사 추출 후 'nouns' 에 덮어쓰기
words <- unlist(nouns) # list 형식의 'nouns' 를 문자열 벡터로 변환해서 'words'에 덮어쓰기
df <- as.data.frame(table(words)) # 'table' 형태로 만들어진문자열 벡터인 words를 '데이터프레임' 형식으로 바꾸고 'df'에 덮어쓰기
head(df) # 'df' 에서 상위 6개의 데이터를 보여주기
words_2 <- words[nchar(words)>=2] # nchar() 함수로 'words'에서 문자열의 길이가 2 이상인 원소들을 골라서 'words' 배열에 나열하고 'words_2' 에 덮어써라
df %>% arrange(desc(Freq)) %>% head(20) # 'df' 의 데이터를 이용해, 'Freq'(빈도) 의 크기대로 내림차순 정렬을 하고, 20번째 데이터 까지 보여줘라 
class(df) 
class(words_2)
class(words)
class(table(words))
class(words[nchar(words)])