exam <- read.csv("csv_exam.csv")
setwd("C:/Rdata")
getwd()
exam
library(dplyr)
exam %>% filter(english<=70&class) %>% 
  filter(class==4|class==5)
exam %>% group_by(class) %>% 
  summarise(mean(english+math+science))
mpg %>% group_by(model) %>% 
  summarise(MEAN=mean(cty)) %>% 
  arrange(MEAN) %>% head(6)
install.packages("ggplot2")
library(ggplot2)
exam %>% filter(english>=90&class==1)
exam %>% filter(math<=90&english>=&class %in%)
exam1 <- exam %>% mutate(total=english+math+
                          science,avg=(english+math+science/3))
exam1
exam %>% mutate(total=math+english+science) %>%
  mutate(avg=total/3) %>% 
  mutate(ifelse(avg>=60,"pass","fail"))
ggplot(mpg,aes(x=class,fill=class))+geom_bar()  
mpg1 <- mpg %>% group_by(manufacturer) %>% 
 summarise(MEAN=mean(cty)) %>% head(5)
mpg2 <- mpg %>% group_by(manufacturer) %>% summarise(mean_cty = mean(cty)) %>% head(5)
ggplot(data=mpg2, aes(x=reorder(manufacturer,-mean_cty), y=mean_cty)) + geom_col() 
       #xlab(),ylab()=라벨링 하기
exam[exam$english>90,]       
exam[exam$class==1,]
exam$class==1
exam[exam$class==1,]
class==1
class
var1 <- c(1,2,3,4,5)
mode(var1)
var2 <- as.factor(var1)
mode(var2)
str(diamonds)
class(var2)
var3 <- c("a","b","b","c","c","c")
class(var3)
var3 <- factor(var3)
class(mpg)
class(mpg[1:3,1:3])
class(mpg[,3])
class(mpg[3,])
class(mpg$manufacturer)
ggplot(data = mpg,aes(x=displ,y=hwy,color=drv))+geom_point()+
  geom_smooth(method = "lm")
g <- ggplot(data = mpg,aes(x=displ,y=hwy,color=drv))+geom_point()
g1 <- g+geom_smooth(method = "lm")
g1+theme_dark()
g1+theme_bw()
library(ggthemes)
g1+theme_wsj()
g2 <- g1+labs(title=
          "<배기량에 따른 고속도로 연비 비교>",
        x="배기량",y="연비")
g2+facet_wrap(~drv)
ggplot(exonomics,aes(date,unemploy))+geom_line()
ggplot(mpg,aes(cty))+geom_histogram()
ggplot(mpg,aes(cty))+geom_histogram(bins=20)+
  geom_freqpoly(bins=20,color='red')

ggplot(mpg,aes(drv,hwy))+geom_point()
ggplot(mpg,aes(drv,hwy))+geom_violin()
mpg %>% filter(hwy<20&drv=="f")
mpg %>% filter(hwy<25&drv=="f") %>% arrange(hwy)
ggplot(mpg,aes(drv,hwy))+geom_point()
ggplot(mpg,aes(drv,hwy,size=hwy,color=hwy))+
  geom_point()
ggplot(mpg,aes(hwy))+geom_freqpoly(binwidth=1)
ggplot(mpg,aes(displ,color=class))+geom_freqpoly(binwidth=0.5)
ggplot(mpg,aes(displ,fill=drv))+geom_histogram(binwidth = 0.5)
ggplot(mpg,aes(displ,fill=drv))+geom_histogram(binwidth = 0.5,position = "dodge")
ggplot(mpg,aes(displ,color=drv))+geom_freqpoly(binwhidth=0.5,color="red")
ggplot(mpg,aes(displ,fill=drv))+geom_histogram(binwidth = 0.5)
ggplot(mpg,aes(displ,fill=drv))+geom_histogram(binwidth = 0.5,position = "dodge")
ggplot(mpg,aes(displ,fill=drv))+geom_histogram(binwidth = 0.5)+facet_wrap(~drv)
ggplot(mpg,aes(manufacturer))+geom_bar()
ggplot(economics,aes(date,unemploy/pop))+geom_line()
install.packages("foreign")
library(foreign)
raw_welfare <- read.spss("Koweps_hpc10_2015_beta1.sav")
setwd("C:/Rdata")
getwd()
dim(welfare)
welfare <- as.data.frame(raw_welfare)
length(names(welfare))
dim(welfare)[2]
welfare<-welfare %>% 
  rename(gender=h10_g3, birth=h10_g4,
         marriage=h10_g10, religion=h10_g11,
         income=p1002_8aq1, job=h10_eco9,
         region=h10_reg7) %>% 
  select(gender, birth, marriage, religion,
         income, job, region) 
welfare <- welfare %>% 
  rename(gender=h10_g3,birth=h10_g4,
         marriage=h10_g10,religion=h10_g11,
         income=p1002_8aq1,job=h10_eco9,
         region=h10_reg7) %>% 
  select(gender,birth,marriage,
         religion,income,job,region)
welfare <- welfare %>% select(gender=h10_g3,birth=h10_g4,
                              marriage=h10_g10,religion=h10_g11,
                              income=p1002_8aq1,job=h10_eco9,
                              region=h10_reg7)
library(dplyr)
welfare
welfare <- as_tibble(welfare)
str(welfare)
boxplot(welfare)
sum(is.na(welfare))
colSums(is.na(welfare))
mean(welfare$income,na.rm = T)
welfare$income <- ifelse(welfare$income==0,NA,welfare$income)
summary(welfare$income)
boxplot(welfare$income)
ggplot(welfare,aes(x=income,color=factor(gender)))+geom_density()
install.packages("ggplot2")
library(ggplot2)
welfare$gender
welfare$gender <- ifelse(welfare$gender==1,"male","female")
ggplot(welfare,aes(x=gender,fill=gender))+geom_bar()
# 여자평균 소득, 남자평균 소득
wel1 <- welfare %>% group_by(gender) %>% 
  summarise(n=mean(income,na.rm=T))
ggplot(wel1,aes(gender,n,fill=gender))+geom_col()+
  ylab("mean of incom")
2014->1
2013->2
2012->3
welfare$age=2014-welfare$birth
welfare$age <- 2015-welfare$birth
welfare %>% select(birth,age)
range((welfare$age))
ggplot(welfare,aes(x=welfare$age,y=welfare$income))+geom_line()
welfare %>% group_by(age) %>% 
  summarise(평균소득=mean(income,na.rm=T)) %>% 
  ggplot(aes(x=age,y=평균소득))+geom_()
