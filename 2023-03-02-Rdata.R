install.packages("readxl")
library(readxl)
exam <- read_excel("exam.xlsx")
getwd()
sd(seq(1,100, 3)) 
setwd()
getwd()
setwd("C:/Rdata")
diamonds


#데이터 전처리 
dplyr
library(dplyr)
exam <- read.csv("csv_exam.csv")
read.csv("csv_exam.csv")

filter(exam,class==1)
filter(exam,class==2)
# 영어가90이상인 학생은 몇 명?
filter(exam,english>=90)
# 영어가90이상이고 1반인 학생은?
filter(exam,english>=90&class==1)
exam%>%filter(class==1)
exam%>%
  filter(english>90)%>%
  filter(class==1)
#수학이90보다 작은 학생중 영어는70보다 크고 
#1반이 아닌 학생들?
exam %>% filter(math < 90) %>% 
  filter(english > 70) %>% 
  filter(class != 1) 
#1반,2반 또는 3반 학생들?
exam %>% filter(class==1|class==2)
exam %>% filter(class==1|class==2|class==3)
exam %>% filter(class %in% c(1,2,3))
exam
#1반 학생의 수학평균은?
df <- exam %>% filter(class==1)
mean(df$math)
#열 뽑기
head(exam)
exam %>% head(2)
select(exam,class, math)
exam %>% select(class,math)
exam %>% select(-math)
exam %>% filter(class==1) %>%
  select(class,math)
#정렬
arrange()
exam %>% arrange(math)
exam %>% arrange(desc(math))
exam %>% arrange(desc(class),math)
#칼럼(열)추가하기
#total(총점)구하기=mutate()
exam %>% mutate(math+english+science)
exam %>% mutate(total=math+english+science)
#총점과 평균 칼럼을 추가해 주세요
exam %>% mutte(ttal=math+engli+science) %>% mutate(avg=total/3)
exam %>% mutate(total = math +nglish + science, mean = total/3) 
#ifels>% mutate(test=ifelse(science>60,"합격","불합격"))
#수학,영어,과학의 평균을 구하고, 내림차순으로
#정렬하mutate( total = math + english + science) %>% 
mutate( avg = total / avg > 60, "pass","fail")) %>% arrange(desc(avg))
#행을 줄이면filter열을 추가하려면mutate
#요약=summarise() group_by()
head(exam)
exam %>% select(math) %>% summarise(평균=mean(math))
class3 <- exam %>% filter(class==3)
mean(class1$math)
exam %>% summarise(mean(math))
exam %>% group_by(class) %>% 
  summarise(max(math))
#요약함수:sum, mean, sd, median, min, max, n()
exam %>% group_by(class) %>% 
  summarise(mean(math+english+science))
exam %>% group_by(class) %>% 
  summarise(n())
#영어의 반별 평균, 최고점, 최하점, 중앙값, 빈도수
diamonds
library(ggplot2)
diamonds
diamonds %>% head()
table(diamonds$cut)
unique(diamonds$cut)
#diamonds의 최댓값과 최대값은?
diamonds %>% group_by(cut) %>% 
  summarise(max=max(price), min=min(price))
#mpg
colnames(mpg)
rownames(mpg)
names(mpg)
mpg$class
mpg %>% group_by(class) %>% 
  summarise(mean(cty)) %>% arrange("desc(mean(cty)))")
#``:backtick, ~:tilde, ^:carat
#hwy 연비평균이 좋은 상위 6개회사
mpg %>% group_by(manufacturer) %>% 
  summarise(MEAN=mean(hwy)) %>% arrange(desc(MEAN)) %>% head(6)
mpg %>% group_by(manufacturer) %>% 
  summarise(MEAN=mean(hwy)) %>% arrange((MEAN)) %>% head(6)
test1 <- data.frame(id=c(1,2,3,4,5,6),
           midterm=c(60,80,70,90,85,100))
test2 <- data.frame(id=c(1,2,3,4,5,7),
           final=c(70,83,65,95,80,0))
right_join(test1,test2,by='id')
full_join(test1,test2)
bind_cols(test1,test2) %>% select(-id...3)
bind_rows(test1,test2)
#NA(not available:값이 없음(결측치))
exam
name <- data.frame(class=c(1,2,3,4,5),
                   teacher=c("Lee","Park","Jeong","Choi","Kim"))
left_join(exam,name,by='class')
#변수명(칼럼)바꾸기
#dplyr:filter,select,rename,arrange,mutate,left_join, bind_rows
exam %>% rename(반=class)
mpg %>% rename(city=cty,highway=hwy) %>% names()
mpg %>% names()
colnames(mpg) <- c("manufacturer", "model",       
                    "displ",        "year",        
                    "cyl",          "trans",       
                    "drv",          "city",        
                    "highway",      "fl",          
                    "class")
colnames(mpg)
mpg <- mpg %>% select(manufacturer, model) %>% head(10)
#manufaturer을 mf로 바꾸시오. 
exam <- exam %>% filter(id<11)
exam <- exam %>% select(id,class,math)
exam$classes <- exam$math
exam %>% rename(english=classes)
exam$Sum <- exam$math+exam$classes
exam

0303(금)복습
install.packages("ggplot2")
library(ggplot2)
exam <- data.frame(class= c(1,2,3,4,5),
        subject = c("math", "english", "science","society","korean"))
name <-data.frame(class= c(1,2,3,4,5),
       teacher = c("kim","lee","park","choi","jung"))
left_join(exam,name)
library(dplyr)
left_join(exam,name)
mpg
mpg %>% group_by(manufacturer) %>% 
  summarise(mean(cty))
mpg %>% group_by(manufacturer) %>% 
  filter(class=="suv") %>% 
  summarise("차종수"=n()) %>% 
  arrange(desc("차종수"))
rm(mpg)

mpg_new <- mpg
mpg_new <- mpg_new %>% rename(displacement=displ,fuel=fl)
names(mpg_new)
exam <- read.csv("csv_exam.csv")
exam %>% mutate(math2=ifelse(math>=80,"A",
         ifelse(math>=60,"B",
        ifelse(math>=40,"C","F"))))
