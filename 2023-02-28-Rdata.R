1+1
1/3
2^3
sqrt(4)
install.packages("dplyr")
library(dplyr)
a=1
primt(a)
print(a)
b="hello"
print(b)
a=2
print(a)
a=c(1,2)
print(a)
c()
combine
print(a)
a=1
b=2
c=a+b
print(c)
a=1
a=a+1
print(a)
var1<-c(1,2,3,)
var2 <-c (1:10)
var3 <- seq(1:10)
help(seq)
var1 <- c(1:5)
str1 <- "hello"
str2 <- c("hello","good morning")
a <- c(1, "hello")
typeof(c)
mode(str1)
typeof(str1)
a <- c(1, "Hello")
mode(a)
c
str1
var1 <- c(1,2,3,4,5)
var2 <- c(1:10)

sum(1,10)
seq1 <- seq(1, 10, 1)
sum1 <- sum(seq1)
print(sum1)

# 1부터 100까지 모든 홀수들의 평균은?
mean(seq(1, 100, 2))
var(seq(1, 100, 2))
sqrt(var(seq(1, 100, 2)))
sd(sqrt(var(seq(1, 100, 2))))
max(seq(1, 100, 2))

installed.packages()
install.packages("stringr")
paste()
str2
paste("hello", "good")
str <- c("hello", "world", "is", "good")
paste(str, collapse=",")
paste(str, collapse=" ")
sentence <- paste(str, collapse=" ")
print(sentence)

#package=library보다 큰개념 framework

qplot()
ggplot()
library(ggplot2)
x <- c("a", "a", 'b', 'c')
qplot(x)
ggplot(data=diamonds, aes(x=carat, y=price, color=cut)) +
 geom_point()

help(mean)
?ggplot

1. 변수이름은 scores
2. 평균은?
3. 평균을 변수 avg 에 담아봅시다.

= : assingment operator
<- <- <- 
  A<- c(80, 60, 70, 50, 90)
a <- c(80, 60, 70, 50, 90)

# 데이터프레임

english <-　ｃ（９０，８０，　６０，　７０）
english <-  c(90, 80, 60, 70)
math <- c(50, 60, 100, 20)
rm(str1)
str1
str2
rm(list=ls())
rm(str, str2)
rm(str)
rm(str2)

str2
rm(str2)

df_midterm <- data.frame(english, math, class)

class <- c(1,1,2,2)

english <- c(90, 80, 60, 70)
math <- c(50, 60, 100, 20)
data.frame(english, math)
df_midterm <- data.frame(english, math)
class <- c(1, 1, 2, 2)
df_midterm <- data.frame(english, math, class)
data.frame(english, math, class)
df_midterm <- data.frame(english, math, class)

df_midterm[2, 3]
df_midterm[3, c(1,2,3)]
df_midterm[2, 2]
df_midterm[c(2,3), c(1,2)]
df_midterm[2:3, 1:2]
df_midterm[3, -1]
df_midterm[3, -2]

df_midterm[ , 1]
df_midterm[ , 'english']
df_midterm$english
df_midterm$math

# 영어성적의 평균은 얼마인가?
mean(df_midterm$english)

df_midterm <- data.frame(english= c(90, 80, 60, 70), math= c(50, 60, 100, 20), class= c(1, 1, 2, 2)

제품 <- c("사과", "딸기", "수박")
가격 <- c(1800, 1500, 3000)
판매량 <- c(24, 38, 13)
data.frame(제품=c("사과", "딸기", "수박"),
           가격=c(1800, 1500, 3000),
           판매량=c(24, 38, 13))
df=data.frame(제품=c("사과", "딸기", "수박"),
              가격=c(1800, 1500, 3000),
              판매량=c(24, 38, 13))
mean(df$판매량)


read_exel("excel_exam.xlsx")
library(readxl)
install.packages("readxl")

# working directory
getwd()
setwd("c:/Rdata")
read_excel("excel_exam.xlsx")

df <- read_excel("excel_exam.xlsx")
mean(df$english)

read.csv("csv_exam.txt")

write.csv(df, "exam.csv")
read.csv("exam.csv")
exam <- read.csv("exam.csv")
exam[ , 2:6]
exam[ , -1]
exam <- exam[ , -1]
exam <- exam[ , -1]
exam

df
save(df, file =  "df.rds")
load(df.rds)
saveRDS(df, file =  "df.rds")
readRDS("df.rds")

exam
df_exam <- read_excel("excel_exam.xlsx")
head(exam, 2)
tail(exam, 2)
View(exam)
dim(exam)
str(exam)

# obs: 관측치
# 행: 관측치, 열: 변수
# int: 정수 

summary(exam)

library(ggplot2)
diamonds

library(ggplot2)
diamonds
summary(diamonds)
#몇행 몇열? 
#캐럿 최댓값?
#가격 중앙값?
#가격 평균?
mpg
