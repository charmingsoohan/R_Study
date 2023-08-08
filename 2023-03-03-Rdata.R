2023-03-03
#결측치
df <- data.frame(gender=c("M","F",NA,"M","F"),
                 score=c(5,4,3,4,NA))
df
#결측치가 있는지
is.na(df)
sum(is.na(df))
table(is.na(df))
airquality
sum(is.na(airquality$Ozone))
summary(mpg)
summary(airquality)
#결측치 처리하기
na.omit(df)
df$gender <- ifelse(is.na(df$gender),"M",df$gender)
df$score<-ifelse(is.na(df$score),5,df$score)
df
df[3,1] <- "F"
df[3,1] <- NA
df[5,2] <- NA
mean(df$score,na.rm = T)
sum(df$sum)
airquality %>% head()
#오존의 평균값은?
mean(airquality$Ozone,na.rm=T)
#1.airquality에서 solar.r을 solar로 바꾸어라.
#2.ozone과 solar만 행 뽑아서, 두 칼럼 각각
#평균을 구해주세요.
airquality <- airquality %>% rename(Solar=Solar.R)
              mean(airquality$Ozone,na.rm = T)
              mean(airquality$Solar,na.rm = T)
#이상치 outlier
outlier <- data.frame(gender=c(1,2,1,3,2,1),
                      score=c(5,4,3,4,2,10))              
outlier
#이상치가 있는지?
boxplot(outlier$score)
boxplot(outlier)
boxplot(diamonds$cut)
ggplot(data = diamonds,aes(x=cut))+geom_boxplot()
boxplot(outlier$score)
#이상치 처리하기
# 6번은 제외하고 평균score을 구하시오.
mean(outlier$score[-6])
#한줄,1차원:vactor
#표,2차원:dataframe
outlier$score[3]
outlier$score[c(1,3,4,5)]
outlier %>% renamer
outlier$score[6] <- NA
mpg
ggplot(data=mpg,aes(x=displ,y=hwy))
ggplot(data=mpg,aes(x=displ,y=hwy))+
  geom_point()
ggplot(data=mpg,mapping=aes(x=displ,y=hwy))+
  geom_point(color="blue",size=3)+
  geom_line()+
  xlim(3,6)+
  ylim(10,30)
ggplot(mpg,aes(x=displ,y=hwy))+geom_point()
ggplot(mpg,aes(x=cty,y=hwy))+geom_point()
str(mpg)
glimpse(mpg)
ggplot(mpg,aes(x=displ,y=hwy,color=class))+geom_point()
#막대그래프:geom_bar()
ggplot(data=mpg,aes(x=class,fill=class)+geom_bar()
df <- table(mpg$class) %>% as.data.frame()
df <- df %>% rename(class=Var1)
ggplot(df,aes(x=class,y=Freq))+geom_col()
ggplot(mpg,aes(x=drv,fill=class))+geom_bar()
ggplot(mpg,aes(x=class,fill=drv))+geom_bar()
ggplot(df,aes(x=class,y=Freq,fill=class))+geom_col()
df %>% arrange(Freq) %>%
  ggplot(aes(x=reorder(class,-Freq),y=Freq,fill=class))+geom_col()
df1 <- mpg %>% group_by(drv) %>% summarise(Mean=mean(hwy))  
ggplot(df1,aes(x=reorder(drv,Mean),y=Mean,fill=drv))+geom_col()
economics
ggplot(economics,aes(x=date,y=unemploy))+geom_line()
economics$date <- as.Date(economics$date)
ggplot(data=mpg,aes(x=drv,y=hwy,fill=drv))+geom_boxplot()
