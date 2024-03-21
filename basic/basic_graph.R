library(ggplot2)
library(forcats)

transp<-c("bicyle", "bus", "bus", "walking", "bus", "bicyle", "bicyle", 
          "bus", "bus", "bus", "bicyle", "bus", "bicyle", "bicyle", "walking", 
          "bus", "bus", "bicyle", "bicyle", "walking", "walking", 
          "bicyle", "bus", "bus", "bus", "bus", "bicyle", 
          "bus", "bus", "bicyle", "bicyle", "bicyle")
transp

dat1 <- data.frame(transp)
dat1

# 막대 그래프
ggplot(dat1) + geom_bar(mapping = aes(fct_infreq(transp))) + 
  xlab('Transportation')

# 도수분포표 기반 막대그래프
obesity<-factor(c("underweight", "normal", "overweight", "obese"),
                levels=c("underweight", "normal", "overweight", "obese"))
count<-c(6, 69, 27, 13)
perc<-count/sum(count)*100
dat2 <- data.frame(obesity, count, perc)
dat2
ggplot(dat2) + 
  geom_bar(mapping = aes(obesity, perc), # x(범주 이름), y(범주별 도수)
           stat="identity") + xlab('Obesity') + ylab('Percentage (%)')

# 도수분포표
table(transp)
dat3 <- data.frame(transportation=c('bus', 'bicycle', 'walking'), count=c(15, 13, 4))
dat3

ggplot(dat3) + geom_bar(mapping=aes('', count, fill=transportation), stat='identity') +
  coord_polar('y', start=0) + xlab('') + ylab('') + # 원그래프
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
  
# 히스토그램
score<-c(93, 83, 91, 68, 75, 87, 89, 96, 97, 67, 83, 81, 87, 80, 64,
         83, 88, 76, 91, 78, 72, 80, 69, 80, 84, 71, 91, 81, 88, 73)
hist(score, main='')

rv<-c(0.8, 0.8, 0.8,  0.9, 0.9, 0.9, 0.9, 0.9, 1, 1, 1.8, 2, 2.1, 2.3, 2.4, 2.8,
      2.9, 3, 3.2, 3.3, 3.5, 3.8, 3.8, 3.9, 4, 4.2, 4.4, 4.5, 5.1, 5.3, 5.3, 5.4,
      14, 17, 18, 19, 21, 21, 23, 25, 27, 28, 32, 34, 36, 41, 42, 44, 48, 49,
      51, 54, 59, 60, 61, 62, 80, 240)
hist(score, main='', xlab='CRP', breaks=20)

# 난수 생성
set.seed(2024)
rn <- c(rnorm(100, 5, 2), rnorm(100, 10, 2))
hist(rn)
hist(rn, breaks = 20, main='', xlab='value')
hist(rn, breaks = 5, main='', xlab='value')

# 점도표
age<-c(57, 61, 47, 57, 48, 58, 57, 61, 54, 50, 68, 51)
m.age <- mean(age)    
par(xpd=TRUE)
stripchart(age, axes=F, pch=19, xlim=c(45,70), method='stack', offset=5, cex=1.5)
axis(1, at=seq(45, 70, 5))
points(m.age, -5, pch=17, cex=2, col='blue')

books<-c(6, 0, 1, 3, 1, 5, 2, 3, 1, 3, 67)
m.books <- mean(books)    
par(xpd=TRUE)
stripchart(books, axes=F, pch=1, xlim=c(0,70), method='stack', offset=5, cex=1.5)
axis(1, at=seq(0, 70, 10))
points(m.books, -5, pch=17, cex=2, col='red')

age<-c(57, 61, 47, 57, 48, 58, 57, 61, 54, 50, 68, 51)
fivenum(age) # 다섯수치요약(최솟값, 1사분위수, 중앙값, 3사분위수, 최댓값)
boxplot(age, ylab='Age') # 상자그림

member<-c(92, 107, 180, 90, 78, 91, 102, 88, 106, 125, 95, 102, 162)
boxplot(member, ylab='Number of board members') 