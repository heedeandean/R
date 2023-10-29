# 기술 통계
setwd('/Users/hj/Downloads/rpy') 
score = read.csv('./score.csv', header=T)
head(score, 3)
dim(score)
score$total = score$midterm + score$final
head(score, 3)
sapply(score[,-c(1:2)], mean, na.rm=TRUE)
sapply(score[,-c(1:2)], sd, na.rm=TRUE)

summary(score[,-1]) # 첫 열(id) 제거
class(score$gender) # type
class(score$midterm) 
fivenum(score$total) # 다섯수치요약(최솟값, 제1사분위수, 중앙값, 제3사분위수, 최댓값)

install.packages('psych')
library(psych)
?describe
Dscore <- describe(score[,-c(1:2)], trim=.2) # 절사 평균 20% 
names(Dscore)

install.packages('moments')
library(moments)
skewness(score$midterm) # 왜도
kurtosis(score$midterm) # 첨도


## 그룹별 기술통계
head(score, 3)
# 1.
xbar <- tapply(score$total, score$gender, mean)
s <- tapply(score$total, score$gender, sd)
n <- tapply(score$total, score$gender, length)
cbind(mean=xbar, stdev=s, n=n)

# 2.
aggregate(score[c('midterm', 'final', 'total')], list(gender=score$gender), mean)

# 3.
by(score[,3:5], score$gender, summary)

# 4.
library(psych)
describeBy(score[,c(3:5)], score$gender)


## 연속형 변수 분포 파악
# 1. 줄기-잎 그림
score$total
stem(score$total)
stem(score$total, scale=2)

install.packages('aplpack')
library(aplpack)
maleScore <- score$total[score$gender=='m']
femaleScore <- score$total[score$gender=='f']
stem.leaf.backback(maleScore, femaleScore)
median(maleScore)
median(femaleScore)

# 2. 상자그림
# 안울타리(inner fence): Q1-1.5*IQR ~ Q3+1.5*IQR
par(mfrow=c(1,2))
boxplot(total ~ gender, data=score)
title('boxplot: total~gender')
boxplot(femaleScore, maleScore)
title('boxplot: variables')

# 3. 히스토그램
par(mfrow=c(2,1))
hist(maleScore)
hist(femaleScore, col='grey')


## 빈도표
enqete <- read.csv('./enqete.csv', header=T)
head(enqete, 3)
grade.freq <- table(enqete$grade)
names(grade.freq) <- c('gr1', 'gr2', 'gr3', 'gr4')
grade.freq

par(mfrow=c(1,2))
barplot(grade.freq)
pie(grade.freq)

# 분할표
table(enqete$grade, enqete$q1)
enqete[enqete==0] <- NA
enqete <- na.omit(enqete)
college <- table(enqete$grade, enqete$q1)
college
colnames(college) <- c('ans1', 'ans2')
rownames(college) <- c('grade1', 'grade2', 'grade3', 'grade4')
college
names(dimnames(college)) <- c('Grade', 'Answer')
college

xtabs(~grade+q1, data=enqete)
par(mfrow=c(1,2))
barplot(college)
barplot(t(college), legend=c('ans1', 'ans2'))

# 독립성 검정
chisq.test(college) # 두 변수가 관련 있다.(유의확률(p-value) 기준으로 판단)

# cf. 패키지명::함수