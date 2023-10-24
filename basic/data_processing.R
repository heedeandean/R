setwd('/Users/hj/Downloads/Rfolder') # 작업 디렉토리 설정 
getwd() # 작업 디렉토리 확인

load('./wd.Rdata')
nwd = wd
head(nwd)
nwd[nwd$x2<0.11, 'x2'] = 99
nwd[nwd==99] = NA

head(nwd, n=5)
rowSums(is.na(nwd)) # 행별로 결측치 수
colSums(is.na(nwd)) # 열별로 결측치 수
mywd = na.omit(nwd) # 결측치 제거
head(mywd)

# 변수명 변경
names(nwd)[6] = 'ny'
names(nwd)
colnames(nwd) = c('a1', 'a2', 'a3', 'a4', 'a5', 'newy')
names(nwd)


## value labels
insurance = read.table('./insurance.txt', header=T)
head(insurance)

# 명목형(nominal data)
insurance$job = factor(insurance$job, levels=c(1:3),
                       labels=c('근로자', '사무직', '전문가'))
# 순서형
insurance$edu2 = ordered(insurance$edu, levels=c(1:5),
                         labels=c('무학', '국졸', '중졸', '고졸', '대졸'))

job.freq = table(insurance$job) # 빈도수
job.freq
barplot(job.freq)
title('막대그림: job')

install.packages('xlsx')
library(xlsx)

## recode
drug = read.xlsx('./drug.xlsx', 1)
head(drug)
drug$agr = drug$age
drug$agr[drug$agr>=20 & drug$agr<=40] = 1
drug$agr[drug$agr>40 & drug$agr<=60] = 2
drug$agr[drug$agr>60] = 3
drug[c(1,20,40,95),]

install.packages('car')
library(car)
drug$agr2 = drug$age
drug$agr2 = recode(drug$age, 'lo:40=1;40:60=2;60:hi=3')
drug[c(1,20,40,80),]
drug$agr2 = ordered(drug$agr2, levels=c(1:3),
                    labels=c('Lo~40', '40~60', '60~Hi'))
agr2.freq = table(drug$agr2)
agr2.freq
barplot(agr2.freq, main='막대그림')

select1 = insurance[insurance$sex=='m',]
head(select1, n=3)

select2 = insurance[which(insurance$sex=='f'&insurance$job==2),]
select2

##
library(dplyr)
dim(insurance)
tbl_df(insurance) # 한 페이지에 요약








