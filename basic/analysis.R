setwd('/Users/hj/git/R/basic/data/dataintro89/')

## 자료 읽기
ex8 <- read.table('ex8-1.txt', header = T)
head(ex8)
head(ex8, 3)

install.packages('readxl')
library(readxl)
?readxl
ex8_xls <- read_excel('ex8-1.xlsx', sheet = 1)
head(ex8_xls)

## 
names(ex8)

mean(ex8$salary)
attach(ex8)
mean(salary)

sd(ex8$salary)
ex8 <- ex8[,-1] # 첫 번째 열 제거
head(ex8)

# 이산형(sex, edu)
ex8$sex <- factor(ex8$sex, levels = c(1:2), labels = c('M', 'F'))
ex8$edu <- factor(ex8$edu, levels = c(1:3), labels = c('Middle', 'High', 'Univ.'))
head(ex8)

# 연속형 : 다섯숫자요약(최솟값, 제1사분위수, 중앙값, 제3사분위수, 최댓값), 평균
# 이산형 : 빈도수
summary(ex8) 

# 그룹별 기술통계량
tapply(ex8$salary, ex8$sex, mean)
tapply(ex8$salary, ex8$sex, sd)
tapply(ex8$salary, ex8$edu, mean)
tapply(ex8$salary, ex8$edu, sd)

table(ex8$sex) # 빈도표(이산형 변수)
table(ex8$edu) 

edu_freq <- table(ex8$edu)
edu_freq
barplot(edu_freq) # 막대 그래프
title('barplot of EDU')
pie(edu_freq, main = 'Pie plot of EDU') # 원 그래프

sex_edu <- list(ex8$sex, ex8$edu) 
sex_edu
mean_sal_sex_edu <- tapply(ex8$salary, sex_edu, mean)
mean_sal_sex_edu
colnames(mean_sal_sex_edu) <- c('중졸이하', '고졸', '대졸이상') # 열 이름
rownames(mean_sal_sex_edu) <- c('남', '여')
mean_sal_sex_edu

sex_edu <- table(ex8$sex, ex8$edu) # 분할표
summary(sex_edu) # 독립성 검정 결과
sex_edu
colnames(sex_edu) <- c('중졸이하', '고졸', '대졸이상') # 열 이름
rownames(sex_edu) <- c('남', '여')
sex_edu
barplot(sex_edu, main='성별 교육정도 막대그래프')

par(mfrow = c(1,2)) # 1행 2열로 분할
hist(ex8$salary) # 히스토그램
hist(ex8$salary, nclass = 4) # nclass: 계급의 수 

# 줄기-잎 그림 : 연속형 
stem(ex8$salary) 
stem(ex8$salary, scale = 2) # 0~4, 5~9

par(mfrow = c(1,1)) 
boxplot(salary~sex, data = ex8)
title('Boxplot of salary by sex')

plot(ex8$age, ex8$salary, col = 'BLUE', pch = 19) # 산점도
title('Scatter plot of (age, salary)')

plot(ex8$age, ex8$salary, type = 'n')
points(ex8$age[ex8$sex=='M'], ex8$salary[ex8$sex=='M'], pch = 17, col = 4)
points(ex8$age[ex8$sex=='F'], ex8$salary[ex8$sex=='F'], pch = 19, col = 2)
legend('bottomright', legend = levels(ex8$sex), pch = c(17, 19), col = c(4, 2))
legend(locator(1), legend = levels(ex8$sex), pch = c(17, 19), col = c(4, 2)) # 'bottomright' == locator(1)
title('Scatter plot of (age, salary) by Sex')

# 벡터 연산
x <- c(-10:10)
y <- 4*x+7
y
x
x[3] # 1부터 시작
x[1:5]
x1 <- x[x<0]
x1

# 데이터 프레임
x <- c('red', 'green', 'blue')
y <- c(1, 2, 3)
z <- c(4, 5, 6)
dframe <- data.frame(x, y, z)
dframe
dframe[1, 1]
names(dframe) # 변수명
dframe$y
dframe[, 2]

x.seq <- seq(-pi, pi, 1)
x.seq
round(x.seq, 4)
seq(-pi, pi, length = 10)


# 행렬 생성
x <- matrix(1, nrow = 4, ncol = 3)
x
x <- matrix(c(1:12), ncol = 4, byrow = T)
x

# 서브 행렬 추출
x[,c(1:3)]
x[c(1:3), -2]

# 행렬 연산
x <- matrix(c(1,2,3,5,6,7), nrow = 2, byrow = T)
x
t(x) # 전치 행렬
y <- matrix(c(1:4), nrow = 2)
y
t(x) %*% y # 행렬 곱

apply(x, 1, mean) # 1(행)
apply(x, 2, mean) # 2(열)

y <- matrix(c(1,2,3,4), ncol = 2, byrow = T)
y
t(y)

# function
sq_value <- function(x) {x*x}
sq_value(4)
sq_value <- function(x) {
  sv = x*x
  return(sv)
}
sq_value(4)

list_value <- function() {
  value = list(v1=1, v2=2, v3=3)
  return(value)
}
list_value()

# 정규분포(normal distribution)
plot(function(x) dnorm(x), -3, 3, main='정규분포')
pnorm(0)
?norm

# 정규분포를 따르는 난수(random number) 생성
rnorm(20) # 평균 0 / 표준편차 1 / 난수 20개 생성
rnorm(100, -5, 2.5) # 난수 개수, 평균, 표준편차

ran_norm <- rnorm(100)
ran_norm
mean(ran_norm)
sd(ran_norm)
hist(ran_norm)

# t-분포를 따르는 난수 생성
my_sample <- rt(50, 5) # 난수 개수, 자유도
my_sample
hist(my_sample)
stem(my_sample)

dev.new()
dev.off()

# 시계열 
co2
plot(co2)
lines(smooth(co2), col='BLUE')