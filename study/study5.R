# 1) '죽반과 매반의 수학성적은 차이가 없다' 라는 가설을 검증하시오.

# 1. 데이터 준비

d
jmmath = d %>% filter(cls %in% c('죽', '매')) %>% select(cls, math)
head(jmmath)
jmmath$cls = factor(jmmath$cls, levels=c('죽','매'), labels=c('죽', '매'))
jmmath$cls

# 2. 데이터 확인 (기술통계 + 그래프)

library(psych) 
describeBy(jmmath$math, jmmath$cls, mat = T)

boxplot(jmmath$math ~ jmmath$cls)
layout(matrix(c(1,1,2,3), 2, 2, byrow = T))
boxplot(jmmath$math ~ jmmath$cls)
hist(jmmath$math[jmmath$cls == '죽'])
hist(jmmath$math[jmmath$cls == '매'])
orgpar = par(no.readonly = T)
par(orgpar)

# 3. 등분산 검정

var.test(jmmath$math ~ jmmath$cls, data = jmmath)

# 4. t-test 수행

t.test(jmmath$math ~ jmmath$cls, data = jmmath,
       alternative = c("two.sided"),
       var.equal = T,                 # 등분산검증의 p-value < 0.05 이면 False로!
       conf.level = 0.95)

# 5. 결과 그래프 

mu = 59.4; se = 1.975140; rn = sort(rnorm(1000, mu, se))
plot(rn, dnorm(rn, mu, se), col='green', type = 'l', main = '평균점수',
     xlim = c(50, 80), ylim = c(0, 0.25)) 
abline(v=mu, col="green", lty=5)
par(new = T)  
mu = 64.28; se = 1.952381; rn = sort(rnorm(1000, mu, se))
plot(rn, dnorm(rn, mu, se), col='red', type = 'l', main = '평균점수',
     xlim = c(50, 80), ylim = c(0, 0.25))
abline(v=mu, col="red", lty=5)



# 2)  4개반 수학성적의 유사도(동질의 정도)를 분석하시오.




# 3) 전교생의 국어성적과 영어성적에 대한 상관분석(Correlation)을 수행하시오.

# 1. 데이터 준비

cdata <- d %>% select(kor, math)
head(cdata)

# 2. 기술 통계 확인

describe(cdata)

# 3. 그래프로 데이터 확인하기

pairs.panels(cdata)          # 연관계수 확인





