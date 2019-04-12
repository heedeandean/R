# 문항 1.

### 1) '죽반과 매반의 수학성적은 차이가 없다' 라는 가설을 검증하시오.

# 1. 데이터 준비
load('../data/d.rda')
head(d)
library(dplyr)

jmmath <- d %>% filter(cls %in% c('죽', '매')) %>% select(cls, math)
head(jmmath)
jmmath$cls <- factor(jmmath$cls, levels = c('죽','매'), labels = c('죽', '매'))
jmmath$cls

# 2. 데이터 확인 (기술통계 + 그래프)
library(psych) 
describeBy(jmmath$math, jmmath$cls, mat = T)

boxplot(jmmath$math ~ jmmath$cls)
layout(matrix(c(1,1,2,3), 2, 2, byrow = T))
boxplot(jmmath$math ~ jmmath$cls)
hist(jmmath$math[jmmath$cls == '죽'])
hist(jmmath$math[jmmath$cls == '매'])
orgpar <- par(no.readonly = T)
par(orgpar)

# 3. 등분산 검정
var.test(jmmath$math ~ jmmath$cls, data = jmmath)

# 4. t-test 수행
t.test(jmmath$math ~ jmmath$cls, data = jmmath,
       alternative = c("two.sided"),
       var.equal = T,                 # 등분산검증의 p-value < 0.05 이면 False로!
       conf.level = 0.95)

# 5. 결과 그래프 
mu <- 59.4; se <- 1.975140; rn <- sort(rnorm(1000, mu, se))
plot(rn, dnorm(rn, mu, se), col = 'green', type = 'l', main = '평균점수',
     xlim = c(50, 80), ylim = c(0, 0.25)) 
abline(v = mu, col = "green", lty = 5)
par(new = T)  
mu <- 64.28; se <- 1.952381; rn <- sort(rnorm(1000, mu, se))
plot(rn, dnorm(rn, mu, se), col = 'red', type = 'l', main = '평균점수',
     xlim = c(50, 80), ylim = c(0, 0.25))
abline(v = mu, col = "red", lty = 5)


# ---------------------------------------------------------
### 2) 4개반 수학성적의 유사도(동질의 정도)를 분석하시오.

describeBy(d$math, d$cls, mat = T)

# 3. 그래프로 확인하기
library(ggplot2)
ggplot(d, aes(x = cls, y = math)) +
  geom_boxplot(outlier.color = 'blue') +
  ggtitle("각반 수학 성적")

ggplot(d, aes(x = math)) +
  geom_histogram(binwidth = 10, col = 'white') +
  facet_grid(. ~ d$cls)   # 그룹별로 그려라!

# 4-1. 등분산(분산의 동질성) 검정 (p-value > 0.05 면 등분산)
bartlett.test(d$math ~ d$cls, data = d)  # ⇒ p-value = 0.8497 ⇒ 약 85% 동질하다

aaa <- aov(d$math ~ d$cls, data = d)
summary(aaa)   

TukeyHSD(aaa)

plot(TukeyHSD(aaa)) 


draw <- function(rn, mu, se, col) {
  plot(rn, dnorm(rn, mu, se), col = col, type = 'l',
       xlim = c(50, 80), ylim = c(0, 0.25))
  abline(v = mu, col = col, lty = 5)
}

mu <- 62.6; se <- 2.097331; rn <- sort(rnorm(1000, mu, se))
draw(rn, mu, se, 'red')
par(new = T)
mu <- 59.4; se <- 1.975140; rn <- sort(rnorm(1000, mu, se))
draw(rn, mu, se, 'blue')
par(new = T)
mu <- 64.2833; se <- 1.9523; rn <- sort(rnorm(1000, mu, se))
draw(rn, mu, se, 'green')
par(new = T)
mu <- 66.6; se <- 1.964653; rn <- sort(rnorm(1000, mu, se))
draw(rn, mu, se, 'black')

legend('topright',
       legend = c('국', '난', '매', '죽'),
       pch = 8,
       col = c('red', 'blue', 'green', 'black'),
       bg = 'gray')


# ---------------------------------------------------------

# 문항 2.

### 1) 전교생의 국어성적과 영어성적에 대한 상관분석(Correlation)을 수행하시오.

# 1. 데이터 준비
cdata <- d %>% select(kor, math)
head(cdata)

# 2. 기술 통계 확인
describe(cdata)

# 3. 그래프로 데이터 확인하기
pairs.panels(cdata)          # 연관계수 확인

# 4. 상관분석
cor(cdata, use = "complete.obs",   # 결측치(NA) 제외
      method = c("pearson"))         # 모수 통계  cf. 비모수(30개 미만)의 경우 spearman)

# 5. 결과 그래프 
plot(kor ~ math, data = cdata)
abline(lm(kor ~ math, data = cdata), col = 'red') 

# ---------------------------------------------------------

### 2) mpg데이터의 displ, cyl, trans, cty, hwy 중
#      1999년과 2008년 두 해의 고객 만족도가 0과 1이라고 했을 때,
#      어떤 요소가 만족도에 많은 기여를 했는지 로지스틱 회귀분석하시오.

# 1. 데이터 준비 : 1999년과 2008년 두 해의 만족도가 0과 1 → 영향을 준 요인은??
unique(mpg$trans); unique(mpg$year);
cdata2 <- mpg %>%
            mutate(trs = ifelse(substr(trans, 1, 4) == 'auto', 1, 0), 
                   y = ifelse(year == 1999, 0, 1)) %>%
            select(y, displ, cyl, trs, cty, hwy)

# 2. 기본 통계치 확인
describe(cdata2)
pairs.panels(cdata2)

# 3. 분석
glmdata <- glm(y ~ displ + cyl + cty + hwy + trs, family = binomial, data = cdata2)
summary(glmdata)  # Estimate: 기울기(비례/반비례), Pr: 0.05보다 작으면 영향이 있다
plot(glmdata)

# 4. coefficients(기울기+절편)와 confint(신뢰구간)로 LOR(Log Odd Ratio) 구하기
round(exp(cbind(LOR = coef(glmdata), confint(glmdata))), 2)



