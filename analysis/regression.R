setwd('/Users/hj/Downloads/rpy') 
health = read.csv('health.csv', header=T)

# 산점도 
plot(health$weight, health$time, pch=19) 
pairs(health[,-1], pch=19) # id열 제거

# 상관 분석: 상관계수 0 => 선형관계 X 
cor(health[,-1]) # 상관계수 행렬

x <- health[,c(2:5)]
cor(x, health$time)

cor.test(health$weight, health$time)

# 회귀 분석
health = health[,-1]
fit = lm(time~., data=health)
summary(fit)
anova(fit) # 분산분석표

head(cbind(fitted(fit), residuals(fit)))
confint(fit, level=0.95) # 회귀계수의 신뢰구간