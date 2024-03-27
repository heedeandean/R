# 주사위 던지기 이산형 확률 분포.
install.packages('distrEx') 
library(distrEx)
X <- DiscreteDistribution(supp = c(1:6), prob = rep(1/6,6))
plot(X)

E(X) # 기댓값
var(X) # 분산
sd(X) # 표준편차

E(2*X+5) 
var(2*X+5) 
sd(2*X+5) 