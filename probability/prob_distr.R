# 1. 이산형 확률분포
# 주사위 던지기 
#install.packages('distrEx') 
library(distrEx)
X <- DiscreteDistribution(supp = c(1:6), prob = rep(1/6,6))
plot(X)

E(X) # 기댓값, 평균
var(X) # 분산
sd(X) # 표준편차

E(2*X+5) 
var(2*X+5) 
sd(2*X+5) 

# 2. 연속형 확률분포
# 2-1. 연속형 균등 분포
X <- Unif(1,5)
1-p(X)(2) # X>2
p(X)(4)-p(X)(2) # 2<X<4
plot(X) # 확률밀도함수, 누적분포함수
E(X) 
var(X) 

# 2-2. 지수분포
X <- Exp(3)
p(X)(1) # X<1
plot(X) 
E(X)
var(X)

# 2-3. 정규분포
X <- Norm(15,5)
p(X)(25) # X<25
plot(X) 
E(X)
var(X)

# 2-4. 감마분포
X <- Gammad(2,1/3)
p(X)(1) # X<1
plot(X, inner=c('pdf of Gamma(2, 3)', 'cdf of Gamma(2, 3)', 'Quantile ftn of Gamma(2, 3)')) 
E(X)
var(X)