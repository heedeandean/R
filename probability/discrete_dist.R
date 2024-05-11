# 이산형 확률 분포: 셀 수 있음. / 확률질량함수(probability mass function)
library(distrEx)

####
# 1. 이항 분포
X <- Binom(4, 0.8)
d(X)(2)
E(X) # 기댓값
var(X) # 분산

####
# 2. 포아송 분포: 희귀한 사건
X <- Pois(1)
1-p(X)(2)
# cf. 이항 분포
X1 <- Binom(1000, 0.001)
1-p(X1)(2) # 포아송 분포와 값 유사.

E(X) 
var(X) 

####
# 3. 초기하 분포: 표본을 비복원추출
X <- Hyper(2,2,2)
d(X)(0)
d(X)(1)
d(X)(2)

E(X) 
var(X) 