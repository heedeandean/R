# 표본분포: 통계량 분포

# 1. 이항분포
par(mfrow=c(2,2))
par(mar=c(3, 3, 2, 2)) 
p <- 0.1
X1 <- Binom(5, p)
X2 <- Binom(10, p)
X3 <- Binom(30, p)
X4 <- Binom(100, p)
plot(X1, cex.points=1, to.draw.arg=c('d'), mfColRow=FALSE,
     inner='B(5,0.1)', xlab='')
plot(X2, cex.points=1, to.draw.arg=c('d'), mfColRow=FALSE,
     inner='B(10,0.1)', xlab='')
plot(X3, cex.points=1, to.draw.arg=c('d'), mfColRow=FALSE,
     inner='B(30,0.1)', xlab='')
plot(X4, cex.points=1, to.draw.arg=c('d'), mfColRow=FALSE,
     inner='B(100,0.1)', xlab='')
# => 표본수가 커질수록, 정규분포에 근사.

####
# 2. 대수의 법칙
#install.packages('distrTeach')
library(distrTeach)
illustrateLLN(Distr = Unif(), main=NULL, withCover=F)
# => 표본수가 커질수록, 표본 평균이 모평균에 수렴.

####
# 3. 중심극한정리
#install.packages('TeachingDemos')
library(TeachingDemos)
clt.examp(2)
clt.examp(5)
clt.examp(30)
# => 표본수가 커질수록, 정규분포에 근사.