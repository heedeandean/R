# 1. 비계층적 군집분석 
# : 랜덤으로 데이터 군집화 (cf. 계층적 군집분석 : 순차적으로 데이터 군집화)
# = 확인적 군집분석 : 클러스터(그룹)의 중앙값을 계산하면서 적절한지 확인.
# 계속적인 확인으로 계층적 군집분석 보다 많은 연산 수행. 빠른 속도

# 1-1. k-means clustering - k(군집의 수)를 이미 알고 있을때 사용.
library(graphics)
# install.packages('flexclust')
library(flexclust)

data(nutrient)

kms <- kmeans(nutrient, 5) 
nrow(nutrient) # 27
kms # 27 = 6 + 11 + 3 + 6 + 1 : 5개의 군집의 각 개수   

kms$betweenss # 군집 간 분산 정도 - BEST 클수록 군집간 거리가 멀어짐
kms$withinss # 군집 내 분산 정도 - BEST 작을수록 군집이 잘 구분됨(밀집)

kms$totss # totss = betweenss + withinss 

# energy, fat 변수는 서로 강한 상관관계를 가짐
plot(nutrient, col = kms$cluster) # 변수 간 관계


# K 변화에 따른 withinss의 변화
within <- c()
for(i in 1:10) {
  within[i] <- sum(kmeans(nutrient, centers = i)$withinss)
}

# k가 증가할수록, withinss(군집 내 분산 정도)가 작아짐.
plot(1:10, within, type='b', 
     xlab='Number of Clusters', ylab='Within group sum of squares')


# K 변화에 따른 betweenss의 변화
between <- c()
for(i in 1:10) {
  between[i] <- sum(kmeans(nutrient, centers = i)$betweenss)
}

# k가 증가할수록, betweenss(군집 간 분산 정도)가 높아짐.
plot(1:10, between, type='b', 
     xlab='Nember of Clusters', ylab='between group sum of squares')

# K 변화에 따른 정확도의 변화
bet_ss <- c()
for(i in 1:10) {
  kms <- kmeans(scale(iris[, c(3,4)]), i)
  bet_ss[i] <- round(kms$betweenss / kms$totss *100, 1)
}

y_name = paste('between_ss', '\n', '/', '\n', 'total_ss', collapse = '')
par(oma=c(0, 1, 0, 0)) # 그래프 여백 조절(하, 좌, 상, 우)
par(mgp=c(1, 0.1, 0)) # 그래프 내 축 여백 조절(제목, 눈금, 지표선)

# k가 증가할수록, 정확도가 높아짐.
plot(1:10, bet_ss, type='b',
     xlab='Number of Clusters', ylab=y_name, ylim=c(0,100), las=1)


# 2. 혼합분포군집
library(ggplot2)
library(dplyr)

options(scipen = 999)

# 두개의 돌출부
p <- ggplot(faithful, aes(x = waiting)) + geom_density()
p

# eruptions(분출) : 분화 길이(분)
# waiting(대기) : 분출 사이의 시간(분)
str(faithful)
head(faithful)

p + 
  geom_vline(xintercept = 53, col = "red", size = 2) + 
  geom_vline(xintercept = 80, col = "blue", size = 2)

# 2-1. 가우시안 혼합 모델(GMM, Gaussian Mixture Model)
# install.packages('mixtools')
library(mixtools)

set.seed(1)
wait <- faithful$waiting

# 2개의 가우시안
mixmdl <- normalmixEM(wait, k=2)
mixmdl
mixmdl$mu # 평균
mixmdl$sigma # 분산

post.df <- as.data.frame(cbind(x=mixmdl$x, mixmdl$posterior))
head(post.df, 10)

post.df %>% 
  filter(x>66, x<68)

post.df %>% 
  mutate(label = ifelse(comp.1 > 0.8, 1, 2)) %>% # 임계값 설정 
  ggplot(aes(x=factor(label))) +
  geom_bar() +
  xlab('Component') +
  ylab('Number of Data Points')

# EM(Expectation Maximization) 알고리즘 
# : 군집 알고리즘
# 확률 기반 군집(Probability-based clustering) (cf. K-Means : 거리 기반 군집)

data(faithful)
attach(faithful)
hist(waiting, main='Time between Old faithful eruptions', 
     xlab='Minutes', ylab='',
     cex.main=1.5, cex.lab=1.5, cex.axis=1.4)

wait1 <- normalmixEM(waiting, lambda = .5, mu = c(55,80), sigma = 5)
summary(wait1)
head(wait1$posterior)

plot(wait1, density = TRUE, cex.axis=1.4, cex.lab=1.4, cex.main=1.8,
     main2='Time between Old faithful eruptions', xlab2='Minutes')

# install.packages('mclust')
library(mclust)
mc <- Mclust(iris[, 1:4], G=3)
summary(mc, parameters=TRUE)

plot.Mclust(mc)

mc$classification # 분류된 그룹
# predict(mc, data=iris)


