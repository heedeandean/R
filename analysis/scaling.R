# Data Scaling in R
# : 데이터 전처리, 각 컬럼의 분포를 맞춤

# Why? 변수의 값이 너무 작으면, 중요한 변수라 하더라도 
# 목적변수(y)에 영향을 미치지 못할 수 있다.

# 1. 표준화 
# 평균으로부터 어느정도 떨어졌는지 나타냄
# 평균 0 / 분산 1로 맞춤.

# 방법 1-1.
scale_model <- caret::preProcess(iris[, -5], 
                                 method = c('center', 'scale'))
iris_stand <- predict(scale_model, iris)
iris_stand

round(sapply(iris_stand[, -5], mean), 2) # 평균
round(sapply(iris_stand[, -5], sd), 2)

# 방법 2-2.
iris_stand2 <- iris
iris_stand2[, -5] <- scale(iris[, -5])

round(sapply(iris_stand2[, -5], mean), 2) # 평균
round(sapply(iris_stand2[, -5], sd), 2)  

# 방법 2-3. 직접 구현
iris_stand3 <- iris
iris_stand3[, -5] <- sapply(iris[, -5], 
                            function(x) {(x-mean(x))/sd(x)})

round(sapply(iris_stand3[, -5], mean), 2) # 평균
round(sapply(iris_stand3[, -5], sd), 2) 


# 2. 정규화 : 데이터 범위를 0~1로 변환

# 방법 2-1.
scale_model <- caret::preProcess(iris[, -5], method = 'range')
iris_range <- predict(scale_model, iris)
iris_range

summary(iris_range) # Species를 제외하고 모두 min=0/max=1

# 방법 2-2. 직접 구현
iris_range2 <- iris
iris_range2[, -5] <- sapply(iris[, -5], 
                            function(x) {(x-min(x))/(max(x)-min(x))})

summary(iris_range2)


