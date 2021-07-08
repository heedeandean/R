# 1개의 목표변수와 26개의 설명변수
# 목표변수는 1,2,3,4,5,6,7 7개의 요인을 가지고 있다.

library(tidyverse)

rm(list=ls())
setwd('C:/Users/82102/OneDrive/바탕 화면/git/R/analysis/data')

# load data
data <- read.csv('data_raw.csv', stringsAsFactors = F, fileEncoding = 'euc-kr',
                 strip.white = T)
glimpse(data) # 1,941 X 27
head(data)
summary(data)
str(data)

# NA값 확인
data <- data %>% mutate_all(list(~na_if(., '')))
data %>% map_int(~sum(is.na(.x)))

# 1. EDA(탐색적 데이터 분석)을 하고 상관분석을 실시하고 분석에 필요한 파생변수를 선별하시오.
# % 시각화와 통계량을 제시할 것.

# EDA
data <- data %>% 
  mutate_if(is.character, list(as.factor))

data %>% 
  mutate_if(is.factor, list(as.numeric)) %>% 
  rowid_to_column() %>% 
  pivot_longer(-c(rowid)) %>% 
  ggplot(aes(value, fill=name)) +
  geom_histogram() +
  facet_wrap(~name, scales = 'free') +
  theme(legend.position = 'none')
# Luminosity_Index, Maximum_of_Luminosity 변수는 정규분포를 따른다.
# Fault 변수는 7개의 변수를 가진다.
# SteelType 변수는 2개의 변수를 가진다.
# 그래프를 보면 왜도의 치우친 정도를 알 수 있다.

# 상관분석
library(corrplot)

# 상관분석 할 수 있는 숫자형만 선택.
data.cor <- data %>% select_if(is.numeric) %>% cor()
corrplot(data.cor)

library(caret)
high_cor_column <- data.cor %>% findCorrelation(., cutoff = 0.7)

data_high <- data %>% select(-all_of(high_cor_column)) # 강한 상관관계변수 제거
bar <- data_high %>% select_if(is.numeric) %>% cor()
corrplot(bar) 

# 파생변수 선별 - 주성분 분석
data.pca <- prcomp(data[, -(26:27)], center = T, scale = T)
plot(data.pca, type='l')
summary(data.pca) # Cumulative Proportion 


PCA <- as.matrix(data[, -(26:27)]) %*% data.pca$rotation
data.result <- cbind(data_high, data.frame(PCA[, 1:8])) # PC8까지 변수 선택
head(data.result)


# 2-1. 데이터를 훈련용(5) 테스트용(3) 평가용(2)으로 나누고 시각화 할 것.
# % 시각화와 통계량을 제시할 것.
train_idx <- createDataPartition(data.result$Fault, p=0.5)$Resample1
data.train <- data.result[train_idx, ]

data.rest <- data.result[-train_idx, ]
valid_idx <- createDataPartition(data.rest$Fault, p=0.6)$Resample1
data.valid <- data.rest[valid_idx, ]
data.test <- data.rest[-valid_idx, ]

table(data.result$Fault) %>% prop.table() %>% round(2)
table(data.train$Fault) %>% prop.table() %>% round(2)
table(data.valid$Fault) %>% prop.table() %>% round(2)
table(data.test$Fault) %>% prop.table() %>% round(2)

data.train %>% 
  ggplot(aes(x=Fault, fill=Fault)) +
  geom_density(alpha=0.5)

data.valid %>% 
  ggplot(aes(x=Fault, fill=Fault)) +
  geom_density(alpha=0.5)

data.test %>% 
  ggplot(aes(x=Fault, fill=Fault)) +
  geom_density(alpha=0.5)

nrow(data.train)
nrow(data.valid)
nrow(data.test)


# 2-2. 목표변수가 1이나올지 안나올지에 대하여 알고싶다.
# 목표변수를 이항변수로 바꾸고 로지스틱 회귀분석을 실시하고
# confusionMatrix를 확인하고 최적의 cut off value 정하여라.
# % 시각화와 통계량을 제시할 것.

data.train.binary <- data.train %>% 
  mutate(Fault = if_else(as.numeric(Fault) == 1, 1, 0)) 

data.valid.binary <- data.valid %>% 
  mutate(Fault = if_else(as.numeric(Fault) == 1, 1, 0)) 

data.test.binary <- data.test %>% 
  mutate(Fault = if_else(as.numeric(Fault) == 1, 1, 0)) 

# 로지스틱 회귀
logit.model <- glm(Fault~., family = 'binomial', data = data.train.binary)

# 유의미한 변수 *
summary(logit.model)

# 유의미한 변수만 정리
# : 선택법(전진(forward), 후진(backward), 단계적(both))
logit.model.result <- step(logit.model, direction = 'both') 
summary(logit.model.result)

# 최적의 cut off 찾기(valid 사용)
pred.valid <- predict(logit.model.result, newdata = data.valid.binary, type = 'response')

library(pROC)
rocobj <- roc(data.valid.binary$Fault, pred.valid)
cutoff <- as.numeric(coords(rocobj, "best")[1])

# coords(rocobj, x="best", input="threshold", best.method="youden")

plot.roc(data.valid.binary$Fault, pred.valid, print.auc=TRUE, # AUC : 0.828
         ci=FALSE, col="black", lty=2, print.thres=TRUE)

# test
pred.test <- predict(logit.model.result, newdata = data.test.binary, type = 'response')
pred.test

logit.cm <- confusionMatrix(factor(if_else(pred.test > cutoff, 1, 0)), 
                            factor(data.test.binary$Fault)) 
logit.cm 


# 2-3. 로지스틱 분석을 제외하고 SVM을 포함하여 3개의 다항분류모형을 선정하고
# confusionMatrix를 확인하고 최적의 cut off value 를 정하여라.
# % 시각화와 통계량을 제시할 것.

library(e1071)
# sv <- svm(factor(Fault)~., data = data.train.binary)
sv <- svm(Fault~., data = data.train.binary)

# 최적의 cut off 찾기(valid 사용)
pred.valid <- predict(sv, newdata = data.valid.binary, type = 'response')

rocobj <- roc(data.valid.binary$Fault, pred.valid)
cutoff <- as.numeric(coords(rocobj, "best")[1])

# coords(rocobj, x="best", input="threshold", best.method="youden")

plot.roc(data.valid.binary$Fault, pred.valid, print.auc=TRUE, # AUC : 0.87
         ci=FALSE, col="black", lty=2, print.thres=TRUE)

# test
pred.test <- predict(sv, newdata = data.test.binary)
pred.test

sv.cm <- confusionMatrix(factor(if_else(pred.test > cutoff, 1, 0)), 
                         factor(data.test.binary$Fault)) 



library(randomForest)

set.seed(42)
rf <- randomForest(Fault~., data = data.train.binary, ntree=200, importance=T)
# rf <- randomForest(factor(Fault)~., data = data.train.binary, ntree=200, importance=T)
varImpPlot(rf, type=1, pch=19, col=1, cex=1, main='')


# 최적의 cut off 찾기(valid 사용)
pred.valid <- predict(rf, newdata = data.valid.binary, type = 'response')

rocobj <- roc(data.valid.binary$Fault, pred.valid)
cutoff <- as.numeric(coords(rocobj, "best")[1])

plot.roc(data.valid.binary$Fault, pred.valid, print.auc=TRUE, # AUC : 0.933
         ci=FALSE, col="black", lty=2, print.thres=TRUE)

# test
pred.test <- predict(rf, newdata = data.test.binary, type = 'response')
pred.test

rf.cm <- confusionMatrix(factor(if_else(pred.test > cutoff, 1, 0)), 
                         factor(data.test.binary$Fault)) 
rf.cm 


library(party)
ct <- ctree(Fault~., data = data.train.binary)

# 최적의 cut off 찾기(valid 사용)
pred.valid <- predict(ct, newdata = data.valid.binary, type = 'response')

rocobj <- roc(data.valid.binary$Fault, pred.valid)
cutoff <- as.numeric(coords(rocobj, "best")[1])

plot.roc(data.valid.binary$Fault, pred.valid, print.auc=TRUE, # AUC : 0.811
         ci=FALSE, col="black", lty=2, print.thres=TRUE)

# test
pred.test <- predict(ct, newdata = data.test.binary, type = 'response')
pred.test

ct.cm <- confusionMatrix(factor(if_else(pred.test > cutoff, 1, 0)), 
                         factor(data.test.binary$Fault)) 
ct.cm 


# 또 모델은 어떤 것이 있을까?


# 2-4. 위에서 실시한 총 4개의 모형중에 가장 적합한 모형을 활용하여
# 군집분석을 실시하고 F1 스코어값을 구하시오.
# % 시각화와 통계량을 제시할 것.

# 가장 적합한 모형은 AUC가 0.933으로 가장 높은 랜덤포레스트.

# 군집분석
data.result.binary <- data.result %>% 
  mutate(Fault = if_else(as.numeric(Fault) == 1, 1, 0)) 

data.scaled <- data.result.binary %>% select_if(is.numeric)
data.scaled <- data.frame(scale(data.scaled))
head(data.scaled)

set.seed(1004)
data.kmeans <- kmeans(data.scaled, centers = 2)

names(data.kmeans)
data.result.binary$cluster <- data.kmeans$cluster
head(data.result.binary)

train_idx <- createDataPartition(data.result.binary$Fault, p=0.7)$Resample1
data.train <- data.result.binary[train_idx, ]
data.test <- data.result.binary[-train_idx, ]

set.seed(42)
# rf <- randomForest(Fault~., data = data.train, ntree=200, importance=T)
rf <- randomForest(factor(Fault)~., data = data.train, ntree=200, importance=T)
varImpPlot(rf, type=1, pch=19, col=1, cex=1, main='')

# test
pred.test <- predict(rf, newdata = data.test, type = 'response')
pred.test

rf.cm <- confusionMatrix(pred.test, factor(data.test$Fault)) 
rf.cm 

rf.cm$byClass[7]









