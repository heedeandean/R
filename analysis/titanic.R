# To Do! 목적변수(Survived:생존)를 예측 (with. 타이타닉 데이터)

rm(list=ls()) # 메모리 삭제
setwd('C:/Users/82102/OneDrive/바탕 화면/git/R/analysis/data/titanic')

library(tidyverse)

#====================================================
# 1. load data
train <- read.csv('train.csv', stringsAsFactors = F, fileEncoding = 'euc-kr',
                  strip.white = T) # 공백 없앰

test <- read.csv('test.csv', stringsAsFactors = F, fileEncoding = 'euc-kr',
                 strip.white = T) # 목적변수(Survived) X

#====================================================
# train + test
train$is_train <- TRUE
test$is_train <- FALSE
test$Survived <- NA

glimpse(test)

data <- rbind(train, test)

#====================================================
# 2. '' -> NA
data <- data %>% mutate_all(list(~na_if(., '')))
data %>% map_int(~sum(is.na(.x)))

# NA값 채우기 
# 2-1. 연속형 데이터(숫자) : 평균
age_mean <- mean(data$Age, na.rm = T)
data[is.na(data$Age), 'Age'] <- age_mean

fare_mean <- mean(data$Fare, na.rm = T)
data[is.na(data$Fare), 'Fare'] <- fare_mean

# 2-2. 범주형 데이터(factor) : 빈도가 가장 높은 값
# 주의. 범주가 너무 많은 경우는 PASS (ex. Cabin)

table(data$Embarked) # S
data[is.na(data$Embarked), 'Embarked'] <- 'S'

data %>% map_int(~sum(is.na(.x))) # NA값 처리 확인

#====================================================
# 3. EDA(Exploratory Data Analysis, 탐색적 자료 분석) : 시각화 

#====================================================
# 4. train/test
train <- data %>% filter(is_train == TRUE) 
test <- data %>% filter(is_train == FALSE)

# train => (train/validation) 

# install.packages('caret')
library(caret)

# 목적변수(Survived)에 따른 비율로 train(7)/valid(3) 분할
set.seed(1234)
train_ind <- createDataPartition(train$Survived, p = 0.7)$Resample1 

data.train <- train[train_ind, ]
data.valid <- train[-train_ind, ]

table(train$Survived) %>% prop.table() %>% round(2)
table(data.train$Survived) %>% prop.table() %>% round(2)
table(data.valid$Survived) %>% prop.table() %>% round(2)

train %>% 
  ggplot(aes(x=Survived, fill=as.factor(Survived))) +
  geom_density(alpha=0.5)

#====================================================
# 5. 모델링 : train 데이터 사용

# 설명변수 추출 : 범주가 넓은 변수 사용 X
glimpse(data.train)

# 모델이 자동으로 chr -> 더미 변환(ex. Sex, Embarked)

# model1. 선형 회귀
lm.model <- lm(Survived~PassengerId+Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,
               data = data.train)
summary(lm.model)

model1 <- step(lm.model, direction = 'both') 
summary(model1)


# model2. 로지스틱 회귀
logit.model <- glm(Survived~PassengerId+Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,
                   family = 'binomial', data = data.train) # family='gaussian' == 선형회귀

# 유의미한 변수 *
summary(logit.model)

# 유의미한 변수만 정리
# : 선택법(전진(forward), 후진(backward), 단계적(both))
model2 <- step(logit.model, direction = 'both') 
summary(model2)


# model3. 랜덤포레스트
# install.packages('randomForest')
library(randomForest)

set.seed(1234)
rf <- randomForest(Survived~PassengerId+Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,
                   data = data.train, ntree=200, importance=T)

# varImpPlot(rf, type=1, pch=19, col=1, cex=1, main='')
#====================================================
# 6. validation
pred1 <- predict(model1, newdata = data.valid, type = 'response')
pred1

pred2 <- predict(model2, newdata = data.valid, type = 'response')
pred2

pred3 <- predict(rf, newdata = data.valid)
pred3

# 최적의 cutoff 찾기
library(pROC)

rocobj <- roc(data.valid$Survived, pred1)
aa <- coords(rocobj, x="best")
cutoff1 <- aa$threshold
cutoff1 # 0.4113541 

rocobj <- roc(data.valid$Survived, pred2)
aa <- coords(rocobj, x="best")
cutoff2 <- aa$threshold
cutoff2 # 0.3552705

rocobj <- roc(data.valid$Survived, pred3)
aa <- coords(rocobj, x="best")
cutoff3 <- aa$threshold
cutoff3 # 0.3619579


# install.packages('e1071')
# 혼동행렬(예측값, 정답)
cm1 <- confusionMatrix(factor(if_else(pred1 > cutoff1, 1, 0)), factor(data.valid$Survived))
cm1 # No Information Rate : 찍었을 때

cm2 <- confusionMatrix(factor(if_else(pred2 > cutoff2, 1, 0)), factor(data.valid$Survived)) 
cm2 

cm3 <- confusionMatrix(factor(if_else(pred3 > cutoff3, 1, 0)), factor(data.valid$Survived))
cm3  

#====================================================
# 가장 좋은 모델은?

# RMSE(Root Mean Squared Error, 평균 제곱근 오차) : 작을수록 좋다.
RMSE(pred1, data.valid$Survived) # 0.3620276
RMSE(pred2, data.valid$Survived) # 0.3575956
RMSE(pred3, data.valid$Survived) # 0.349408

# ls(cm1)
# F1 높을수록 좋다. 
cm1$byClass[7] # 0.846395 
cm2$byClass[7] # 0.8444444 
cm3$byClass[7] # 0.863354 

# AUC
plot.roc(data.valid$Survived, pred1, print.auc=TRUE, # AUC : 0.879
         ci=FALSE, col="black", lty=2, print.thres=TRUE)

plot.roc(data.valid$Survived, pred2, print.auc=TRUE, # AUC : 0.878
         ci=FALSE, col="black", lty=2, print.thres=TRUE)

plot.roc(data.valid$Survived, pred3, print.auc=TRUE, # AUC : 0.882
         ci=FALSE, col="black", lty=2, print.thres=TRUE)

# => 랜덤 포레스트가 제일 좋은 성능을 낸다.
#====================================================
# 7. output : test 데이터
data.result.pred <- predict(rf, newdata = test)

result <- test %>% 
  select(PassengerId) %>% 
  cbind(Survived = if_else(data.result.pred > cutoff3, 1, 0))

write.csv(result, './result.csv') # 결과 제출


# 채점
sub <- read.csv('gender_submission.csv') # 정답
sub

data.result <- left_join(sub, result, by='PassengerId')
data.result %>% head()

RMSE(data.result$Survived.y, data.result$Survived.x) # 0.4402044 
# 결과는 별로 안좋다ㅠㅠ 
