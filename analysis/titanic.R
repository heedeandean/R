# To Do!
# 타이타닉 데이터를 이용하여 
# [선형 회귀/로지스틱 회귀/랜덤포레스트] 중 좋은 모델을 선별해
# 목적변수(Survived:생존)를 예측

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

# train -> (train/validation) 

# install.packages('caret')
library(caret)

# 지정 변수(Survived)에 따른 비율로 나눠줌
train_ind <- createDataPartition(train$Survived, p = 0.7)$Resample1 # 7(train):3(valid)

data.train <- train[train_ind, ]
data.valid <- train[-train_ind, ]

table(train$Survived) %>% prop.table() %>% round(2)
table(data.train$Survived) %>% prop.table() %>% round(2)
table(data.valid$Survived) %>% prop.table() %>% round(2)

train %>% 
  ggplot(aes(x=Survived, fill=as.factor(Survived))) +
  geom_density(alpha=0.5)

#====================================================
# 5. 모델링 : train 데이터

# 설명변수 추출 : 범주가 넓거나 필요없는 변수는 쓰지 않는다.
glimpse(train)

# model1. 선형 회귀
lm.model <- lm(Survived~PassengerId+Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,
               family = 'binomial', data = data.train)
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

set.seed(42)
rf <- randomForest(Survived~PassengerId+Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,
                   data = train, ntree=200, importance=T)
summary(rf)
varImpPlot(rf, type=1, pch=19, col=1, cex=1, main='')

predictions <- predict(rf, newdata = data.result)


#====================================================
# 6. validation : validation 데이터
data.valid$Survived # 정답

pred1 <- predict(model1, newdata = data.valid, type = 'response')
pred1

pred2 <- predict(model2, newdata = data.valid, type = 'response')
pred2

# install.packages('e1071')
# 혼동행렬(예측값, 정답)
cm1 <- confusionMatrix(factor(if_else(pred1 > 0.5, 1, 0)), factor(data.valid$Survived))
cm1 # Accuracy : 0.8165 (cf. No Information Rate : 찍음)

cm2 <- confusionMatrix(factor(if_else(pred2 > 0.5, 1, 0)), factor(data.valid$Survived)) 
cm2 # Accuracy : 0.8015

ls(cm2)
cm2$byClass # F1 높을수록 좋다. 

#====================================================
# 7. output : Accuracy가 더 높은 선형회귀모델 선택

sub <- read.csv('gender_submission.csv') # 정답
sub

data.result <- inner_join(test, sub, by='PassengerId')
data.result

data.result.pred <- predict(model1, newdata = data.result, type='response')
data.result.pred

confusionMatrix(factor(if_else(data.result.pred > 0.5, 1, 0)), 
                factor(data.result$Survived.y)) # Accuracy : 0.9713  
#====================================================