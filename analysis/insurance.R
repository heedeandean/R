# TO DO! charges(보험료) 예측
rm(list=ls())

library(tidyverse)
library(caret)

setwd('C:/Users/82102/OneDrive/바탕 화면/git/R/analysis/data/insurance/')

# 변수 설명
# age: 초기 수혜자 나이

# bmi: 체질량 지수, 신체, 키에 비해 상대적으로 높거나 낮은 체중에 대한 이해 제공, 신장 대 체중의 비율을 사용하여 체중의 객관적인 지수 (kg / m ^ 2), 이상적으로는 18.5 ~ 24.9

# children: 건강보험 적용 자녀 수 / 피부양자 수

# charges: 건강 보험에서 청구하는 개인 의료 비용

## 1. load data

# cf. read_csv
data <- read.csv('insurance_train.csv', stringsAsFactors = F, 
                 strip.white = T)
glimpse(data) # 1,074 X 8
head(data)

## 2. NA 
colSums(is.na(data)) 

## 3. EDA

# 독립변수와 설명변수의 관계
data %>% 
  ggplot(aes(x=children, y=charges)) +
  geom_point() +
  geom_smooth(method = 'lm') 

data %>% 
  ggplot(aes(smoker, charges)) +
  geom_point(aes(color=smoker), alpha = 0.7) +
  theme_light()
# => 흡연자가 보험료가 더 높다.


## 전처리
data %>% distinct(X) %>% nrow() # X는 단순변수
data <- data %>% select(-X)

glimpse(data)
# children은 양적변수다. (cf. 질적)


data %>% count(sex)
data %>% count(smoker)
data %>% count(region)


# [설명변수] factor, character형 -> 더미 변환
# data_recipe <- recipe(charges~., data = data) %>%
#   # step_rm(X) %>% 
#   step_dummy(all_predictors()) %>%
#   step_YeoJohnson(all_predictors()) %>%
#   prep()
# 
# foo <- bake(data_recipe, data)
# glimpse(foo)


## 4. train(7)/valid(3) 분할
set.seed(1234)
train_ind <- createDataPartition(data$charges, p = 0.7)$Resample1 

train <- data[train_ind, ]
valid <- data[-train_ind, ]


## 5. 모델링

####### 로지스틱회귀
set.seed(1234)
glm.model <- glm(charges~., data = train) 

glimpse(data)
# character형(sex, smoker, region) 자동으로 더미로 변환.
summary(glm.model)

# 변수선택법(전진(forward), 후진(backward), 단계적(both))
glm.model <- stats::step(glm.model, direction = 'both') 
summary(glm.model)

pred1 <- predict(glm.model, newdata = valid)


####### 랜덤포레스트
library(randomForest)

set.seed(1234)
rf <- randomForest(charges~., data = train, ntree=200, importance=T)

pred2 <- predict(rf, newdata = valid)


# RMSE(Root Mean Squared Error, 평균 제곱근 오차) : 작을수록 좋다.
RMSE(pred1, valid$charges) # 6062.137
RMSE(pred2, valid$charges) # 4957.965

# ============================
# RMSE 줄이는 법 생각하기(with. 랜덤포레스트)

# 병렬처리(Parallel Processing) : 빠른 처리
# install.packages("doParallel")
library(doParallel) # 주의 : caretEnsemble 패키지에서 에러남

getDoParWorkers() # 기본 코어 수
registerDoParallel(cores = 11) # 작업관리자 -> 성능 : 12(전체코어) - 1 = 11
getDoParWorkers()

# 랜덤포레스트(튜닝파라미터 : mtree 1개)

# 10-fold cross validation 을 5번 반복
set.seed(1234)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

set.seed(1234)
rf_fit <- train(charges~., data = train, method = "rf",
                trControl = fitControl, verbose = F)
rf_fit # mtree : 5 

rf.pred <- predict(rf_fit, newdata = valid) 

RMSE(rf.pred, valid$charges) # 4907.3


## 최적의 mtree
customGrid <- expand.grid(mtry = 1:10)

set.seed(1234)
rf_fit2 <- train(charges~., data = train, method = "rf", 
                 trControl = fitControl, tuneGrid = customGrid, verbose = F)
rf_fit2 # mtree : 4

rf.pred2 <- predict(rf_fit2, newdata = valid) 

RMSE(rf.pred2, valid$charges) # 4885.296


# 홀드아웃 : 데이터가 작을때 train/valid/test -> train/test 만 나눈다.
set.seed(1234)
rf_fit3 <- train(charges~., data = data, method = "rf", 
                 trControl = fitControl, tuneGrid = customGrid, verbose = F)
rf_fit3 # mtree : 4

# => 최종모델은 rf.fit3

# ========================
## output
test_raw <- read.csv('insurance_test.csv', stringsAsFactors = F, 
                     strip.white = T)

# 전처리
test <- test_raw %>% select(-X)

pred <- predict(rf_fit3, newdata = test)
result <- test_raw %>% select(X) %>% cbind(pred) 

write.csv(result, 'result_um.csv') # 결과 제출

## 채점
correct <- read.csv('correct.csv', stringsAsFactors = F, strip.white = T)

RMSE(result$pred, correct$charges) # 4994.266

# ===========================
# robust(튼튼한) : 이상치의 영향을 덜받음
# ===========================
# 앙상블 : 모델들을 합친다

# Stacking 
library(caretEnsemble)

# library(doParallel)
# detectCores() %>% registerDoParallel()

control <- trainControl(method="boot", number=10, savePredictions=TRUE) 
algorithmList <- c('rpart', 'glm', 'svmLinear','rf') #(의사 결정, 일반화 선형, 서포트 백터 머신 , 랜덤 포레스트)
set.seed(1234)
models <- caretList(charges~., data=train, trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)

# 부스팅 모델(메인 모델)과 스택을 통해 모델을 결합 
stackControl <- trainControl(method="boot", number=10, savePredictions=TRUE) 
stack.gbm <- caretStack(models, method="gbm", metric="RMSE", trControl=stackControl)
print(stack.gbm)
stack.pred <- predict(stack.gbm, newdata = valid) 
cat('RMSE : ',RMSE(as.numeric(stack.pred), as.numeric(valid$charges)))
cat('R2 : ',R2(as.numeric(stack.pred), as.numeric(valid$charges)))
