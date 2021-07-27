# TO DO! 목적변수(Potability) 예측

rm(list=ls())
library(tidyverse)

setwd('C:/Users/82102/OneDrive/바탕 화면/git/R/analysis/data/water/')

# =====================================
## 1. load data
data <- read.csv('data.csv', stringsAsFactors = F, strip.white = T)
glimpse(data) # 3,276 X 11
head(data)
# View(data)

# =====================================
## 2. NA  
colSums(is.na(data)) # ph/Sulfate/Trihalomethanes

# 연속형 변수 - 평균
data %>% glimpse()

data %>% distinct(ph) %>% nrow()
data %>% distinct(Sulfate) %>% nrow()
data %>% distinct(Trihalomethanes) %>% nrow()

ph_mean <- mean(data$ph, na.rm = T)
data[is.na(data$ph), 'ph'] <- ph_mean

sulfate_mean <- mean(data$Sulfate, na.rm = T)
data[is.na(data$Sulfate), 'Sulfate'] <- sulfate_mean

tri_mean <- mean(data$Trihalomethanes, na.rm = T)
data[is.na(data$Trihalomethanes), 'Trihalomethanes'] <- tri_mean

colSums(is.na(data)) 


data %>% distinct(X) %>% nrow() 
data <- data %>% select(-X) # 단순변수(X) 제거


## test 분리
train_raw <- data %>% filter(!is.na(Potability))
test <- data %>% filter(is.na(Potability))

nrow(data) == nrow(train_raw) + nrow(test)


## train_raw -> train(7)/valid(3)
library(caret)

set.seed(1234)
train_ind <- createDataPartition(train_raw$Potability, p = 0.7)$Resample1 

train <- train_raw[train_ind, ]
valid <- train_raw[-train_ind, ]

## 모델링

# 로지스틱 회귀
set.seed(1234)
glm.model <- glm(Potability~., data = train) 
summary(glm.model)

# 변수선택법(전진(forward), 후진(backward), 단계적(both))
glm.model <- stats::step(glm.model, direction = 'both') 
summary(glm.model)

pred1 <- predict(glm.model, newdata = valid)


library(pROC)
rocobj <- roc(valid$Potability, pred1, auc=TRUE)
cutoff <- coords(rocobj, x="best", input="threshold", 
                 best.method="youden")$threshold

pred1 <- if_else(pred1 > cutoff, 1, 0)


# 랜덤포레스트
library(randomForest)

set.seed(1234)
rf <- randomForest(Potability~., data = train, ntree=200, 
                   importance=T)

pred2 <- predict(rf, newdata = valid)

rocobj <- roc(valid$Potability, pred1, auc=TRUE)
rocobj$auc # 0.5462
cutoff <- coords(rocobj, x="best", input="threshold", 
                 best.method="youden")$threshold

pred2 <- if_else(pred2 > cutoff, 1, 0)

# RMSE(Root Mean Squared Error, 평균 제곱근 오차) : 작을수록 좋다.
RMSE(pred1, valid$Potability) # 0.6233674
RMSE(pred2, valid$Potability) # 0.5954994


# SOM(Self-Organizing Map)

library(kohonen)

x <- train_raw %>% select(-Potability) %>% as.matrix() # -목적변수, matrix형
y <- train_raw %>% select(Potability) %>% as.matrix() %>% as.factor() # 목적변수, factor형

# train(7), test(3) 분할
set.seed(1234)

training <- createDataPartition(train_raw$Potability, p = 0.7)$Resample1
Xtraining <- scale(x[training, ])
Xtest <- scale(x[-training, ],
               center = attr(Xtraining, "scaled:center"),
               scale = attr(Xtraining, "scaled:scale"))

trainingdata <- list(x = Xtraining, y = y[training])
testdata <- list(x = Xtest, y = y[-training])

mygrid = somgrid(5, 5, "hexagonal")

set.seed(1234)
som.water <- supersom(trainingdata, grid = mygrid)


## ###############################################################
## Situation 0: trainingdata 사용

som.prediction <- predict(som.water)
som.prediction

RMSE(as.numeric(som.prediction$predictions$y), as.numeric(y[training])) # 0

## ################################################################
## Situation 1: testdata 사용

som.prediction <- predict(som.water, newdata = testdata)

RMSE(as.numeric(som.prediction$predictions$y), as.numeric(y[-training])) # 0

# Situation 0-1 : y에 정답을 선언해줬기 때문일까?


## ################################################################
## Situation 2: 매핑을 기반으로 목적변수 예측

# 방법 1.
?predict
som.prediction <- stats::predict(som.water, newdata = testdata,
                          whatmap = 'measurements') # 에러 ㅠㅠ


# 방법 2.
som.prediction <- predict(som.water, newdata = testdata[1]) # 목적변수 제외하고 예측

RMSE(as.numeric(som.prediction$predictions$y), as.numeric(y[-training])) # 0.6255432


# 방법 3.

## list name이 없으면 whatmap에 명시적으로 선언해야 한다.
som.prediction <- predict(som.water, newdata = list(Xtest), whatmap = 1)

RMSE(as.numeric(som.prediction$predictions$y), as.numeric(y[-training])) # 0.6233674


## ###############################################################
## Situation 3: 원본 데이터에 없는 레이어에 대한 예측. 
# 해당 레이어에 대한 trainingdata를 주어야 함.

set.seed(1234)
som.water <- supersom(Xtraining, grid = mygrid) 
som.prediction <- predict(som.water, newdata = testdata,
                          trainingdata = trainingdata)

RMSE(as.numeric(som.prediction$predictions$y), as.numeric(y[-training])) # 0.6277115

# ========================

# 랜덤 포레스트 모델이 제일 좋았다..

## output 
write.csv(pred, 'result_um.csv')


# ================================
data_raw <- read.csv('water_potability.csv', stringsAsFactors = F, 
                     strip.white = T)
glimpse(data_raw) # 3,276 X 10
data %>% select(-X) -> data
glimpse(data)
data_raw %>% distinct(Hardness) %>% nrow()

test %>% nrow()
test %>% glimpse()

data_raw <- data_raw %>% mutate(aa = as.character(Hardness)) 
# data <- data %>% mutate(aa = as.character(Hardness)) 
test_join <- test %>% mutate(aa = as.character(Hardness)) 


correct <- inner_join(data_raw, data, by = 'aa')
correct <- inner_join(data_raw, test_join, by = 'aa')

nrow(correct)

correct <- correct %>% select(ends_with('.x')) 






