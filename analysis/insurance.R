# TO DO! charges(보험료) 예측
rm(list=ls())

library(tidymodels)
library(caret)

setwd('C:/Users/82102/OneDrive/바탕 화면/git/R/analysis/data/insurance/')

# 변수 설명
# age: 초기 수혜자 나이

# bmi: 체질량 지수, 신체, 키에 비해 상대적으로 높거나 낮은 체중에 대한 이해 제공, 신장 대 체중의 비율을 사용하여 체중의 객관적인 지수 (kg / m ^ 2), 이상적으로는 18.5 ~ 24.9

# children: 건강보험 적용 자녀 수 / 피부양자 수

# charges: 건강 보험에서 청구하는 개인 의료 비용

## 1. load data
data <- read.csv('insurance_train.csv', stringsAsFactors = F, 
                 strip.white = T)
glimpse(data) # 1,074 X 8
head(data)


## 2. NA 
colSums(is.na(data)) 

## 3. EDA

# 1) 독립변수와 설명변수의 관계
x <- data %>% 
  ggplot(aes(x = age, y = charges)) +
  geom_jitter(color = 'blue', alpha = 0.5) +
  theme_light()

y <- data %>% 
  ggplot(aes(x= bmi, y = charges)) +
  geom_jitter(color = 'green', alpha = 0.5) +
  theme_light()

# install.packages('cowplot')
library(cowplot)

p <- plot_grid(x, y)
title <- ggdraw() + draw_label('charges와 age/bmi의 상관관계', 
                               fontface = 'bold')
plot_grid(title, p, ncol=1, rel_heights = c(0.1, 1))
# 해석 : age, bmi가 올라가면 charges도 올라가는 추세다.


x <- data %>% 
  ggplot(aes(x=sex, y=charges)) +
  geom_jitter(aes(color=sex), alpha = 0.7) +
  theme_light()

y <- data %>% 
  ggplot(aes(x=children, y=charges)) +
  geom_jitter(aes(color=children), alpha = 0.7) +
  theme_light()

p <- plot_grid(x, y)
title <- ggdraw() + draw_label('charges와 sex/children의 상관관계',
                               fontface = 'bold')
plot_grid(title, p, ncol=1, rel_heights = c(0.1, 1))
# 해석 
# : charges와 age 사이에는 명백한 연관성이 없다.
# : children 4~5명은 보험료가 인하되는 것으로 보인다.


x <- data %>% 
  ggplot(aes(smoker, charges)) +
  geom_jitter(aes(color=smoker), alpha = 0.7) +
  theme_light()

y <- data %>% 
  ggplot(aes(region, charges)) +
  geom_jitter(aes(color=region), alpha = 0.7) +
  theme_light()

p <- plot_grid(x, y)
title <- ggdraw() + draw_label('charges와 smoker/region의 상관관계',
                               fontface = 'bold')
plot_grid(title, p, ncol=1, rel_heights = c(0.1, 1))
# 해석
# : 흡연자(smoker)가 비흡연자보다 charges가 더 높다.
# : charges와 region 사이에는 명백한 연관성이 없다.

# data <- data %>% 
#   mutate_if(is.character, list(as.factor)) %>% 
#   mutate_if(is.factor, list(as.numeric))


# 2) 전체 설명 변수
# library(PerformanceAnalytics)
# chart.Correlation(data[-7], histogram=TRUE, pch=1)



## 전처리
glimpse(data)

#
data %>% distinct(X) %>% nrow() # X는 단순변수
data <- data %>% select(-X)

# 설명변수 factor -> 더미 변환하면 모델 성능이 더 좋아질까? -> NO

# factor형 설명변수는?
# data %>% count(sex) 
# data %>% count(children) 
# data %>% count(smoker) 
# data %>% count(region) 
# 
# foo <- data %>% 
#   mutate_at(vars(sex, children, smoker, region), list(as.factor)) 
# 
# foo %>% glimpse()
# 
# data_recipe <- recipe(charges~., data = foo) %>% 
#   step_rm(X) %>% 
#   step_dummy(all_predictors()) %>% 
#   step_YeoJohnson(all_predictors()) %>% 
#   prep()
# 
# foo <- bake(data_recipe, foo)
# glimpse(foo)
# 
# data <- foo


## 4. train(6)/valid(4) 분할
set.seed(1234)
train_ind <- createDataPartition(data$charges, p = 0.6)$Resample1 

train <- data[train_ind, ]
valid <- data[-train_ind, ]


## 5. 모델링

######## 선형회귀
lm.model <- lm(charges~., data = train)

glimpse(data)
# character형(sex, smoker, region) 자동으로 더미로 변환.
summary(lm.model)

# 변수선택법(전진(forward), 후진(backward), 단계적(both))
lm.model <- stats::step(lm.model, direction = 'both') 
summary(lm.model)

pred1 <- predict(lm.model, newdata=valid)


# valid %>% 
#   ggplot(aes(x=lm.pred, y=charges)) +
#   geom_point(col = 'blue', alpha = 0.7) +
#   geom_abline(col = 'red') +
#   labs(title = '예측 값 vs 실제 값', x = '예측 값') 


####### 랜덤포레스트
library(randomForest)

set.seed(1234)
rf <- randomForest(charges~., data = train, ntree=200, importance=T)

pred2 <- predict(rf, newdata = valid)


# RMSE(Root Mean Squared Error, 평균 제곱근 오차) : 작을수록 좋다.
RMSE(pred1, valid$charges) # 5579.271
RMSE(pred2, valid$charges) # 4411.095


# ============================
# RMSE 줄이는 법 생각하기(with. 랜덤포레스트)

# 병렬처리(Parallel Processing) : 빠른 처리
# install.packages("doParallel")
library(doParallel)

getDoParWorkers() # 기본 코어 수
registerDoParallel(cores = 3)
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

RMSE(rf.pred, valid$charges) # 4293.502


## 최적의 mtree
customGrid <- expand.grid(mtry = 1:10)

set.seed(1234)
rf_fit2 <- train(charges~., data = train, method = "rf", 
                 trControl = fitControl, tuneGrid = customGrid, verbose = F)
rf_fit2 # mtree : 4

rf.pred2 <- predict(rf_fit2, newdata = valid) 

RMSE(rf.pred2, valid$charges) # 4273.042


# 최종모델은 rf.fit2

## output

# 해야할 전처리
data <- data %>% select(-X)
