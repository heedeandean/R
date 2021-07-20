rm(list=ls())

library(tidymodels)
library(caret)

setwd('C:/Users/82102/OneDrive/바탕 화면/git/R/analysis/data/heartattack/')

# 변수 설명
# 1. 나이 : 사람의 나이
# 
# 2. 성별 : 성별 (1 = 남성, 0 = 여성)
# 
# 3. cp : 경험 한 흉통 유형 (값 1 : 전형적인 협심증, 값 2 : 비정형 협심증, 값 3 : 비 협심증 통증, 값 4 : 무증상)
# 
# 4. trestbps : 안정 혈압 (입원시 mm Hg)
# 
# 5. chol : mg / dl 단위의 콜레스테롤 측정
# 
# 6. fbs : 공복 혈당 (> 120mg / dl 인 경우, 1 = 참, 0 = 거짓)
# 
# 7. restecg : 휴식중인 심전도 측정 (0 = 정상, 1 = ST-T 파 이상, 2 = Estes의 기준에 따라 가능한 또는 확실한 좌심실 비대 표시)
# 
# 8. 탈락 : 최대 심박수 달성
# 
# 9. exang : 운동으로 인한 협심증 (1 = 예, 0 = 아니오)
# 
# 10. oldpeak : 휴식에 비해 운동으로 유발 된 ST 우울증 ( 'ST'는 ECG 플롯의 위치와 관련됨)
# 
# 11. 기울기 : 최고 운동 ST 세그먼트의 기울기 (값 1 : 상승, 값 2 : 평평, 값 3 : 하향)
# 
# 12. ca : 주요 선박 수 (0–3)
# 
# 13. thal : 지중해 빈혈이라고 불리는 혈액 질환 (1 = 정상, 2 = 고정 결함, 3 = 가역적 결함)
# 
# 14. 목표 : 심장병 (0 = 아니오, 1 = 예)


## 1. load data

data <- read.csv('train.csv', stringsAsFactors = F, strip.white = T)
# fileEncoding = 'euc-kr' : 한글일 경우만 사용 

glimpse(data) # 243 X 15
head(data)


## 2. NA 
colSums(is.na(data)) 


## 3. EDA

# 상관분석
library(corrplot)
corrplot(cor(data), order = 'hclust') # 0.7 이상부터 높은 관계 


# 설명변수 분포
data %>% 
  select(-output) %>% 
  rowid_to_column() %>% 
  pivot_longer(-c(rowid)) %>% 
  ggplot(aes(x=value)) +
  geom_density() +
  facet_wrap(~name, scales = 'free_y')
# => 정규분포가 아닌 변수가 많다. 

# H0(귀무 가설) : 정규분포이다.
# H1(대립 가설) : 정규분포가 아니다.

# 정규분포 (p-value < 0.05 => 귀무가설 기각)
shapiro.test(data$caa) # 정규분포가 아니다.
shapiro.test(data$restecg) # 정규분포가 아니다.


# 목적변수와 설명변수와의 관계 
data %>% 
  rowid_to_column() %>% 
  pivot_longer(-c(rowid, output)) %>% 
  ggplot(aes(x=as.factor(output), y=value)) +
  geom_boxplot() +
  facet_wrap(~name, scale='free_y')

## 4. 데이터 전처리
data.backup <- data

# [목적변수] factor형 변환
data <- data %>% 
  mutate(output = factor(output, labels=c('no_attack', 'attack')))

# factor(3요인 이상)형 변환
data %>% count(cp)
data %>% count(restecg)
data %>% count(slp)
data %>% count(caa)
data %>% count(thall)

data <- data %>% 
  mutate_at(vars(cp, restecg, slp, caa, thall), list(as.factor)) 

# [설명변수] factor -> 더미 변환
data %>% distinct(X) %>% nrow() # X는 단순변수

data_recipe <- recipe(output~., data = data) %>% 
  step_rm(X) %>% 
  step_dummy(all_predictors()) %>% 
  step_YeoJohnson(all_predictors()) %>% 
  prep()

data <- bake(data_recipe, data)
glimpse(data)


## 5. train(6)/valid(4) 분할
set.seed(1234)
train_ind <- createDataPartition(data$output, p = 0.6)$Resample1 

train <- data[train_ind, ]
valid <- data[-train_ind, ]

table(data$output) %>% prop.table() %>% round(2)
table(train$output) %>% prop.table() %>% round(2)
table(valid$output) %>% prop.table() %>% round(2)


train %>% 
  rowid_to_column() %>% 
  pivot_longer(-c(rowid, output)) %>% 
  ggplot(aes(x=output, y=value)) +
  geom_boxplot() +
  facet_wrap(~name, scales = 'free_y')


## 5. 모델링 (cf. 분류문제는 선형회귀 X)



library(pROC)

###### 교차검증 - 최적 모델 찾기
n <- 10

set.seed(1234)
folds <- createFolds(data$output, k=n, list=T, returnTrain=F)

#glm
auc_result <- c()

set.seed(1234)
for(i in 1:n) {
  ind <- folds[[i]]
  train <- data[-ind, ]
  valid <- data[ind, ]
  
  mod <- stats::step(glm(output~., data=train, family=binomial), 
                     direction = 'both', trace=0)
  pred <- predict(mod, newdata=valid)
  auc1 <- roc(valid$output, pred, auc=T)
  auc_result <- c(auc_result, auc1$auc)
}

auc_result
glm_auc <- mean(auc_result)
glm_auc # 0.8917125


# randomForest
library(randomForest)

auc_result <- c()

set.seed(1234)
for(i in 1:n) {
  ind <- folds[[i]]
  train <- data[-ind, ]
  valid <- data[ind, ]
  
  mod <- randomForest(output~., data=train, ntree=500)
  pred <- predict(mod, newdata=valid, type='prob')
  auc1 <- roc(valid$output, pred[, 2], auc=T)
  auc_result <- c(auc_result, auc1$auc)
}

random_auc <- mean(auc_result)
random_auc # 0.8973776


# Tree
library(rpart)

auc_result <- c()

set.seed(1234)
for(i in 1:n) {
  ind <- folds[[i]]
  train <- data[-ind, ]
  valid <- data[ind, ]
  
  mod <- rpart(output~., data=train)
  pred <- predict(mod, newdata=valid, type='prob')
  auc1 <- roc(valid$output, pred[, 2], auc=T)
  auc_result <- c(auc_result, auc1$auc)
}

tree_auc <- mean(auc_result)
tree_auc # 0.7585644


# svm
library(e1071)

auc_result <- c()

set.seed(1234)
for(i in 1:n) {
  ind <- folds[[i]]
  train <- data[-ind, ]
  valid <- data[ind, ]
  
  mod <- svm(output~., data=train, probability=T)
  pred <- predict(mod, newdata=valid, probability=T)
  auc1 <- roc(valid$output, attr(pred, 'probabilities')[, 2], auc=T)
  auc_result <- c(auc_result, auc1$auc)
}

svm_auc <- mean(auc_result)
svm_auc # 0.9035007

result <- data.frame(model = c('glm_auc', 'random_auc', 'tree_auc', 'svm_auc'),
                     auc = c(glm_auc, random_auc, tree_auc, svm_auc))

result %>% 
  ggplot(aes(x=model, y=auc)) +
  geom_col(aes(fill=model)) +
  geom_text(aes(label=auc))


# => 로지스틱회귀가 가장 좋은 성능 냄
set.seed(1234)
glm.model <- glm(output~., family = 'binomial', data = train) 

summary(glm.model)

# 변수선택법(전진(forward), 후진(backward), 단계적(both))
glm.model <- stats::step(glm.model, direction = 'both') 
summary(glm.model)

glm.pred <- predict(glm.model, newdata = valid)

rocobj <- roc(valid$output, glm.pred, auc=TRUE)
rocobj$auc 
cutoff <- coords(rocobj, x="best", input="threshold", best.method="youden")$threshold


#====================================================
## 7. output : test 데이터
test_raw <- read.csv('test.csv')

test <- test_raw %>% 
  mutate_at(vars(cp, restecg, slp, caa, thall), list(as.factor)) 

test %>% distinct(X) %>% nrow() # X는 단순변수

test <- bake(data_recipe, test)
glimpse(test)

result.pred <- predict(glm.model, newdata = test) 

result <- test_raw %>% 
  select(X) %>% 
  cbind(output = if_else(result.pred > cutoff, 1, 0))

write.csv(result, './result_um.csv') # 결과 제출



