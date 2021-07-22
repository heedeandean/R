# TO DO! 목적변수(status) 예측

# 변수설명
# sl_no = Serial Number
# gender = Male='M', Female='F'
# ssc_p = 중등 교육 비율 - 10학년
# ssc_b = 교육위원회 - Central/ Others
# hsc_p = 고등 교육 비율 - 12학년
# hsc_b = 교육위원회 - Central/ Others
# hsc_s = 고등 교육의 전문화
# degree_p = 학위 백분율
# degree_t = 졸업예정자(학위유형) - 학위 교육 분야
# workex = 직장 경험
# etest_p = 취업 테스트 비율(대학에서 실시)
# specialisation = 졸업 후(MBA) - 전문화
# mba_p = MBA percentage
# status = Placed/Not placed
# salary = 급여


rm(list=ls())

library(tidyverse)

setwd('C:/Users/82102/OneDrive/바탕 화면/git/R/analysis/data/campus/')

# =====================================
## 1. load data
data <- read.csv('train.csv', stringsAsFactors = F, strip.white = T)
glimpse(data) # 149 X 16
head(data)
# View(data)

# =====================================
## 2. NA 
colSums(is.na(data)) # salary 46 

data %>% 
  filter(is.na(salary)) %>%
  count(status)

data %>% 
  filter(!is.na(salary)) %>%
  count(status)

# => status::Not Placed => salary NA

data <- data %>% mutate(salary = if_else(is.na(salary), 0, as.numeric(salary)))

colSums(is.na(data)) 
# =====================================
## 3. EDA
foo <- data %>%  
  mutate_if(is.character, list(as.factor)) %>% 
  mutate_if(is.factor, list(as.numeric)) 

glimpse(foo)

# 1) 상관분석
library(corrplot)

# 0.7 이상부터 높은 관계 
corrplot(cor(foo), order = 'hclust', method = 'number') 

bar <- cor(foo) %>% as.data.frame()

# Q. 취직에 가장 영향을 미치는 factor는? (salary 제외)
# A. ssc_p 
bar %>% 
  select(status) %>% 
  arrange(desc(status)) %>% 
  head()

# Q. Percentage가 취직에 영향을 미치는가?
bar %>% 
  select(status)   

# Q. 기업이 가장 많이 요구하는 specialisation 전공은?
# A. Mkt&Fin
data %>% 
  ggplot(aes(x = specialisation)) +
  geom_bar(aes(fill=status))

# Q. 모든 통계 테스트를 진행할것?



# 2) 전체 설명변수
foo %>% 
  rowid_to_column() %>% 
  pivot_longer(-c(rowid, status)) %>% 
  ggplot(aes(x=value, fill=name)) +
  geom_histogram() +
  facet_wrap(~name, scale = 'free') +
  theme(legend.position = 'none')
# => factor형으로 의심되는 변수 
# (degree_t, gender, hsc_b, hsc_s, specialisation, ssc_b, workex)


# 3) 목적변수와 설명변수와의 관계
foo <- foo %>% mutate(status = as.factor(status)) # 목적변수만 factor형 변환

foo %>% 
  rowid_to_column() %>%
  pivot_longer(-c(rowid, status)) %>% 
  ggplot(aes(x=status, y=value)) +
  geom_boxplot() +
  facet_wrap(~name, scale = 'free_y')



# =====================================
data_backup <- data
# data <- data_backup

## 4. 전처리
data %>% distinct(X) %>% nrow() 
data %>% distinct(sl_no) %>% nrow()
# => X, sl_no 는 단순변수

data <- data %>% select(-X, -sl_no)

glimpse(data)

# 위 그래프에서 facor형으로 의심된 변수들 
data %>% 
  select(degree_t, gender, hsc_b, hsc_s, specialisation, ssc_b, workex) %>% 
  glimpse()
# 모두 character형
# => 로지스틱, 랜덤포레스트는 자동으로 더미로 바꿔준다. 
# 궁금한점. 다른 모델들도 자동으로 더미로 바꿔줄까?

# =====================================
glimpse(data)

## 5. train(7)/valid(3) 분할
library(caret)

set.seed(1234)
train_ind <- createDataPartition(data$status, p = 0.7)$Resample1 

train <- data[train_ind, ]
valid <- data[-train_ind, ]

table(data$status) %>% prop.table() %>% round(2)
table(train$status) %>% prop.table() %>% round(2)
table(valid$status) %>% prop.table() %>% round(2)

# =====================================
## 6. 모델링

####### 랜덤포레스트
library(randomForest)

set.seed(1234)
rf <- randomForest(factor(status)~., data = train, ntree=200, importance=T)

pred1 <- predict(rf, newdata = valid)

# RMSE(Root Mean Squared Error, 평균 제곱근 오차) : 작을수록 좋다.
RMSE(as.numeric(pred1), as.numeric(as.factor(valid$status))) # 0 

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
rf_fit <- train(status~., data = train, method = "rf",
                trControl = fitControl, verbose = F)
rf_fit # mtree : 8 

rf.pred <- predict(rf_fit, newdata = valid) 

RMSE(as.numeric(rf.pred), as.numeric(as.factor(valid$status))) # 0 


# 홀드아웃 : 데이터가 작을때 train/valid/test -> train/test 만 나눈다.
set.seed(1234)
rf_fit2 <- train(status~., data = data, method = "rf",
                 trControl = fitControl, verbose = F)
rf_fit2 # mtree : 8

# => 최종모델은 rf.fit2
# =====================================
## 7. ouput
test_raw <- read.csv('test.csv', stringsAsFactors = F, strip.white = T)
glimpse(test_raw) # 66 X 15

colSums(is.na(test_raw)) 

test <- test_raw %>% 
  mutate(salary = if_else(is.na(salary), 0, as.numeric(salary)))

colSums(is.na(test)) 

pred <- predict(rf_fit2, newdata = test)

write.csv(pred, 'result_um.csv') # 결과 제출
# =====================================
# 정답 추측..
correct <- test_raw %>% 
  mutate(status = if_else(is.na(salary), 'Not Placed', 'Placed')) %>% 
  select(X, status)

# 채점
RMSE(as.numeric(pred), as.numeric(as.factor(correct$status))) # 0 

# ===========================================
# (salary 제외)
data <- read.csv('train.csv', stringsAsFactors = F, strip.white = T)

data <- data %>% select(-X, -sl_no, -salary)
data %>% head

data <- data %>% 
  mutate(status = as.numeric(as.factor(status)))


## train(7)/valid(3) 분할
library(caret)

set.seed(1234)
train_ind <- createDataPartition(data$status, p = 0.7)$Resample1 

train <- data[train_ind, ]
valid <- data[-train_ind, ]

table(data$status) %>% prop.table() %>% round(2)
table(train$status) %>% prop.table() %>% round(2)
table(valid$status) %>% prop.table() %>% round(2)

####### 랜덤포레스트
library(randomForest)

set.seed(1234)
rf <- randomForest(factor(status)~., data = train, ntree=200, importance=T)

pred1 <- predict(rf, newdata = valid)

# RMSE(Root Mean Squared Error, 평균 제곱근 오차) : 작을수록 좋다.
RMSE(as.numeric(pred1), valid$status) # 0.3692745


## 앙상블(Stacking)을 하면 RMSE가 더 좋아질까? 
# Stacking 
library(caretEnsemble)

# library(doParallel)
# detectCores() %>% registerDoParallel()

control <- trainControl(method="boot", number=10, savePredictions=TRUE) 
algorithmList <- c('rpart', 'glm', 'svmLinear','rf') #(의사 결정, 일반화 선형, 서포트 백터 머신 , 랜덤 포레스트)
set.seed(1234)
models <- caretList(status~., data=train, trControl=control, 
                    methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)

# 부스팅 모델(메인 모델)과 스택을 통해 모델을 결합 
stackControl <- trainControl(method="boot", number=10, savePredictions=TRUE) 
stack.gbm <- caretStack(models, method="gbm", metric="RMSE", 
                        trControl=stackControl)
print(stack.gbm)
stack.pred <- predict(stack.gbm, newdata = valid) 

RMSE(as.numeric(stack.pred), valid$status) # 0.2608862
# R2(as.numeric(stack.pred), valid$status)
