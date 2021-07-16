rm(list=ls())

library(tidymodels)
library(caret)

setwd('C:/Users/82102/OneDrive/바탕 화면/git/R/analysis/data')

## 1. load data

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

train <- read.csv('heart_train.csv', stringsAsFactors = F, 
                  fileEncoding = 'euc-kr', strip.white = T)
glimpse(train) # 243 X 15
head(train)

train <- train %>% select(-X)


## 2. NA 
colSums(is.na(train)) 


## 3. EDA
# 변수 전체 
train %>% 
  rowid_to_column() %>% 
  pivot_longer(-c(rowid)) %>% 
  ggplot(aes(x=value, fill=name)) +
  geom_histogram() +
  facet_wrap(~name, scale = 'free') +
  theme(legend.position = 'none')
# ==============================
# 상관분석
library(corrplot)

corrplot(cor(train), order = 'hclust')
# ==============================
## 4. train(6)/valid(4) 분할
set.seed(1234)
train_ind <- createDataPartition(train$output, p = 0.6)$Resample1 

data.train <- train[train_ind, ]
data.valid <- train[-train_ind, ]

table(train$output) %>% prop.table() %>% round(2)
table(data.train$output) %>% prop.table() %>% round(2)
table(data.valid$output) %>% prop.table() %>% round(2)


## 5. 모델링

# model1. 선형 회귀
lm.model <- lm(output~., data = data.train)
summary(lm.model)

model1 <- stats::step(lm.model, direction = 'both') 
summary(model1)


# model2. 로지스틱 회귀
logit.model <- glm(output~., family = 'binomial', data = data.train) 

# 유의미한 변수 *
summary(logit.model)

# 유의미한 변수만 정리
# : 선택법(전진(forward), 후진(backward), 단계적(both))
model2 <- stats::step(logit.model, direction = 'both') 
summary(model2)


# model3. 랜덤포레스트
library(randomForest)

set.seed(1234)
rf <- randomForest(output~., data = data.train, ntree=200, importance=T)

#====================================================
## 6. validation
pred1 <- predict(model1, newdata = data.valid, type = 'response')
pred1

pred2 <- predict(model2, newdata = data.valid, type = 'response')
pred2

pred3 <- predict(rf, newdata = data.valid)
pred3

# 최적의 cutoff 찾기
library(pROC)

rocobj <- roc(data.valid$output, pred1)
aa <- coords(rocobj, x="best")
cutoff1 <- aa$threshold
cutoff1 

rocobj <- roc(data.valid$output, pred2)
aa <- coords(rocobj, x="best")
cutoff2 <- aa$threshold
cutoff2 

rocobj <- roc(data.valid$output, pred3)
aa <- coords(rocobj, x="best")
cutoff3 <- aa$threshold
cutoff3 


# install.packages('e1071')
# 혼동행렬(예측값, 정답)
cm1 <- confusionMatrix(factor(if_else(pred1 > cutoff1, 1, 0)), factor(data.valid$output))
cm1 # No Information Rate : 찍었을 때

cm2 <- confusionMatrix(factor(if_else(pred2 > cutoff2, 1, 0)), factor(data.valid$output)) 
cm2 

cm3 <- confusionMatrix(factor(if_else(pred3 > cutoff3, 1, 0)), factor(data.valid$output))
cm3  

#====================================================
# 가장 좋은 모델은?

# RMSE(Root Mean Squared Error, 평균 제곱근 오차) : 작을수록 좋다.
RMSE(pred1, data.valid$output) 
RMSE(pred2, data.valid$output)
RMSE(pred3, data.valid$output)

# F1 높을수록 좋다. 
cm1$byClass[7] 
cm2$byClass[7]
cm3$byClass[7]

# AUC
plot.roc(data.valid$output, pred1, print.auc=TRUE, 
         ci=FALSE, col="black", lty=2, print.thres=TRUE)

plot.roc(data.valid$output, pred2, print.auc=TRUE,
         ci=FALSE, col="black", lty=2, print.thres=TRUE)

plot.roc(data.valid$output, pred3, print.auc=TRUE,
         ci=FALSE, col="black", lty=2, print.thres=TRUE)

# => 랜덤포레스트가 제일 좋은 성능을 낸다. (기준. RMSE)
#====================================================
# 7. output : test 데이터