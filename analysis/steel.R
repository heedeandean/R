# 1개의 목표변수와 26개의 설명변수
# 목표변수는 1,2,3,4,5,6,7 7개의 요인을 가지고 있다.
rm(list=ls())

library(tidymodels)
library(caret)

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
data %>% map_int(~sum(is.na(.x))) # 1안.
colSums(is.na(data)) # 2안.

# ==============================
data <- data %>% 
  mutate_if(is.character, list(as.factor))

glimpse(data)
data %>% count(SteelType) 
data %>% count(Fault)

# ==============================
# 설명변수 factor -> 더미 변환★★★★★ (변수생성 개수 n-1)
steel_recipe <- recipe(Fault~., data = data) %>% 
  step_dummy(all_predictors()) %>% 
  prep()

foo <- bake(steel_recipe, data) # SteelType변수가 더미로 변환
glimpse(foo) 

# ==============================
# 1. EDA(탐색적 데이터 분석)을 하고 상관분석을 실시하고
# 분석에 필요한 파생변수를 선별하시오.
# % 시각화와 통계량을 제시할 것.

# EDA 
# 1) 변수 전체 보기
foo %>% 
  rowid_to_column() %>% 
  pivot_longer(-c(rowid, Fault)) %>% 
  ggplot(aes(x=value, fill=name)) +
  geom_histogram() +
  facet_wrap(~name, scale = 'free') +
  theme(legend.position = 'none')
# => 해석
# Y_Maximum, Y_Minimum 변수의 왜도는 양수로 왼쪽으로 치우쳐 있다.

# 왜도 
# install.packages('fBasics')
library(fBasics)
skewness(foo$Y_Maximum) # 양수(왼쪽으로 치우친 정도)
skewness(foo$Y_Minimum)

# 
rr <- recipe(Fault~., data) %>% 
  step_dummy(all_predictors()) %>% 
  step_YeoJohnson(all_predictors()) %>%  # 왜도 보정
  prep()

bar <- bake(rr, data)
bar %>% 
  rowid_to_column() %>% 
  pivot_longer(-c(rowid, Fault)) %>% 
  ggplot(aes(x=value, fill=name)) +
  geom_histogram() +
  facet_wrap(~name, scale = 'free') +
  theme(legend.position = 'none')

skewness(bar$Y_Maximum) # -0.02
#
nearZeroVar(data, saveMetrics = TRUE)

# 첨도(뾰족한 정도)
kurtosis(data$Empty_Index)

# H0(귀무 가설) : 정규분포이다.
# H1(대립 가설) : 정규분포가 아니다.

# 정규분포 (p-value < 0.05이면, 귀무가설이 기각한다.)
shapiro.test(foo$Luminosity_Index) # 정규분포가 아니다.
shapiro.test(foo$Maximum_of_Luminosity) # 정규분포가 아니다.
# => 육안으로 보고 그래프를 평가할 수 없다.


# ==============================
# 2) 목적 변수(Fault)와의 관계 보기★★★★★
foo %>% 
  rowid_to_column() %>%
  pivot_longer(-c(rowid, Fault)) %>% 
  ggplot(aes(x=Fault, y=value)) +
  geom_boxplot() +
  facet_wrap(~name, scale = 'free_y')
# => 해석
# Log_X_Index변수는 K_Scatch의 빈도를 증가시킨다.
# ==============================
data <- foo

# 상관분석
library(corrplot)

data.cor <- data %>% select(-Fault) %>% cor()

# 파랑(양의 상관관계) / 빨강(음의 상관관계)
corrplot(data.cor, order = 'hclust') 
# => 해석
# 양의 상관관계가 높은 변수는 X_Perimeter, Y_Perimeter 등이 있다. 


# 파생변수 선별 
high <- data.cor %>% findCorrelation(., cutoff = 0.7)
high

# 상관도가 높은 변수들을 그냥 제거하기 아까우니까, 주성분분석 시행
data.high <- data %>% select(all_of(high))
head(data.high)

data.pca <- prcomp(data.high, center = T, scale = T)
plot(data.pca, type='l') # type = 'lines'
summary(data.pca) # Cumulative Proportion 

data2 <- data %>% 
  select(-all_of(high)) %>% # 강한 상관관계변수 제거
  cbind(data.pca$x[, 1:5]) # PC1:PC5 선택

glimpse(data2)

# 상관관계 그래프 다시 그리기
data.cor <- data2 %>% select(-Fault) %>% cor()
corrplot(data.cor, order = 'hclust') 


# ==============================
# 2-1. 데이터를 훈련용(5) 테스트용(3) 평가용(2)으로 나누고 시각화 할 것.
# % 시각화와 통계량을 제시할 것.
data <- data2

train_idx <- createDataPartition(data$Fault, p=0.5)$Resample1
train <- data[train_idx, ]
temp <- data[-train_idx, ]
valid_idx <- createDataPartition(temp$Fault, p=0.6)$Resample1
valid <- temp[valid_idx, ]
test <- temp[-valid_idx, ]
nrow(data) == nrow(train) + nrow(valid) + nrow(test)

table(data$Fault) %>% prop.table() %>% round(2)
table(train$Fault) %>% prop.table() %>% round(2)
table(valid$Fault) %>% prop.table() %>% round(2)
table(test$Fault) %>% prop.table() %>% round(2)

p1 <- data %>% 
  ggplot(aes(x=Fault, fill=Fault)) +
  geom_density(alpha=0.5) 

p1 <- data %>% 
  ggplot(aes(x=Fault, fill=Fault)) +
  geom_density(alpha=0.5) 

p2 <- train %>% 
  ggplot(aes(x=Fault, fill=Fault)) +
  geom_density(alpha=0.5) 

p3 <- valid %>% 
  ggplot(aes(x=Fault, fill=Fault)) +
  geom_density(alpha=0.5) 

p4 <- test %>% 
  ggplot(aes(x=Fault, fill=Fault)) +
  geom_density(alpha=0.5) 

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow=4)


# ==============================
# 2-2. 목표변수가 1이나올지 안나올지에 대하여 알고싶다.
# 목표변수를 이항변수(0, 1)로 바꾸고 로지스틱 회귀분석을 실시하고
# confusionMatrix를 확인하고 최적의 cut off value 정하여라.
# % 시각화와 통계량을 제시할 것.

# Fault의 Pastry가 1이라고 가정. (다른방안 : as.numeric() 사용)
foo <- data %>% mutate(Fault = if_else(Fault == 'Pastry', 1, 0))
# if_else : in dplyr (보통, %>% 안에는 이것을 사용) -> 더 예민
# ifelse : in R
 

train_idx <- createDataPartition(foo$Fault, p=0.5)$Resample1
train <- foo[train_idx, ]
temp <- foo[-train_idx, ]
valid_idx <- createDataPartition(temp$Fault, p=0.6)$Resample1
valid <- temp[valid_idx, ]
test <- temp[-valid_idx, ]
nrow(foo) == nrow(train) + nrow(valid) + nrow(test)

# 로지스틱 회귀
logit.model <- glm(Fault~., data=train, family=binomial)

# 유의미한 변수(*)
summary(logit.model)

# 변수선택법(전진(forward), 후진(backward), 단계적(both))
logit.model <- stats::step(logit.model, direction = 'both')
summary(logit.model)


# 최적의 cut off 찾기(valid 사용)
logit.pred <- predict(logit.model, newdata=valid) 

library(pROC)
rocobj <- roc(valid$Fault, logit.pred)
aa <- coords(rocobj, x="best")
# coords(rocobj, x="best", input="threshold", best.method="youden")
cutoff <- aa$threshold
cutoff # -2.019133 


# test
logit.pred2 <- predict(logit.model, newdata=test)

cm <- confusionMatrix(as.factor(ifelse(logit.pred2 > cutoff, 1, 0)), 
                      as.factor(test$Fault)) 
cm # No Information Rate 큰 이유는 목적변수(Fault)의 데이터 불균형 때문이다. 
# => 해결 : library(Rose)

cm$byClass[7]
# Accuracy < F1, AUC 더 중요. Why? 목적변수가 불균형이면 Accuracy는 의미 X

plot.roc(test$Fault, logit.pred2, print.auc=TRUE, # AUC : 0.928
         ci=FALSE, col="black", lty=2, print.thres=TRUE)


# ==============================
# 2-3. 로지스틱 분석을 제외하고 SVM을 포함하여 3개의 다항분류모형을 선정하고
# confusionMatrix를 확인하고 최적의 cut off value 를 정하여라.
# % 시각화와 통계량을 제시할 것.

## SVM
library(e1071)
svm.model <- svm(Fault~., data = train)
summary(svm.model)

# 최적의 cut off 찾기(valid 사용)
svm.pred <- predict(svm.model, newdata=valid) 

rocobj <- roc(valid$Fault, svm.pred)
aa <- coords(rocobj, x="best")
# coords(rocobj, x="best", input="threshold", best.method="youden")
cutoff <- aa$threshold
cutoff # 0.03427405

# test
svm.pred2 <- predict(svm.model, newdata=test)

cm <- confusionMatrix(as.factor(ifelse(svm.pred2 > cutoff, 1, 0)), 
                      as.factor(test$Fault)) 
cm$byClass[7]

plot.roc(test$Fault, svm.pred2, print.auc=TRUE, # AUC : 0.869
         ci=FALSE, col="black", lty=2, print.thres=TRUE)

# ==============================
library(randomForest)

set.seed(42)
rf.model <- randomForest(Fault~., data = train, ntree=300)
summary(rf.model)

# 최적의 cut off 찾기(valid 사용)
rf.pred <- predict(rf.model, newdata=valid) 

rocobj <- roc(valid$Fault, rf.pred)
aa <- coords(rocobj, x="best")
# coords(rocobj, x="best", input="threshold", best.method="youden")
cutoff <- aa$threshold
cutoff # 0.1946111

# test
rf.pred2 <- predict(rf.model, newdata=test)

cm <- confusionMatrix(as.factor(ifelse(rf.pred2 > cutoff, 1, 0)), 
                      as.factor(test$Fault)) 
cm$byClass[7] # 0.9335303 

plot.roc(test$Fault, rf.pred2, print.auc=TRUE, # AUC : 0.937
         ci=FALSE, col="black", lty=2, print.thres=TRUE)

# ==============================
## ctree 
library(party)
ctree.model <- ctree(Fault~., data=train)
summary(ctree.model)

# 최적의 cut off 찾기(valid 사용)
ctree.pred <- predict(ctree.model, newdata=valid) 

rocobj <- roc(valid$Fault, ctree.pred)
aa <- coords(rocobj, x="best")
# coords(rocobj, x="best", input="threshold", best.method="youden")
cutoff <- aa$threshold
cutoff # 0.1144201

# test
ctree.pred2 <- predict(ctree.model, newdata=test)

cm <- confusionMatrix(as.factor(ifelse(ctree.pred2 > cutoff, 1, 0)), 
                      as.factor(test$Fault)) 
cm$byClass[7]

plot.roc(test$Fault, ctree.pred2, print.auc=TRUE, # AUC : 0.823
         ci=FALSE, col="black", lty=2, print.thres=TRUE)

# ==============================
# 2-4. 위에서 실시한 총 4개의 모형중에 가장 적합한 모형을 활용하여
# 군집분석을 실시하고 F1 스코어값을 구하시오.
# % 시각화와 통계량을 제시할 것.

# 가장 적합한 모형은 AUC가 0.937로 가장 높은 랜덤포레스트.

data.scale <- foo %>% 
  select(-Fault) %>% 
  mutate_all(~scale(.))

# 최적의 군집수 구하기
library(NbClust)
nc <- NbClust(data.scale, min.nc = 2, max.nc = 5, method = 'kmeans') # 2

kmean.model <- kmeans(data.scale, 2, nstart = 25)
kmean.model

data.cluster <- foo %>% 
  mutate(cluster = as.numeric(kmean.model$cluster))
glimpse(data.cluster)

train_idx <- createDataPartition(data.cluster$Fault, p=0.5)$Resample1
train <- data.cluster[train_idx, ]
temp <- data.cluster[-train_idx, ]
valid_idx <- createDataPartition(temp$Fault, p=0.6)$Resample1
valid <- temp[valid_idx, ]
test <- temp[-valid_idx, ]
nrow(data) == nrow(train) + nrow(valid) + nrow(test)

set.seed(42)
rf.model <- randomForest(Fault~., data = train, ntree=300)
summary(rf.model)

# 최적의 cut off 찾기(valid 사용)
rf.pred <- predict(rf.model, newdata=valid) 

rocobj <- roc(valid$Fault, rf.pred)
aa <- coords(rocobj, x="best")
# coords(rocobj, x="best", input="threshold", best.method="youden")
cutoff <- aa$threshold
cutoff # 0.1865556 

# test
rf.pred2 <- predict(rf.model, newdata=test)

cm <- confusionMatrix(as.factor(ifelse(rf.pred2 > cutoff, 1, 0)), 
                      as.factor(test$Fault)) 
cm$byClass[7] # 0.9542857  

plot.roc(test$Fault, rf.pred2, print.auc=TRUE, # AUC : 0.949
         ci=FALSE, col="black", lty=2, print.thres=TRUE)
