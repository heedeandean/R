# TO DO! 목적변수(G3) 예측

setwd('C:/Users/82102/OneDrive/바탕 화면/git/R/analysis/data/student/')

library(tidymodels)

# 변수 설명
# school - 학교명('GP' - Gabriel Pereira or 'MS' - Mousinho da Silveira)
# sex - ('F' - female or 'M' - male)
# age - (numeric: from 15 to 22)
# address - ('U' - urban(도시) or 'R' - rural(시골))
# famsize - 가족 수 ('LE3' - 3이하 or 'GT3' - 3초과)
# Pstatus - 부모의 동거 상태 ('T' - living together or 'A' - apart)
# Medu - mother's education (numeric: 0 - none, 1 - 초등교육(4th grade), 2 - 5th to 9th grade, 3 - 중등교육, 4 - 고등교육)
# Fedu - father's education 
# Mjob - mother's job ('teacher', 'health', 'services'(ex. 행정 or 경찰), 'at_home', 'other')
# Fjob - father's job
# reason - 이 학교를 선택한 이유 (close to 'home', 'reputation'(평판), 'course'(코스 선호도), 'other')
# guardian - 학생의 보호자 ('mother', 'father', 'other')
# traveltime - 집에서 학교까지 시간 (numeric: 1 - 1 hour)
# studytime - 주간 공부 시간 (numeric: 1 - 10 hours)
# failures - 과거 수업 실패 횟수 (numeric: n if 1<=n<3, else 4)
# schoolsup - 추가 교육 지원 (yes or no)
# famsup -  가족 교육 지원 (yes or no)
# paid - 추가 유료 수업 (수학 or 포르투갈어) (yes or no)
# activities - 교과 외 활동 (yes or no)
# nursery - 보육원에 다녔다 (yes or no)
# higher - 고등 교육을 원하는지 (yes or no)
# internet - 집에서 인터넷 접속 (yes or no)
# romantic - 로맨틱한 관계 여부 (yes or no)
# famrel - 가족간의 관계 (numeric: from 1 - very bad to 5 - excellent)
# freetime - 방과 후 자유시간 (numeric: from 1 - very low to 5 - very high)
# goout - 친구들과 외출 (numeric: from 1 - very low to 5 - very high)
# Dalc - 평일 알코올 소비 (numeric: from 1 - very low to 5 - very high)
# Walc - 주말 알코올 소비 (numeric: from 1 - very low to 5 - very high)
# health - 현재 건강 상태 (numeric: from 1 - very bad to 5 - very good)
# absences - 결석 횟수 (numeric: from 0 to 93)
# G3 - 최종 성적 (numeric: from 0 to 20) -> 목적변수

## 1. load data
data <- read.csv('train.csv', strip.white = T)

glimpse(data) # 294 X 31
head(data)
data %>% count(G3)


## 2. NA 
colSums(is.na(data)) 


## 3. EDA

# 상관관계
library(corrplot)

foo <- data %>% select_if(is.numeric) %>% cor()
corrplot(foo, order = 'hclust') 


# 목적변수(G3)의 분포
hist(data$G3, 
     main="Histogram for Final Grade G3", 
     xlab="Students' final grade", 
     border="blue", 
     col="green",
     las=1, 
     breaks=15)


# G3 vs age
data_age <- data %>% mutate(age = as.factor(age)) 
data_age %>% 
  ggplot(aes(x=age, y=G3, fill=age)) +
  geom_boxplot() 
# => 17,18세의 중앙값이 비슷해보인다. / 19세에 외도가 있어보인다. / 20살이 가장 높은 성적을 받은 것으로 보인다.


# G3 vs 부모의 직업
data %>% 
  ggplot(aes(x = Fjob, y = G3)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F)

data %>% 
  ggplot(aes(x = Mjob, y = G3)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F)
# => 부모의 직업에 따른 학생의 성취도에서 뚜렷한 차이를 볼 수 없습니다.


## 4. train(7)/valid(3) 분할
library(caret)

set.seed(1234)
train_ind <- createDataPartition(data$G3, p = 0.7)$Resample1 # initial_split 동일

train <- data[train_ind, ]
valid <- data[-train_ind, ]


## 5. 모델링

# ===============================
# 랜덤포레스트
library(randomForest)

set.seed(1234)
rf <- randomForest(factor(G3)~., data = train, ntree=200, importance=T)

rf.pred <- predict(rf, newdata = valid)
pred1 <- rf.pred %>% as.matrix() %>% as.numeric()


# ===============================
## ridge 회귀
library(glmnet)
x_train <- model.matrix(train$G3~., train)[,-1] 
x_valid <- model.matrix(valid$G3~., valid)[,-1]

y_train <- as.matrix(train$G3)

grid = 10^seq(10, -2, length = 100) # lambda값 지정

# alpha = 0 (ridge), alpha = 1 (lasso), alpha = 0.5 (elasticnet)
ridge_mod <- glmnet(x_train, y_train, alpha = 0, lambda = grid)

set.seed(1234)
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0) # cross validation 사용.
plot(cv_ridge) 

bestlam_ridge <- cv_ridge$lambda.min # 가장 낮은 점이 최적의 람다이다
bestlam_ridge

ridge_pred <- predict(ridge_mod, s = bestlam_ridge, newx = x_valid) 


# ===============================
## SOM

library(tidymodels)

# chr -> 더미
data_recipe <- recipe(G3~., data = data) %>% 
  step_dummy(all_predictors()) %>% 
  step_YeoJohnson(all_predictors()) %>% 
  prep()

data_baked <- bake(data_recipe, data)
glimpse(data)

library(kohonen)
x <- data_baked %>% select(-G3) %>% as.matrix() # -목적변수, matrix형
y <- data_baked %>% select(G3) %>% as.matrix() %>% as.factor() # 목적변수, factor형

# train(7), test(3) 분할
set.seed(1234)

training <- createDataPartition(data_baked$G3, p = 0.7)$Resample1
Xtraining <- scale(x[training, ])
Xtest <- scale(x[-training, ],
               center = attr(Xtraining, "scaled:center"),
               scale = attr(Xtraining, "scaled:scale"))

trainingdata <- list(x = Xtraining, y = y[training])
testdata <- list(x = Xtest, y = y[-training])

mygrid = somgrid(5, 5, "hexagonal")

# set.seed(1234)
# som.stu <- supersom(trainingdata, grid = mygrid)
# som.prediction <- predict(som.stu, newdata = testdata[1]) # 목적변수 제외하고 예측


# 
set.seed(1234)
som.stu <- supersom(Xtraining, grid = mygrid)
som.prediction <- predict(som.stu, newdata = testdata,
                          trainingdata = trainingdata)


# ===============================
## 앙상블(스태킹) 
library(caretEnsemble)

# library(doParallel)
# detectCores() %>% registerDoParallel()

control <- trainControl(method="boot", number=10, savePredictions=TRUE) 
algorithmList <- c('rpart', 'glm', 'svmLinear','rf') #(의사 결정, 일반화 선형, 서포트 백터 머신 , 랜덤 포레스트)
set.seed(1234)
models <- caretList(G3~., data=train, trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)

# 부스팅 모델(메인 모델)과 스택을 통해 모델을 결합 
stackControl <- trainControl(method="boot", number=10, savePredictions=TRUE) 
stack.gbm <- caretStack(models, method="gbm", metric="RMSE", trControl=stackControl)
stack.gbm
stack.pred <- predict(stack.gbm, newdata = valid) 


# ===============================
# RMSE(Root Mean Squared Error, 평균 제곱근 오차) : 작을수록 좋다.

# 랜덤포레스트 
RMSE(pred1, valid$G3) # 5.078454

# 릿지회귀 
RMSE(ridge_pred, valid$G3) # 4.374721 
pred2 <- round(ridge_pred[, 1])
RMSE(pred2, valid$G3) # 4.431651

# SOM  
RMSE(as.numeric(som.prediction$predictions$y), as.numeric(y[-training])) # 4.871607

# 앙상블(스태킹)
RMSE(stack.pred, valid$G3) # 4.104874
pred3 <- round(stack.pred)
RMSE(pred3, valid$G3) # 4.141396

# => 앙상블이 가장 좋은 성능을 내었다.

# ============================================
# 홀드아웃

# x_data <- model.matrix(data$G3~., data)[,-1] 
# y_data <- as.matrix(data$G3)
# 
# # alpha = 0 (ridge), alpha = 1 (lasso), alpha = 0.5 (elasticnet)
# ridge_mod <- glmnet(x_data, y_data, alpha = 0, lambda = grid)
# 
# set.seed(1234)
# cv_ridge <- cv.glmnet(x_data, y_data, alpha = 0) # cross validation 사용.
# 
# bestlam_ridge <- cv_ridge$lambda.min # 가장 낮은 점이 최적의 람다이다
# bestlam_ridge
# 
# # 최종모델은 ridge_mod

set.seed(1234)
result.model <- caretList(G3~., data=data, trControl=control, methodList=algorithmList)

# 부스팅 모델(메인 모델)과 스택을 통해 모델을 결합 
stack.gbm <- caretStack(models, method="gbm", metric="RMSE", trControl=stackControl)

# 최종모델은 stack.gbm 


# ============================================
## output
test <- read.csv('test.csv', strip.white = T)

colSums(is.na(test)) 

stack.pred <- predict(stack.gbm, newdata = test) 
result <- round(stack.pred)

write.csv(result, 'result_um.csv')
