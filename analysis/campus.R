# TO DO! 목적변수(status) 예측

# 변수설명
# sl_no = Serial Number
# gender = Male='M', Female='F'
# ssc_p = 중학교 성적
# ssc_b = 교육위원회 - Central/ Others
# hsc_p = 고등학교 성적 
# hsc_b = 교육위원회 - Central/ Others
# hsc_s = 고등 교육의 전문화
# degree_p = 학위 백분율
# degree_t = 학위 전공 
# workex = 직장 경험
# etest_p = 취업 테스트 성적(대학에서 실시)
# specialisation = 졸업 후(MBA) - 전문화
# mba_p = MBA 성적
# status = Placed/Not placed
# salary = 급여


rm(list=ls())

library(tidymodels)

setwd('C:/Users/82102/OneDrive/바탕 화면/git/R/analysis/data/campus/')

# =====================================
## 1. load data
data <- read.csv('train.csv', stringsAsFactors = F, strip.white = T)
glimpse(data) # 149 X 16
head(data)


## 전처리

data %>% filter(duplicated(.)) # 중복 row체크


data %>% distinct(X) %>% nrow() 
identical(data$X, data$sl_no)

data <- data %>% select(-X, -sl_no)
# => X, sl_no 단순변수이므로 제거

# =====================================
## 2. NA 

colSums(is.na(data)) # 1안 
data %>% map_int(~sum(is.na(.x))) # 2안
# => salary 46


# =====================================
# Q. 중요한 변수는? 
# A. ssc_p, mba_p

data %>% count(status)

# 목적변수(status) factor형 변환
foo <- data %>% 
  mutate(status = as.factor(if_else(status == 'Placed', 1, 0))) %>% 
  select(-salary)


# 로지스틱 회귀는 character -> 더미 자동 변환
model <- glm(status~., data=foo, family=binomial)
summary(model)

m_data_total <- tidy(model)

model <- stats::step(model, direction = 'both')
summary(model)

m_data <- tidy(model)

m_data %>% arrange(desc(abs(estimate))) 

m_data %>% 
  ggplot(aes(reorder(x=term, -abs(estimate)), y=estimate, fill=term)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90))


m_data %>% arrange(p.value)

m_data %>% 
  ggplot(aes(reorder(x=term, p.value), y=p.value)) +
  geom_bar(stat='identity', aes(fill=term))

# => [p.value 기준] 
# ssc_p(높을수록), mba_p(낮을수록) 취업이 잘 되는 것으로 나타났다.

# =====================================
# Q. Percentage(성적)이 취업에 영향을 미치는가?

# 로지스틱 회귀

# 1) 성적 변수만 이용
data_p <- foo %>% select(ends_with('_p'), status)
model2 <- glm(status~., data=data_p, family=binomial)
summary(model2)
# => ssc_p, mba_p, hsc_p 영향을 미침


# 2) 전체 변수 이용
m_data_total %>% filter(grepl('_p', term)) %>% arrange(p.value)
# => etest_p는 취업에 영향을 미치지 않았고, 
# 나머지 변수들(ssc_p, mba_p, hsc_p, degree_p)은 취업에 영향을 미쳤다.

# =====================================
# Q. 취업에 영향을 미치는 학위 전공은?

table(data$degree_t)

# 로지스틱 회귀

# 1) 학위전공 변수만 이용
model3 <- glm(status~degree_t, data=foo, family=binomial)
summary(model3)
# => 통계적으로 유의한 변수가 없다. 


# 2) 전체 변수 이용
m_data_total %>% arrange(p.value)
# => degree_tSci&Tech, degree_tOthers 변수가 통계적으로 유의하지만, 
# 계수의 부호가 - 이므로 취업이 되지 않는 것으로 나타났다. 

# ====================================
set.seed(1234)

data_recipe <- recipe(status~., data=foo) %>% 
  step_dummy(all_predictors()) %>% 
  prep()

baked_data <- bake(data_recipe, foo)

data_split <- initial_split(baked_data, prop=3/4, strata=status)
train <- training(data_split)
test <- testing(data_split)

# 랜덤포레스트
library(randomForest)

rf.model <- randomForest(status~., data=train, importance=T)
varImpPlot(rf.model)

pred <- predict(rf.model, newdata=test)
rf.cm <- confusionMatrix(pred, test$status)
rf.cm$byClass[7] # F1 : 0.7


# 홀드아웃
rf.model <- randomForest(status~., data=baked_data, importance=T)
# => 최종 사용 모델 

test <- read.csv('test.csv', stringsAsFactors = F, strip.white = T)
bar <- test %>% select(-X, -sl_no, -salary)
test_baked <- bake(data_recipe, bar)
result <- predict(rf.model, test_baked)

result <- result %>% 
  as.data.frame() %>% 
  mutate(status = if_else(. == 1, 'Placed', 'Not Placed')) %>% 
  select(status)

write.csv(result, 'result_um.csv')

# =====================================
# 교차검증
n <- 5

set.seed(1234)
folds <- createFolds(baked_data$status, k=n, list = T, returnTrain = F)

auc_result <- c()
threshold <- c()

library(pROC)

set.seed(1234)
for (i in 1:n) {
  ind <- folds[[i]]
  train <- baked_data[-ind, ]
  test <- baked_data[ind, ]
  
  rf.model <- randomForest(status~., data=train, ntree=500)
  pred <- predict(rf.model, newdata=test, type='prob')
  rocobj <- roc(as.numeric(test$status), as.numeric(pred[, 1]), auc=T)
  auc_result <- c(auc_result, rocobj$auc)
}
auc1 <- mean(auc_result)
auc1

# =====================================
foo %>% 
  select(ends_with('_p')) %>% 
  rowid_to_column() %>% 
  pivot_longer(-c(rowid)) %>% 
  ggplot(aes(x=value)) +
  geom_density() +
  facet_wrap(~name)
