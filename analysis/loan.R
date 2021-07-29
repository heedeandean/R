setwd('C:/Users/82102/OneDrive/바탕 화면/git/R/analysis/data/loan/')

library(tidymodels)
library(caret)

# ========================================
# 문제 생성
# data_raw <- read.csv('Loan payments data.csv')  
# 
# table(data_raw$loan_status)
# data_raw %>% view()
# 
# # train(8)/test(2) 분할
# set.seed(1234)
# train_ind <- createDataPartition(data_raw$loan_status, p = 0.8)$Resample1 
# 
# train <- data_raw[train_ind, ]
# test_correct <- data_raw[-train_ind, ]
# 
# test <- test_correct %>% select(-loan_status)
# 
# dim(train)
# dim(test_correct)
# dim(test)
# write.csv(train, 'train.csv')
# write.csv(test, 'test.csv')
# write.csv(test_correct, 'correct.csv')

# ========================================
# TO DO! 목적변수(loan_status) 예측

# 변수 설명
# Loan_ID	: 대출한 고객의 고유한 ID

# loan_status	: 목적변수 (PAIDOFF: 기한 내 대출금 완납 / COLLECTION: 미납 / COLLECTION_PAIDOFF: 기한 초과 대출금 완납) 

# Principal	: 대출 금액
# terms	: 대출금 지급까지 걸린 기간
# effective_date	: 실제 계약 효과가 발휘하기 시작한 날짜
# due_date	: 대출금 납부 기한 날짜
# paid_off_time	: 대출 상환 날짜, 시간
# past_due_days	: 대출 연체 일수
# age	: 고객의 나이

# education	: 고객의 교육 수준
# (High School or Below : 고졸
#  college : 대졸
#  Bechalor : 석사
#  Master or Above : 박사 이상)

# Gender : 고객의 성별
# ===============================
## 1. load data
data <- read.csv('train.csv', na.string = c('', ' ', NA))

glimpse(data) # 400 X 12
head(data)
# data %>% view()

data %>% filter(duplicated(.)) # 중복 row 확인

# ===============================
## 2. 전처리

# 단순 변수(X, Loan_ID) 제거
data %>% distinct(X) %>% nrow() 
data %>% filter(grepl('xqd', Loan_ID)) %>% nrow()

data <- data %>% select(-X, -Loan_ID) 

## NA 
colSums(is.na(data)) 

# 2-1. past_due_days(연체일)이 NA인 경우
data %>% 
  filter(is.na(past_due_days)) %>% 
  distinct(loan_status)
# => PAIDOFF (기한 내 대출금 완납) 

data %>% 
  filter(!is.na(past_due_days)) %>% 
  distinct(loan_status)

# 2-2. paid_off_time(대출 상환 날짜, 시간) NA인 경우
data %>% 
  filter(is.na(paid_off_time)) %>% 
  distinct(loan_status)
# => COLLECTION (미납)

data %>% 
  filter(!is.na(paid_off_time)) %>% 
  distinct(loan_status)


# NA -> 0
data[is.na(data)] <- 0
colSums(is.na(data)) 
  
glimpse(data)

# date형을 어떻게 다룰까? - effective_date, due_date, paid_off_time
# data %>% mutate_at(vars(effective_date, due_date), 
#                    list(~as.Date(., format='%m/%d/%Y')))
# 
# data %>% mutate(paid_off_time = as.Date(paid_off_time, format='%m/%d/%Y %H:%M')) 


## 3. EDA

# ★★★★★
# 1. numeric, numeric : geom_point, geom_smooth(method = 'lm') 
# 2. factor, factor : geom_point, geom_count
# 3. x=factor or chr, y=numeric : geom_boxplot

# 설명변수(numeric) vs 목적변수(factor) 
data %>% 
  select_if(is.numeric) %>%
  mutate(loan_status = data$loan_status) %>% 
  rowid_to_column() %>% 
  pivot_longer(-c(rowid, loan_status)) %>% 
  ggplot(aes(x=value, y=loan_status)) +
  geom_boxplot() +
  facet_wrap(~name, scale='free_x') 


# 설명변수(facor) vs 목적변수(factor)
data %>% select_if(is.character) %>% colnames()

data %>% 
  select(Gender, loan_status) %>% 
  rowid_to_column() %>% 
  pivot_longer(-c(rowid, loan_status)) %>% 
  ggplot(aes(x=loan_status, y=value)) +
  geom_point() +
  geom_count(mapping = aes(x=loan_status, y=value, shape=name)) 


data %>% 
  select(education, loan_status) %>% 
  rowid_to_column() %>% 
  pivot_longer(-c(rowid, loan_status)) %>% 
  ggplot(aes(x=loan_status, y=value)) +
  geom_point() +
  geom_count(mapping = aes(x=loan_status, y=value, shape=name)) 

# 목적변수의 비율이 너무 치중되어 있다..
table(data$loan_status)

# ============================================
## 4. train(7)/valid(3) 분할

set.seed(1234)
train_ind <- createDataPartition(data$loan_status, p = 0.7)$Resample1 # initial_split 동일

train <- data[train_ind, ]
valid <- data[-train_ind, ]

# ============================================
## 5. 모델링

# 랜덤포레스트
library(randomForest)

set.seed(1234)
rf <- randomForest(factor(loan_status)~., data = train, ntree=200, importance=T)

rf.pred <- predict(rf, newdata = valid)

rf.cm <- confusionMatrix(rf.pred, as.factor(valid$loan_status))
rf.cm$byClass[7] # F1 : 1 


## 
# foo <- data %>% select(-past_due_days, -paid_off_time)
# 
# set.seed(1234)
# train_ind <- createDataPartition(foo$loan_status, p = 0.7)$Resample1 # initial_split 동일
# 
# train_foo <- foo[train_ind, ]
# valid_foo <- foo[-train_ind, ]
# 
# set.seed(1234)
# foo.rf <- randomForest(factor(loan_status)~., data = train_foo, ntree=200, importance=T)
# 
# foo.pred <- predict(foo.rf, newdata = valid_foo)
# 
# foo.cm <- confusionMatrix(foo.pred, as.factor(valid_foo$loan_status))
# foo.cm$byClass[7] # F1 : 0.25 






# ============================================
# ============================================
# ============================================
# ============================================
# ============================================
## output
test <- read.csv('test.csv', na.string = c('', ' ', NA))

# 단순 변수(X, Loan_ID) 제거
test %>% distinct(X) %>% nrow() 
test %>% filter(grepl('xqd', Loan_ID)) %>% nrow()

test <- test %>% select(-X, -Loan_ID) 

## NA 
colSums(is.na(test)) 

test[is.na(test)] <- 0
colSums(is.na(test)) 

result <- predict(rf, newdata = test)

write.csv(result, 'result_um.csv')



# ============================================
# ============================================
# ============================================
# ============================================
# ============================================
# ============================================
# 채점
# correct <- read.csv('correct.csv', na.string = c('', ' ', NA))
# 
# result.cm <- confusionMatrix(result, as.factor(correct$loan_status))
# result.cm$byClass[7] # F1 : 1 




stack.pred <- predict(stack.gbm, newdata = test) 
result <- round(stack.pred)

write.csv(result, 'result_um.csv')


