rm(list=ls()) # 메모리 삭제
setwd('C:/Users/82102/OneDrive/바탕 화면/git/R/analysis/data/')

library(tidyverse)

## 100개의 NA가 포함된 데이터. (목적변수 : X1.1)
data <- read.csv('cacer.csv', header = F) 
head(data)
glimpse(data)
nrow(data)

# 1. 데이터 전처리

# NA값 확인. 
colSums(is.na(data))
data %>% map_int(~sum(is.na(.x)))

colnames(data) <- c('id', 'p', 'result')

# NA값 채우기 
# 연속형 데이터(숫자) : 평균
p_mean <- mean(data$p, na.rm = T)
data[is.na(data$p), 'p'] <- p_mean

result_na <- data[is.na(data$result), ] %>% select(id)
result_na

data2 <- anti_join(data, result_na, by='id')
nrow(data2)

data3 <- inner_join(data, result_na, by='id')
nrow(data3)


# 2. train과 test를 80개 20개로 나누기.
library(caret)

train_ind <- createDataPartition(data2$result, p = 0.83)$Resample1 
train <- data2[train_ind, ]
test <- data2[-train_ind, ]

# data2:96, train:80, test:15
table(data2$result) %>% prop.table() %>% round(2)
table(train$result) %>% prop.table() %>% round(2)
table(test$result) %>% prop.table() %>% round(2)

nrow(train)

# 목적변수가 NA값을 test데이터로
test <- rbind(test, data3)
nrow(test)


# 3. 데이터 분포 시각화.(EDA)

# result가 -로 내려갔기 때문에 선형회귀 사용 X
train %>% 
  ggplot(aes(x=p, y=result)) +
  geom_point() +
  geom_smooth(method = 'lm')

train %>% 
  pivot_longer(-c(id)) %>% 
  ggplot(aes(x=value, fill=name)) +
  geom_histogram() +
  facet_grid(~name)

train %>% 
  ggplot(aes(x=p, fill=result)) +
  geom_histogram() +
  facet_grid(~result)

# 4. 모델링과 rmse.
model <- glm(result~., family = 'binomial', data = train)
summary(model)

pred <- predict(model, test, type = 'response')
pred

rmse2 <- function(y, y_pred) {
  sqrt(mean((y-y_pred)^2, na.rm=T))
}
rmse2(test$result, pred)


# 5. ROC curve
# test %>% map_int(~sum(is.na(.x)))
# test.na <- test[is.na(test$result), 'id']
# test.na <- test.na %>% as.data.frame() %>% rename(id='.')
# test.na
# 
# test <- anti_join(test, test.na, by='id')
# nrow(test)
# 
# pred <- predict(model, test, type='response')
# 
# df <- data.frame()
# for(i in pred) {
#   cm <- confusionMatrix(factor(ifelse(pred > i, 1, 0)), factor(test$result))
#   df <- rbind(df, data.frame(sensitivity=cm$byClass[1], specificity=cm$byClass[2]))
# }
# df
# 
# df %>% 
#   ggplot(aes(x=1-specificity, y=sensitivity)) +
#   geom_line()
# 
# library(pROC)
# rocobj <- roc(test$result, test$p)
# coords(rocobj, "best")
# coords(rocobj, x="best", input="threshold", best.method="youden")