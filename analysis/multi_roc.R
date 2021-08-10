setwd('C:/Users/82102/OneDrive/바탕 화면/git/R/analysis/data')
library(tidyverse)

# load data
data <- read.csv('data_raw.csv', strip.white = T, stringsAsFactors = T)
glimpse(data)

library(caret)
# train/test
train_idx <- createDataPartition(data$Fault, p=0.7)$Resample1
train <- data[train_idx, ]
test <- data[-train_idx, ]

library(tidymodels)
data_recipe <- recipe(Fault~., data = train) %>% 
  step_dummy(all_predictors(), -all_outcomes()) %>% 
  prep()

baked_train2 <- bake(data_recipe, train)
baked_test2 <- bake(data_recipe, test)

glimpse(baked_train2)

baked_train2 %>% count(Fault)

baked_train2 %>% 
  mutate(Fault = ifelse(Fault == 'Bumps', 1, 0)) %>% 
  mutate(d_type = 'Bumps') -> tr1
baked_train2 %>% 
  mutate(Fault = ifelse(Fault == 'Dirtiness', 1, 0)) %>% 
  mutate(d_type = 'Dirtiness') -> tr2
baked_train2 %>% 
  mutate(Fault = ifelse(Fault == 'K_Scatch', 1, 0)) %>% 
  mutate(d_type = 'K_Scatch') -> tr3
baked_train2 %>% 
  mutate(Fault = ifelse(Fault == 'Other_Faults', 1, 0)) %>% 
  mutate(d_type = 'Other_Faults') -> tr4
baked_train2 %>% 
  mutate(Fault = ifelse(Fault == 'Pastry', 1, 0)) %>% 
  mutate(d_type = 'Pastry') -> tr5
baked_train2 %>% 
  mutate(Fault = ifelse(Fault == 'Stains', 1, 0)) %>% 
  mutate(d_type = 'Stains') -> tr6
baked_train2 %>% 
  mutate(Fault = ifelse(Fault == 'Z_Scratch', 1, 0)) %>% 
  mutate(d_type = 'Z_Scratch') -> tr7

bind_rows(tr1, tr2, tr3, tr4, tr5, tr6, tr7) -> new_train

glimpse(new_train)

baked_test2 %>% 
  mutate(Fault = ifelse(Fault == 'Bumps', 1, 0)) %>% 
  mutate(d_type = 'Bumps') -> te1
baked_test2 %>% 
  mutate(Fault = ifelse(Fault == 'Dirtiness', 1, 0)) %>% 
  mutate(d_type = 'Dirtiness') -> te2
baked_test2 %>% 
  mutate(Fault = ifelse(Fault == 'K_Scatch', 1, 0)) %>% 
  mutate(d_type = 'K_Scatch') -> te3
baked_test2 %>% 
  mutate(Fault = ifelse(Fault == 'Other_Faults', 1, 0)) %>% 
  mutate(d_type = 'Other_Faults') -> te4
baked_test2 %>% 
  mutate(Fault = ifelse(Fault == 'Pastry', 1, 0)) %>% 
  mutate(d_type = 'Pastry') -> te5
baked_test2 %>% 
  mutate(Fault = ifelse(Fault == 'Stains', 1, 0)) %>% 
  mutate(d_type = 'Stains') -> te6
baked_test2 %>% 
  mutate(Fault = ifelse(Fault == 'Z_Scratch', 1, 0)) %>% 
  mutate(d_type = 'Z_Scratch') -> te7

bind_rows(te1, te2, te3, te4, te5, te6, te7) -> new_test
glimpse(new_test)



new_train %>% 
  split(new_train$d_type)

library(randomForest)

models <- new_train %>% 
  split(new_train$d_type) %>% 
  map(~randomForest(factor(Fault)~., data=.x %>% select(-d_type))) 

new_test <- new_test %>% split(new_test$d_type) %>% 
  map(~.x %>% select(-d_type)) 

roc_curve_result <- map2(.x = models, 
     .y = new_test,
     ~predict(.x, .y, type='prob')) %>% 
  map2(.y = new_test,
       ~bind_cols(.x[, 1], .y)) %>% 
  map(~roc_curve(.x, factor(Fault), `...1`)) %>% 
  set_names(nm = c('Bumps', 'Dirtiness', 'K_Scratch', 'Other_Faults', 'Pastry', 'Stains', 'Z_Scratch')) %>% 
  bind_rows(.id='data_type')

roc_curve_result %>% 
  ggplot(aes(x=1-specificity, y=sensitivity, col=data_type)) +
  geom_line() +
  geom_abline(lty=2)
