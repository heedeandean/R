rm(list=ls())
setwd('C:/Users/82102/OneDrive/바탕 화면/git/R/analysis/data/usage/')
library(tidyverse)
library(lubridate)

## load data
# 1) weather.csv
# - dt : 날짜데이터
# - avg_temp: 해당일의 평균온도
weather <- read.csv('weather.csv', stringsAsFactors = F, strip.white = T)

glimpse(weather) # 70 X 2
head(weather)


# 2) usage.csv
# - amount : 15분동안 사용된 사용전력
usage <- read.csv('usage.csv', stringsAsFactors = F, strip.white = T)

# 유닉스 시간 변환
usage$datetime <- as_datetime(usage$timestamp)

head(usage) # timestamp 15분 간격
glimpse(usage) # 6,720 X 2
# View(usage)


# 3) usage_history.tsv
# - wclass : A는 최저기온, B는 최고기온, C는 상기온, D는 저기온
# - A~E : 건물에서 용도별 전력 사용량
# - time : 날짜는 생략된 시간데이터

usage.history <- read.table('usage_history.tsv', sep = '\t', 
                            stringsAsFactors = F, strip.white = T,
                            header = TRUE)


glimpse(usage.history) # 201,600 X 7
head(usage.history)


## NA값 확인
weather <- weather %>% mutate_all(list(~na_if(., '')))
weather %>% map_int(~sum(is.na(.x))) # 1안.
colSums(is.na(weather)) # 2안.

colSums(is.na(usage)) 

usage.history <- usage.history %>% mutate_all(list(~na_if(., '')))
colSums(is.na(usage.history))


# 1. 첫번째 제공 파일의 총 사용량 컬럼을 용도별로 분류하고, 연월과 사용 목적별로 전력의 하루 평균 사용량을 구하여 도표를 도출하시오.
foo <- usage.history %>% 
  mutate_at(vars(LETTERS[1:5]), list(~lag(., 1, 0)))

bar <- usage.history[, 3:7] - foo[, 3:7] 

usage.history.group <- usage.history %>% 
  select(time) %>% 
  cbind(bar) %>% 
  rowwise() %>% 
  mutate(amount = sum(A, B, C, D, E)) %>% 
  cbind(idx = rep(1:(nrow(usage.history)/30), 
                  each = 30, length.out = nrow(usage.history))) %>% 
  group_by(idx) %>% 
  summarise_at(vars(LETTERS[1:5], amount), list(~sum(.))) 

usage.history.group %>% head()
usage %>% head()

usage.history.group$amount <- signif(usage.history.group$amount, 10)
usage$amount <- signif(usage$amount, 10)

usage.join <- inner_join(usage, usage.history.group, by = c('amount'))
usage.join %>% head()

nrow(usage) == nrow(usage.join)

usage.join$date <- format(usage.join$datetime, '%Y-%m-%d')

usage.result <- usage.join %>% 
  group_by(date) %>% 
  summarise_at(vars(LETTERS[1:5], amount), list(~mean(.))) 

usage.result %>% View()


# 2. 요일별 평균전력 사용량을 도출하시오. 
# 또한 가로축을 요일, 세로축을 평균사용량으로 하여 요일별 평균 사용량을 시각화하여 제출하시오.

usage <- usage %>% 
  mutate(weekday = wday(datetime, label = T))

usage %>% 
  group_by(weekday) %>% 
  summarise(amount = mean(amount)) %>% 
  ggplot(aes(x=weekday, y=amount, fill=weekday)) +
  geom_bar(stat = 'identity')


#3. 요일별 총 전력 사용량의 평균값의 차이를 분석하여 가장 큰 차이를 보이는 요일은 어떤 요일인지 제시하시오.
usage.weekday <- usage %>% 
  group_by(weekday) %>% 
  summarise(amount_mean = mean(amount))

usage.weekday.result <- left_join(usage, usage.weekday, by = c('weekday'))

usage.weekday.result %>% 
  mutate(new_var = amount_mean - amount) %>% 
  group_by(weekday) %>% 
  summarise(diff = sum(new_var)) %>% 
  arrange(desc(diff))
  

# 4. 각 날짜별 평균 기온과 용도별 전력 사용량의 관계를 분석하여, 
# 기온과 가장 밀접한 관계를 지닌 사용용도의 종류를 제시하시오.

weather %>% head()
usage.result %>% head()

nrow(weather) == nrow(usage.result) # 70

weather <- weather %>% rename(date = dt)

weather.usage <- inner_join(weather, usage.result, by = c('date')) 
weather.usage %>% head()

library(corrplot)
data.cor <- weather.usage %>% select(-date, -amount) %>% cor()
corrplot(data.cor, order = 'hclust') 
# 기온과 가장 밀접한 관계를 지닌 사용용도는 A로 확인.


