rm(list=ls())
setwd('C:/Users/82102/OneDrive/바탕 화면/git/R/analysis/data/usage/')
library(tidyverse)

## load data

# 1) weather.csv
# - dt : 날짜데이터
# - avg_temp: 해당일의 평균온도
weather <- read.csv('weather.csv', stringsAsFactors = F, strip.white = T)

# weather <- weather %>% as_tibble() # 10행만 보여줌
head(weather)
glimpse(weather) # 70 X 2


# 2) usage.csv
# - amount : 15분동안 사용된 사용전력
usage <- read.csv('usage.csv', stringsAsFactors = F, strip.white = T)

# usage <- usage %>% as_tibble()
head(usage) 
glimpse(usage) # 6,720 X 2

# 유닉스 시간 변환
library(lubridate)

usage <- usage %>% 
  mutate(datetime = as_datetime(timestamp)) %>% # 1안
  # mutate(datetime = as.POSIXct(timestamp, origin='1970-01-01')) %>% # 2안
  select(-timestamp)

usage # 15분 간격


# 3) usage_history.tsv
# - wclass : A는 최저기온, B는 최고기온, C는 상기온, D는 저기온
# - A~E : 건물에서 용도별 전력 사용량
# - time : 날짜는 생략된 시간데이터

usage_history <- read.csv('usage_history.tsv', sep = '\t', 
                          stringsAsFactors = F, strip.white = T)

# usage_history <- usage_history %>% as_tibble()
head(usage_history)
glimpse(usage_history) # 201,600 X 7


## NA값 확인
weather <- weather %>% mutate_all(list(~na_if(., '')))
weather %>% map_int(~sum(is.na(.x))) # 1안.
colSums(is.na(weather)) # 2안.

colSums(is.na(usage)) 

colSums(is.na(usage_history))


# 1. 첫번째 제공 파일의 총 사용량 컬럼을 용도별로 분류하고, 연월과 사용 목적별로 전력의 하루 평균 사용량을 구하여 도표를 도출하시오.
usage_history_lag <- usage_history %>% 
  transmute_at(vars(A:E), list(~lag(., 1, 0))) 

usage_history_diff <- usage_history %>% 
  select(time) %>% 
  cbind(usage_history[, 3:7] - usage_history_lag) %>% 
  mutate(timestamp_idx = rep(1:(NROW(.)/30), each = 30)) # NROW = nrow
  
usage_history_diff %>% head()

usage_history_sum <- usage_history_diff %>% 
  group_by(timestamp_idx) %>% 
  summarise(sum = sum(A+B+C+D+E))

usage_history_sum

foo <- usage_history_sum %>% mutate(sum = signif(sum, 10))
bar <- usage %>% mutate(amount = signif(amount, 10))

join_data <- inner_join(foo, bar, by=c('sum'='amount'))


# 2. 요일별 평균전력 사용량을 도출하시오. 
# 또한 가로축을 요일, 세로축을 평균사용량으로 하여 요일별 평균 사용량을 시각화하여 제출하시오.

foo <- usage_history_diff %>% 
  left_join(join_data, by='timestamp_idx') %>% 
  select(datetime, LETTERS[1:5]) %>% 
  pivot_longer(cols = LETTERS[1:5]) %>% 
  group_by(ymd = substr(datetime, 1, 10), name) %>% 
  summarize(daily_sum = sum(value))

wday_usage <- foo %>% 
  group_by(ymd) %>% 
  summarize(wday_sum = sum(daily_sum)) %>% 
  mutate(wday = wday(ymd, label = T))

wday_usage


wday_usage %>% 
  group_by(wday) %>% 
  summarise(wday_mean=mean(wday_sum)) %>% 
  ggplot(aes(reorder(x=wday, desc(wday_mean)), y=wday_mean, fill=wday)) +
  geom_bar(stat = 'identity') +
  xlab('wday')
  

# 3. 요일별 총 전력 사용량의 평균값의 차이를 분석하여 가장 큰 차이를 보이는 요일은 어떤 요일인지 제시하시오.

aov.result <- aov(wday_sum~factor(wday), data=wday_usage)
summary(aov.result)

TukeyHSD(aov.result)
TukeyHSD(aov.result)$`factor(wday)` %>% 
  as.data.frame() %>% 
  filter(`p adj` < 0.05) %>% 
  arrange(desc(abs(diff))) %>% 
  mutate(day_diff = rownames(.)) %>% 
  ggplot(aes(reorder(x=day_diff, desc(abs(diff))), y=abs(diff))) +
  geom_col()


# 4. 각 날짜별 평균 기온과 용도별 전력 사용량의 관계를 분석하여, 
# 기온과 가장 밀접한 관계를 지닌 사용용도의 종류를 제시하시오.
foo
aa <- foo %>% pivot_wider(names_from = name, values_from = daily_sum)

bar <- aa %>% left_join(weather, by=c('ymd' = 'dt'))

model <- lm(avg_temp~A+B+C+D+E, data=bar)
summary(model)
# 기온과 가장 밀접한 관계를 지닌 사용용도는 A로 확인.
