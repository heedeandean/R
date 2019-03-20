
library(ggplot2)

options(encoding = 'utf-8')
data <- read.csv("data/성적.csv")

save(data, file = 'data/data.rda')
load("data/data.rda")

ggplot(data = data %>% filter(수학 >= 90), aes(반)) +
  geom_bar(aes(fill = 성별), width = 0.7) +
  labs(title = '수학 우수 학생', subtitle = '(80점 이상)', x = '학급', y ='학생수', fill = '성별')

# 

ggplot(data[data$수학 >= 90, ], aes(수학)) +
  geom_density(aes(fill = factor(반)), alpha = 0.5) +
  labs(title = "반별 수학 우수 학생", subtitle = "(수학 성적 A+)", 
       x = "성적", y = "밀도", fill = "학급", caption = "기준점수 >= 90")

