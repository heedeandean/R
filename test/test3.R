library(ggplot2)
library(dplyr)

# 1)
# mpg데이터에서 연도별 배기량에 따른 도시/고속도로 연비를 꺽은선으로 그리시오.
# (단, 2008년은 굵은 선으로 표현하시오.)

mpg2 <- mpg %>% 
        group_by(year, displ) %>%
        summarise(m1 = mean(cty), m2 = mean(hwy))
mpg2 

ggplot() +
  geom_line(data = mpg2 %>% filter(year == '1999'), aes(x = displ, y = m1, color = '1999 cty')) +
  geom_line(data = mpg2 %>% filter(year == '1999'), aes(x = displ, y = m2, color = '1999 hwy')) +
  geom_line(data = mpg2 %>% filter(year == '2008'), aes(x = displ, y = m1, color = '2008 cty'), size = 2) +
  geom_line(data = mpg2 %>% filter(year == '2008'), aes(x = displ, y = m2, color = '2008 hwy'), size = 2) +
  scale_y_continuous(limits = c(0, 50)) +
  scale_color_manual("", breaks = c("1999 cty", "1999 hwy", "2008 cty", "2008 hwy"),
                     values = c("gray", "pink", "blue", "darkblue")) +
  labs(title = '연도별 통합 연비', subtitle = '(굵은선은 2008년)', 
       x = '배기량(cc)', y = '연비(M/h)', colour = '')
  

# 2)
# data(성적.csv) 데이터에서 국어 성적이 80점 이상인
# 학생들의 수를 성비가 보이도록 학급별로 막대그래프를 그리시오.

library(ggplot2)

options(encoding = 'utf-8')
data <- read.csv("data/성적.csv")

save(data, file = 'data/data.rda')
load("data/data.rda")

ggplot(data = data %>% filter(국어 >= 80), aes(반)) +
  geom_bar(aes(fill = 성별), width = 0.7) +
  labs(title = '국어 우수 학생', subtitle = '(80점 이상)', x = '학급', y ='학생수', fill = '성별')
  

# 3)
# 국어 성적이 95점 이상인 학생들의
# 학급별 밀도그래프를 그리시오.

# 1안. 

d3 <- data %>% filter(국어 >= 95)
d3
      
ggplot(d3, aes(국어)) +
  geom_density(aes(fill = factor(반)), alpha = 0.5) +
  labs(title = "반별 국어 우수 학생", subtitle = "(국어 성적 A+)", 
       x = "성적", y = "밀도", fill = "학급", caption = "기준점수 >= 95")

# 2안.

ggplot(data[data$국어 >= 95, ], aes(국어)) +
  geom_density(aes(fill = factor(반)), alpha = 0.5) +
  labs(title = "반별 국어 우수 학생", subtitle = "(국어 성적 A+)", 
       x = "성적", y = "밀도", fill = "학급", caption = "기준점수 >= 95")


# 4)
# midwest데이터에서 전체인구와 아시아계 인구의 
# 관계를 알아보기 위한 그래프를 그리시오.
# (단, 전체인구는 50만명 이하, 아시아계인구는 1만명 이하만 표시.)

midwest <- as.data.frame(ggplot2::midwest)
head(midwest)

# 1안.

m2 <- midwest %>% filter(poptotal <= 500000 & popasian <= 10000)

ggplot(m2, aes(x = area, y = popasian)) +
       geom_point(aes(col = state, size = popasian)) +
       geom_smooth(method = "auto", se = F) + 
       xlim(c(0, 0.1)) +
       ylim(c(0, 10000)) +
       labs(subtitle = "Area Vs popasian", x = "Area", y = "popasian",
            title = "각 Area 별 아시아계 인구",
            caption = "Source : ggplot2::midwest") 

# 2안.

ggplot(midwest) +
  geom_point(aes(x = poptotal, y = popasian), alpha = 0.5) +
  xlim(0, 500000) + ylim(0, 10000) +
  labs(title = "전체인구 대비 아시아계 인구", x = "전체 인구", y = "아시아계 인구")
