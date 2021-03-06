# ggplot2 패키지의 그래프 함수 정리.
# ggplot() + <GEOM_FUNCTION>

library(dplyr)
library(ggplot2)

# 1) geom_point() - 산점도

save(smdt, file = 'data/smdt.rda')
load('data/smdt.rda')
head(smdt)

ggplot() + geom_point(data = smdt, aes(x = stuno, y = Korean))

ggplot() +
  geom_point(data = smdt,
             aes(x = stuno, y = Korean),
             color = 'blue', size = 5)

load('data/d.rda')
head(d)
d2 <- d %>% filter(학번 >= 30000)
head(d2)

ggplot(d2, aes(cls, kor)) +
  geom_point(aes(color = cls, size = kor), 
             alpha = 0.3)


# 2) geom_line() - 꺾은선 그래프

mpg <- as.data.frame(ggplot2::mpg)
head(mpg)

d3 <- mpg %>% 
        group_by(manufacturer, displ) %>% 
        summarise(m1 = mean(cty), m2 = mean(hwy))
head(d3)

# 배기량에 따른 도시, 고속도로 평균 연비

ggplot(d3, aes(x = displ)) + 
  geom_line(aes(y = m1, color = 'cty')) + 
  geom_line(aes(y = m2, color = 'hwy'), size = 1) +
  scale_colour_manual("", breaks = c("cty", "hwy"),
                      values = c("red", "blue")) +
  xlab("배기량(cc)") +
  xlim(1, 8) +
  scale_y_continuous("연비(M/h)", limits = c(5, 45)) +
  labs(title = '타이틀', subtitle = '서브 타이틀') 


# 3) geom_histogram() - 히스토그램

# 연비 히스토그램 (클래스별)

ggplot(mpg, aes(displ)) +
  geom_histogram(aes(fill = class), 
                 binwidth = .3,     # 또는  bins = 5
                 col = 'black',       # line color
                 size = .1) +         # line size
  labs(title = 'Title', subtitle = 'Sub Title')


# 4) geom_bar() - 막대그래프

# 제조사별 자동차 수 (클래스별)

ggplot(mpg, aes(manufacturer)) +
  geom_bar(aes(fill = class),
           width = 0.5) +
  theme(axis.text.x = element_text(angle = 45,       # 글씨의 기울기
                                   vjust = 0.6)) +   # 글씨의 하단 맞춤(띄우기)
  scale_fill_discrete(name = "class") +            # legend
  labs(title = 'Title', subtitle = 'Sub Title')


# 5) geom_density() - 분포, 밀도그래프

# 실린더 수에 따른 시내주행거리(cty)

ggplot(mpg, aes(cty)) +
  geom_density(aes(fill = factor(cyl)), alpha = 0.8) +
  labs(title = "밀도그래프", subtitle = "실린더수에 따른 시내연비의 밀도그래프",
       caption = "Source: ggplot2::mpg",
       x = "도시 연비",
       fill = "실린더수")
