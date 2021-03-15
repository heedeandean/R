library(ggplot2)
library(dplyr)
mpg <- as.data.frame(ggplot2::mpg)
midwest <- as.data.frame(ggplot2::midwest) # midwest : 미국 지역별 인구통계 데이터.

# 산점도 ####

# 1) [mpg 데이터] x축은 cty(도시연비), y축은 hwy(고속도로 연비)로 된 산점도.

ggplot(data = mpg, aes(x = cty, y = hwy)) +
  geom_point()

# cty와 hwy는 비례하다.


# 2) [midwest 데이터] x축은 poptotal(전체 인구), y축은 popasian(아시아인 인구)로 된 산점도.
#    (전체 인구는 50만 명 이하, 아시아인 인구는 1만 명 이하인 지역만 표시.)

ggplot(data = midwest, aes(x = poptotal, y = popasian)) +
  geom_point() +
  xlim(0, 500000) +
  ylim(0, 10000)

options(scipen = 99) # 축을 정수로 표현.
options(scipen = 0) # 축을 지수로 표현(default).


# 막대 그래프 ####

# [mpg 데이터]
# 1) suv(차종)을 대상으로 평균 cty(도시 연비)가 가장 높은 회사 다섯 곳을 막대그래프로 표현.
#    (막대는 연비가 높은 순으로 정렬.)

df_mpg <- mpg %>% 
            filter(class == 'suv') %>% 
            group_by(manufacturer) %>% 
            summarise(mean_cty = mean(cty)) %>% 
            arrange(desc(mean_cty)) %>% 
            head(5)

ggplot(data = df_mpg, aes(x = reorder(manufacturer, -mean_cty), y = mean_cty)) +
  geom_col()

# 회사 subaru에서 생산한 'suv'차종의 도시 연비가 제일 높다.


# 2) 자동차 종류별 빈도를 표현한 막대그래프.

ggplot(data = mpg, aes(x = class)) +
  geom_bar()

# 자동차 종류 중 suv가 가장 많다.


# 선 그래프 ####

# economics 데이터 : 미국의 경제 지표들을 월별로 나타낸 데이터.

# 1) 시간에 따른 psavert(개인 저축률)의 변화를 나타낸 시계열 그래프.

ggplot(data = economics, aes(x = date, y = psavert)) +
  geom_line()


# 상자 그림 그래프 ####

# [mpg 데이터]
# 1) class(자동차 종류)가 "compact", "subcompact", "suv"인 
#    자동차의 cty(도시 연비)가 어떻게 다른지 비교해 보려고 합니다.
#    세 차종의 cty를 나타낸 상자 그림을 만들어 보세요.

mc <- mpg %>% filter(class %in% c("compact", "subcompact", "suv"))

ggplot(data = mc, aes(x = class, y = cty)) +
  geom_boxplot()

