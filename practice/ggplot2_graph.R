library(ggplot2)

# 산점도.

# 1) [mpg 데이터] x축은 cty(도시연비), y축은 hwy(고속도로 연비)로 된 산점도.

mpg <- as.data.frame(ggplot2::mpg)

ggplot(data = mpg, aes(x = cty, y = hwy)) +
  geom_point()

# cty와 hwy는 비례하다.


# 2) [midwest 데이터] x축은 poptotal(전체 인구), y축은 popasian(아시아인 인구)로 된 산점도.
#    (전체 인구는 50만 명 이하, 아시아인 인구는 1만 명 이하인 지역만 표시.)

# midwest : 미국 지역별 인구통계 데이터.

midwest <- as.data.frame(ggplot2::midwest)

options(scipen = 99) # 축을 정수로 표현.
options(scipen = 0) # 축을 지수로 표현(default).




