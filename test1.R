# 문항 1. ####

library(ggplot2)
mpg <- as.data.frame(ggplot2::mpg)

# [mpg column]
# manufacturer: 제조사
# displ: 배기량(displacement)
# trans: 변속기 종류
# cyl: 실린더 개수
# drv: 구동방식(drive wheel)
# cty: 도시 연비
# hwy: 고속도로 연비
# fl: 연료종류(fuel)

# 1)
# mpg 데이터에서 통합 연비(도시와 고속도로)가 높은 순으로 출력하시오.

# 1안.

mpg[order(-mpg$cty, -mpg$hwy),]

# 2안.

mpg$cthw <- mpg$cty + mpg$hwy
head(mpg)
mpg[order(-mpg$cthw),]


# 2)
# mpg 데이터에서 생산연도별 연료 종류에 따른 통합연비를 연도순으로 출력하시오.

x <- aggregate(data = mpg, cthw~(year+fl), mean)
x[order(x$year),]


# --------------------------------------------------------------

# 문항 2. ####

# 1)
# midwest 데이터를 data.frame으로 불러온 후, 
# 전체인구와 아시아계인구의 데이터의 특징을 설명하시오.

midwest <- as.data.frame(ggplot2::midwest)
class(midwest)

library('psych')
describe(midwest)
colnames(midwest)
aggregate(data = midwest, cbind(poptotal, popdensity, popasian)~state, mean)

# 데이터의 특징 : 각 지역에 사는 인구의 특징을 나타냈다.
#                 (특히, 인종, 민족, 성인/어린이, 빈곤률 등 각 지역의 특징을 나타냈다.)
#
# percasian 컬럼은 각 PID의 (popasian / poptotal) * 100 이다.
# 즉, 각 PID의 인구 대비 아시아계 인구 비율을 알 수 있다.


# 2)
# poptotal 변수(컬럼)를 total로, popasian 변수를 asian으로 변수명을 변경하는 코드를 작성하시오.

# 1안.

install.packages('dplyr')
library('dplyr')

midwest <- rename(midwest, total = poptotal)
midwest <- rename(midwest, asian = popasian)

colnames(midwest)

# 2안.

colnames(midwest)[5] <- 'total'
colnames(midwest)[10] <- 'asian'


# 3)
# 전체 아시아계 인구수와, asian 변수를 이용해 '전체 아시아계 인구 대비 아시아계 인구 백분율' 
# 파생변수(asianpct)를 추가하고, 히스토그램을 그리시오.

ta <- sum(midwest$asian)

midwest$asianpct <- (midwest$asian / ta) * 100

hist(midwest$asianpct)

sum(midwest$asianpct) # 데이터 검증

library(ggplot2)
plot(midwest$asianpct)


# 4)
# 도시(state)기준으로 아시아계 인구가 어떻게 분포하는지 설명하시오.

table(midwest$state)

sa <- aggregate(data = midwest, asianpct~state, sum)
sa

sa[order(-sa$asianpct),] # 아시아계 인구비율로 내림차순.(state 기준) 

sum(sa$asianpct) # 데이터 검증

# 설명.
# state(IL, IN, MI, OH, WI)가 있고, 
# 아시아계 인구 비율이 가장 높은 state는 IL으로 49.8%로 나타났다.
#                    가장 적은 state는 IN으로 6.57%로 나타났다.


# 5)
# 아시아계 인구 백분율(asianpct)의 전체 평균을 구하고, 
# 평균을 초과하면 "lg", 그 외는 "sm"을 부여하는 파생변수(asianrate)를 추가하는 코드를 작성하시오.

am <- mean(midwest$asianpct)

midwest$asianrate <- ifelse(midwest$asianpct > am, 'lg', 'sm')
head(midwest)


# 6)
# "lg"와 "sm"에 해당하는 지역이 얼마나 되는지 빈도 막대그래프(qplot)을 그려보시오.

library(ggplot2)
qplot(midwest$asianrate)
