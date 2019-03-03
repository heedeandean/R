library(ggplot2)
mpg = as.data.frame(ggplot2::mpg)
midwest = as.data.frame(ggplot2::midwest)

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

mpg[order(-mpg$cty, -mpg$hwy),]

# 2)
# mpg 데이터에서 생산연도별 연료 종류에 따른 통합연비를 연도순으로 출력하시오.

table(mpg$year)

year1 = mpg[mpg$year == 1999,]
year2 = mpg[mpg$year == 2008,]

year1 = aggregate(data=year1, cbind(cty, hwy)~fl, mean)
year1$year = 1999

year2 = aggregate(data=year2, cbind(cty, hwy)~fl, mean)
year2$year = 2008

mpg_year_fl = rbind(year1, year2)
mpg_year_fl[order(mpg_year_fl$year),]

# 3)
# midwest 데이터를 data.frame으로 불러온 후, 데이터의 특징을 설명하시오.

midwest = as.data.frame(ggplot2::midwest)
class(midwest)

# 데이터의 특징 : 각 지역에 사는 인구의 특징을 나타냈다.
#                 (특히, 인종, 민족, 성인/어린이, 빈곤률 등 각 지역의 특징을 나타냈다.)


# 4)
# poptotal 변수(컬럼)를 total로, popasian 변수를 asian으로 변수명을 변경하는 코드를 작성하시오.

install.packages('dplyr')
library('dplyr')

midwest = rename(midwest, total = poptotal)
midwest = rename(midwest, asian = popasian)

colnames(midwest)

# 5)
# total, asian 변수를 이용해 `전체 인구 대비 아시아계 인구 백분율` 파생변수(asianpct)를 추가하고, 히스토그램을 그려, 도시들이 어떻게 분포하는지 설명하시오.




# 6)
# 아시아계 인구 백분율(asianpct)의 전체 평균을 구하고, 평균을 초과하면 "lg", 그 외는 "sm"을 부여하는 파생변수(asianrate)를 추가하는 코드를 작성하시오.


# 7)
# "lg"와 "sm"에 해당하는 지역이 얼마나 되는지 빈도 막대그래프(qplot)을 그려보시오.
