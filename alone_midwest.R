library(ggplot2)
library(dplyr)

midwest <- as.data.frame(ggplot2::midwest)

# 1) popadults는 해당 지역의 성인인구, poptotal은 전체 인구를 나타냅니다.
#    midwest 데이터에 '전체 인구 대비 미성년 인구 백분율' 변수를 추가하세요. 

midwest <- midwest %>% mutate(mi = (poptotal-popadults) / poptotal * 100)
head(midwest)


# 2) 미성년 인구 백분율이 가장 높은 상위 5개 county(지역)와 미성년 인구 백분율을 출력하세요.



# 3) 미성년 비율 등급 변수를 추가하고, 각 등급에 몇 개의 지역이 있는지 알아보세요.

midwest <- midwest %>% 
              mutate(mi_size = ifelse(mi >= 40, "large", 
                                  ifelse(mi >= 30, "middle", "small")))



# 4) popasian은 해당 지역의 아시아인 인구를 나타냅니다. 
#    '전체 인구 대비 아시아인 인구 백분율' 변수를 추가하고,
#    하위 10개 지역의 state(주), county(지역), 아시아인 인구 백분율을 출력하세요.


  
