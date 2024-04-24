# R의 색 체계: RGB

library(scales)
# 🌈색 입력
# 1. 텍스트
show_col(c('red', 'orange', 'yellow', 'green'))

# 2. hex code(16진법): #빨강초록파랑, 00(가장 약한 강도) vs FF(가장 강한 강도)
show_col(c('#FF0000', '#00FF00', '#FFFFFF', '#000000'))

# 3. rgb(,,)
rgb(1, 0, 0) # (빨강, 초록, 파랑): 강도 0~1 => hex code 출력
show_col(c(rgb(0, 0, 1), rgb(0, 0, 0.5), rgb(0, 0, 0.3), rgb(0, 0, 0)))

# 4. 숫자: 1~8 / 9~ 재활용 (ex. 1==9, 2==10)
show_col(1:9)

####
# 🌈colormap: 색의 모음
# 1.
rainbow(10) # (색의 개수) / hex code 출력
show_col(rainbow(10), ncol=1, cex_label=0.7, border=NA)
show_col(heat.colors(10), ncol=1, cex_label=0.7, border=NA)
show_col(topo.colors(10), ncol=1, cex_label=0.7, border=NA)

# 2.
library(RColorBrewer) # ggplot2 내장
display.brewer.all()
# 순차형(sequential): 양적 특성 / 크기O 
# 질적(qualitative): 범주형 특성 / 크기X / 순서X / 채도, 명도 비슷 / 색상 다양
# 확산형(diverging): 양적 특성 / 색 2개 / 양극단으로 갈수록 채도⬆️, 명도⬇ 

brewer.pal(7, 'RdBu') # (색의 개수, 컬러맵) => hex code 출력
display.brewer.pal(7, 'RdBu') 

# 3.
#install.packages('scico')
library(scico) # 색맹 / 흑백으로 인쇄해도 읽기 좋다.
scico_palette_show()
scico(7, palette = 'acton')
show_col(scico(7, palette = 'acton'))

####
setwd('/Users/hj/Downloads/data')
load('sah.RData')

library(ggplot2)

# 1. 범주형 변수: BMI.cat 
# 1-1. 산점도: 점,선의 색
ggplot(sah, aes(age, ldl, color=BMI.cat)) + geom_point() 
ggplot(sah, aes(age, ldl, color=BMI.cat)) + geom_point() +
  scale_color_manual(values=c('tomato', 'orange', 'skyblue', 'forestgreen'))
ggplot(sah, aes(age, ldl, color=BMI.cat)) + geom_point() +
  scale_color_manual(values=c('underweight'='tomato', 'normal'='orange', 'overweight'='skyblue', 'obese'='forestgreen'))
ggplot(sah, aes(age, ldl, color=BMI.cat)) + geom_point() +
  scale_color_brewer(palette = 'Set2') 

# 1-2. 막대그래프: 면의 색(명도/채도 채널 효과적)
ggplot(sah, aes(BMI.cat)) + geom_bar() 
ggplot(sah, aes(BMI.cat, fill=BMI.cat)) + geom_bar() 
ggplot(sah, aes(BMI.cat, fill=BMI.cat)) + geom_bar() +
  scale_fill_manual(values=c('tomato', 'orange', 'skyblue', 'forestgreen'))
ggplot(sah, aes(BMI.cat, fill=BMI.cat)) + geom_bar() +
  scale_fill_manual(values=c('#66C2A5', '#FC8D62', '#8DA0CB', '#E78AC3'))
ggplot(sah, aes(BMI.cat, fill=BMI.cat)) + geom_bar() +
  scale_fill_brewer(palette = 'Blues') 

# 1-3.
ggplot(sah, aes(age, ldl, color=BMI.cat)) + geom_point(color='dodgerblue3') 

ggplot(sah, aes(BMI.cat)) + geom_bar(fill='dodgerblue3') 
ggplot(sah) + geom_bar(aes(BMI.cat), fill='dodgerblue3') 

ggplot(sah) + geom_bar(aes(BMI.cat, fill='dodgerblue3')) # X


# 2. 연속형 변수: obesity
# 2-1. 산점도: 점의 색 => 효과적X (학습을 위해 실습)
e <- ggplot(sah, aes(age, ldl, color=obesity)) + geom_point() 
e
e + scale_color_gradient(low='yellow', high='red')
e + scale_color_viridis_c()
e + scale_color_viridis_c(option='A')
e + scale_color_viridis_c(option='E')

e + scale_color_distiller(palette = 'Purples')
e + scale_color_scico(palette = 'vik')

e + guides(color=guide_colorbar(reverse = TRUE))
e + theme(legend.position = 'bottom') +
  guides(color=guide_colorbar(direction = 'horizontal'))