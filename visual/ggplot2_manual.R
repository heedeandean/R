setwd('/Users/hj/Downloads/data') # 작업 디렉토리 지정 
load('sah.RData')

library(ggplot2)
# [mapping]: 데이터를 화면에 대응.
# 1. 층(layer): geom_*(), 기본 틀, 필수⭐️, 원형과 채널 정보.
# 2. sacle: 범례.. 
# 3. 좌표(coord)
# 4. 분할(facet): 나열
# 5. theme: 배경색..

####
# 1. layer
# - 막대 그래프: geom_bar(), geom_col()
# - 선 그래프: geom_line(), geom_path()
# - 산점도: geom_point()
# - 평활곡선 그래프: geom_smooth()
# - 상자 그림: geom_boxplot()
# - 오차막대 그래프: geom_errorbar()
# - 오차선 그래프: geom_pointrange()
# - geom_histogram()
# - 밀도곡선 그래프: geom_density()
# - 바이올린 플롯: geom_violin()

ggplot(sah) + geom_point(aes(age, sbp)) 
ggplot(sah, aes(age, sbp)) + geom_point()
ggplot(sah, aes(age, sbp, color=chd)) + geom_point()
ggplot(sah, aes(age, sbp, shape=chd)) + geom_point()

ggplot(sah, aes(age, sbp)) + geom_smooth() 
ggplot(sah, aes(age, sbp, color=chd)) + geom_smooth()
ggplot(sah, aes(age, sbp)) + geom_point() + geom_smooth() # 중첩

####
# 2. scale: 디테일 수정(가로축, 세로축, 범례, 채널)
# 연속형 변수
ggplot(sah, aes(age, sbp, color=chd)) + geom_point() +
  scale_x_continuous(limits = c(0,100)) # 가로축(x) 최솟값, 최댓값 지정.

a <- ggplot(sah, aes(age, sbp, color=chd)) + geom_point() 
a + scale_x_continuous(limits = c(0,100)) + scale_y_continuous(limits = c(90, 250))
a + lims(x=c(0,100)) + lims(y=c(90, 250))
a + xlim(c(0,100)) + ylim(c(90,250))

# ‼주의: 디폴트 설정보다 축의 범위 좁힐 때. => 범위 바깥으로 밀려난 데이터 값은 내부적으로 결측치 처리됨.
# 변환데이터 사용하는 그래프는 예상치 못한 결과.(ex. 상자그림, 평활곡선) (cf. 산점도는 원데이터를 그대로 보여주기 때문에 영향 X)
a + geom_smooth()
a + geom_smooth() + xlim(c(20,40))
a + geom_smooth() + coord_cartesian(xlim=c(20,40)) # ⭐️좁힌 범위만 출력

a + scale_x_continuous(limits=c(0,100))
a + scale_x_continuous(limits=c(0,100), expand = expansion(0)) # 양쪽 여분 제거

# 양쪽 여분 추가
a + scale_x_continuous(limits=c(0,100), expand = expansion(add=10)) # 절대적인 추가분 지정.
a + scale_x_continuous(limits=c(0,100), expand = expansion(mult=0.3)) # 상대적인 추가분 지정, 가로축 전체 길이의 몇 배.

a
a + scale_x_continuous(breaks=c(20,40,60)) # 눈금 위치
a + scale_x_continuous(breaks=c(20,40,60), labels=c('20.0', '40.0', '60.0')) # 눈금 이름

ggplot(sah, aes(age, ldl)) + geom_point()
ggplot(sah, aes(age, ldl)) + geom_point() + scale_y_continuous(trans='log')
ggplot(sah, aes(age, log(ldl))) + geom_point() 
# trans 옵션
# - log/log10/log2
# - reciproca: 1/x(역순)
# - reverse: 내림차순
# - sqrt: 제곱근


# 범주형 변수: BMI.cat
b <- ggplot(sah, aes(BMI.cat, ldl)) + geom_boxplot()
b

b + scale_x_discrete(limits=c('obese', 'overweight', 'unknown'))

# 눈금 이름 바꾸기
b + scale_x_discrete(labels=c('UNDERWEIGHT', 'NORMAL', 'OVERWEIGHT', 'OBESE'))
b + scale_x_discrete(labels=c(overweight='OVERWEIGHT', obese='OBESE'))

# 특정 범주의 위치에만 눈금 표시.
b + scale_x_discrete(breaks=c('obese', 'overweight'))

b + scale_x_discrete(limits=c('normal', 'overweight', 'obese'),
                     breaks=c('obese', 'overweight'),
                     labels=c(overweight='OVERWEIGHT', obese='OBESE'))

# 눈금 이름을 90도 눕히기
b + scale_x_discrete(guide = guide_axis(angle = 90))
b + guides(x=guide_axis(angle = 90))

ggplot(sah, aes(age, ldl, size=obesity)) + # size=연속형 변수를 면적 채널을 이용해 표시
  geom_point() +
  scale_size(range=c(0.3,3)) # 점의 크기 (default: c(1,6))

# 모양 채널
ggplot(sah, aes(age, ldl, shape=BMI.cat)) + geom_point()
ggplot(sah, aes(age, ldl, shape=BMI.cat)) + geom_point() +
  scale_shape_manual(values=c('underweight'=1, 'normal'=2, 'overweight'=3, 'obese'=4))

####
# 3. 좌표(coord): 데이터 변환 후 적용. (cf. scale(): 데이터 변환 전 적용.)
a
a + coord_flip() # 가로축 <-> 세로축 뒤바꾸기
b
b + coord_flip() 

#install.packages('datarium')
library(datarium)
ggplot(mice2, aes(before, after)) + geom_point() 
ggplot(mice2, aes(before, after)) + geom_point() + coord_fixed() # 가로축, 세로축 스케일이 같도록 조정.

# etc..
# coord_polar(): 각도 채널(ex. 원그래프)
# coord_quickmap(), coord_map(): 지리형 데이터

####
# 4. 분할(facet): 조건부 플롯(범주형 변수)
a + facet_wrap(~BMI.cat) 
a + facet_wrap(~BMI.cat, ncol=1) 
a + facet_wrap(~BMI.cat, nrow=1) 

a + facet_grid(.~famhist) # 행 기준 변수~열 기준 변수(지정X: .)
a + facet_grid(chd~.)
a + facet_grid(chd~famhist)

####
# 5. theme: 데이터와 직접 관련 X, 외관과 관련된 요소 조정.
d <- ggplot(sah, aes(age, sbp, color=chd)) + geom_point()
d
d + xlab('Age') + ylab('SBP') + ggtitle('Scatterplot')

# theme()
# > plot.title: 그래프 제목 - element_text(face='글씨체', color, size='글자 크기', hjust)
# > plot.background: 그래프 배경 - element_rect()
# > plot.margin - margin()
# > axis.title.x: 가로축 제목 - element_text()
# > axis.title.y: 세로축 제목 - element_text()
# > axis.text.x: 가로축 눈금 이름 
# > axis.text.y: 세로축 눈금 이름 
# > legend.title: 범례 제목
# > legend.text
# > legend.title.align: 범례 제목 위치 - (0=오른쪽, 1=왼쪽)
# > legend.position: 범례 위치 - ('right', 'left, 'bottom', 'none')
# > panel.background: 그래프 배경 - element_rect()
# > panel.grid.major.x: 수직 보조선 - element_line()
# > panel.grid.major.y: 수평 보조선 - element_line()
# > ascpect.ratio - 세로/가로 길이
# > element_blank(): 요소 제거

d + labs(x='Age', y='SBP', title = 'Scatterplot', color='CHD') + # subtitle='그래프 부제목'
  theme(plot.title=element_text(face = 'bold', color='red')) 
d + labs(x='Age', y='SBP', title = 'Scatterplot', color='CHD') + 
  theme(plot.title=element_text(hjust = 0.5)) # 가운데 정렬
d + labs(x='Age', y='SBP', title = 'Scatterplot', color='CHD') +
  theme(plot.title=element_text(hjust = 1)) # 오른쪽 정렬

d + labs(x='Age', y='SBP', title = 'Scatterplot', color='CHD') + 
  theme(axis.title.x=element_text(face = 'italic', size='16'),
        axis.title.y=element_blank(),
        legend.title = element_text(color='blue'),
        legend.position = 'bottom',
        aspect.ratio = 9/16) 

d
ggsave('scatterplot.pdf', width=5, height=4) # 그래프 저장




