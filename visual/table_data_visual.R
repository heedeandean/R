setwd('/Users/hj/Downloads/data')
load('sah.RData')
library(ggplot2)

# 1. 산점도(scatterplot)
ggplot(sah, aes(age, ldl)) + geom_point()
ggplot(sah, aes(age, ldl)) + geom_point(shape=1) 
ggplot(sah, aes(age, ldl)) + geom_point(size=1, color='forestgreen') 

# 1-1. 범주형 변수: famhist
# 1-1-1. 색상 채널
ggplot(sah, aes(age, ldl, color=famhist)) + geom_point()
ggplot(sah, aes(age, ldl, color=famhist)) + geom_point(shape=1, size=2)

# 1-1-2. 모양 채널
ggplot(sah, aes(age, ldl, shape=famhist)) + geom_point()
ggplot(sah, aes(age, ldl, shape=famhist)) + geom_point() +
  scale_shape_manual(values=c(1, 4)) # 직접 지정

# 1-2. 이산형 변수
ggplot(Loblolly, aes(age, height)) + geom_point() # 겹쳐보임
ggplot(Loblolly, aes(age, height)) + geom_jitter() # 랜덤 노이즈를 더해 점을 흩뜨림
ggplot(Loblolly, aes(age, height)) + geom_jitter(width=0.5, height=0) # 랜덤 노이즈 크기 조정

# 1-3. 회귀직선: 두 변수 간 관계, 경향성
ggplot(sah, aes(age, ldl)) + geom_point() + geom_smooth(method = lm)
ggplot(sah, aes(age, ldl)) + geom_point() + geom_smooth(method = lm, level=0.99) # 신뢰구간 default: 0.95
ggplot(sah, aes(age, ldl)) + geom_point() + 
  geom_smooth(method = lm, se=FALSE, # 신뢰구간 X
              color='red') 
# 1-4. 산점도 행렬
plot(iris[, 1:4])

# 1-5.
f <- ggplot(sah, aes(age, ldl)) + geom_point()

# 1-5-1.
f + annotate('text', x=50, y=15, label='ID 17')
f + annotate('text', x=c(50, 57), y=c(15, 13.8), label=c('ID 17', 'ID 414'))

# 1-5-2. 사각형 추가
f + annotate('rect', xmin=30, xmax=40, ymin=6, ymax=12, alpha=0.3, fill='skyblue')
f + annotate('rect', xmin=60, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.3, fill='skyblue')

# 1-5-3. 선 추가 - segment 

# 1-6. 보조선
f + geom_hline(yintercept=13, linetype='dashed', color='red') # 수평선 
f + geom_vline(xintercept=60) # 수직선 
f + geom_vline(xintercept=50) + geom_hline(yintercept = 10)
f + geom_abline(intercept=3, slope = 0.05) # 절편, 기울기


#install.packages('GGally')
#remove.packages('GGally')
library(GGally)
ggpairs(iris[, 1:4]) # 밀도곡선, 상관계수

# 2. 막대그래프
# 2-1. 범주형 변수 + 양적 변수(막대 높이)
library(dplyr)

# 변환 데이터: 그룹별로 평균 계산
sah2 <- sah %>% group_by(BMI.cat) %>% 
  summarise(mean.ldl=mean(ldl)) 
sah2
ggplot(sah2, aes(BMI.cat, mean.ldl)) + geom_col()
ggplot(sah2, aes(BMI.cat, mean.ldl)) + geom_col(width=0.5) # default: 0.9
ggplot(sah2, aes(BMI.cat, mean.ldl)) + geom_col(width=1) # max: 1
ggplot(sah2, aes(BMI.cat, mean.ldl)) + geom_col() +
  geom_text(aes(label=round(mean.ldl, 2)), vjust=-0.2) # 수직 위치 조정

sah3 <- sah %>% group_by(BMI.cat, chd) %>% 
  summarise(mean.ldl=mean(ldl)) 
sah3
ggplot(sah3, aes(BMI.cat, mean.ldl, fill=chd)) + geom_col(position = 'dodge') 

# 2-2. 범주형 변수(1개) - 도수분포표(자동) => y(X) 
ggplot(sah, aes(x=BMI.cat)) + geom_bar() 
ggplot(sah, aes(x=BMI.cat)) + geom_bar(width = 0.7) +
  geom_text(aes(label=..count..), stat='count', vjust=1.5, color='white')

ggplot(sah, aes(x=BMI.cat)) + geom_bar(aes(y=..count../sum(..count..))) + # 0~1
  ylab('percent') + scale_y_continuous(labels = scales::percent) # 백분율

ggplot(sah, aes(x=BMI.cat)) + geom_bar(aes(y=..count../sum(..count..)*100)) + # %
  ylab('Percent(%)')
  
# 2-3. 범주형 변수(2개) - 누적 막대그래프
ggplot(sah, aes(x=BMI.cat, fill=chd)) + geom_bar()
ggplot(sah, aes(x=BMI.cat, fill=chd)) + geom_bar(position = 'fill') +
  ylab('Percent') + scale_y_continuous(labels = scales::percent)

# 3. histogram
# x=양적변수, y=도수분포표(자동)
ggplot(sah, aes(x=sbp)) + geom_histogram()
ggplot(sah, aes(x=sbp)) + geom_histogram(fill='skyblue', color='black')

# 구간을 어떻게 설정하느냐 따라서, 같은 데이터도 달라 보일 수 있다. (*default: 30)
ggplot(sah, aes(x=sbp)) + 
  geom_histogram(fill='skyblue', color='black', binwidth = 20) # 구간의 너비

ggplot(sah, aes(x=sbp)) + 
  geom_histogram(fill='skyblue', color='black', binwidth = 20, boundary=120) # 첫 구간(막대) 끝점

ggplot(sah, aes(x=sbp)) + 
  geom_histogram(fill='skyblue', color='black', breaks=seq(100,220,10)) # 모든 구간의 끝점

#
ggplot(sah, aes(x=sbp, fill=chd)) + # 색 채널
  geom_histogram(position = 'identity', # 중첩
                 alpha=0.5) # 투명도(0~1) 

# 4. 밀도곡선: 구간 폭을 0에 가깝게 하여, 매끄럽게 이어지는 히스토그램(계단 모양 X) - 연속형 변수
ggplot(sah, aes(x=sbp)) + geom_density()
ggplot(sah, aes(x=sbp)) + geom_density(adjust=0.2) # bandwidth 지정 (*default=1): 값이 클수록 완만한 곡선. 
ggplot(sah, aes(x=sbp)) + geom_density(adjust=3)

#
ggplot(sah, aes(x=sbp, color=chd)) +  # 중첩
  geom_density(key_glyph=draw_key_path) 

# 5. 상자그림: 다섯수치요약(그룹별 분포 비교 용이)
ggplot(sah, aes(chd, sbp)) + geom_boxplot()
ggplot(sah, aes(chd, sbp)) + # aes(범주형 변수, 양적 변수)⭐️
  geom_boxplot(width=0.5, outlier.size = 3, outlier.shape = 4)

class(sah$chd) 
ggplot(Loblolly, aes(age, height)) + geom_boxplot() + xlab('age') # 의도와 다른 결과. Why? 범주형 변수: 반드시 factor형!⭐️⭐️⭐️ (* as.factor()||factor())
ggplot(Loblolly, aes(as.factor(age), height)) + geom_boxplot() + xlab('age')

# 5-1. 상자 1개
ggplot(sah, aes(1, sbp)) + geom_boxplot() +
  scale_x_continuous(breaks=NULL) + # 눈금 삭제
  theme(axis.title.x = element_blank()) # 가로축 제목 삭제

# 6. 바이올린 플롯: 데이터 밀도를 가로폭으로 나타냄.
ggplot(sah, aes(chd, sbp)) + geom_violin()

# 7. 선그래프: 시계열 - 경향성
ggplot(BOD, aes(Time, demand)) + geom_line()
ggplot(BOD, aes(Time, demand)) + 
  geom_line(linetype='dashed', linewidth=2, color='forestgreen')
ggplot(BOD, aes(Time, demand)) + geom_line() + geom_point() # 산점도

# 7-1. 선 아래쪽 면적이 색깔로 채워진 그래프.
ggplot(BOD, aes(Time, demand)) + geom_area()
ggplot(BOD, aes(Time, demand)) + geom_area(color='black', fill='skyblue', alpha=0.3)

# 7-2. 스파게티 플롯: 중첩
ggplot(Orange, aes(age, circumference, group=Tree)) +
  geom_line()

Orange$Tree

ggplot(Orange, aes(age, circumference, color=Tree)) + # 범례
  geom_line()
ggplot(Orange, aes(age, circumference, color=Tree)) + 
  geom_line() + scale_color_discrete(limits=c('1','2','3','4','5'))

# 8. 원그래프 - 각도 채널
# 8-1. 
ggplot(sah, aes(x=1, fill=BMI.cat)) + geom_bar() + # 범주형 변수 - 막대그래프 그린 후 
  coord_polar('y') + theme_void()

# 8-2.
vote <- data.frame(response=c('BLACKPINK', 'TWICE', 'RED VELVET', 'Other'), perc=c(45, 23, 21, 11)) # 도수분포표
ggplot(vote) + geom_col(aes(x='', y=perc, fill=response)) +
  coord_polar(theta='y') + theme_void()