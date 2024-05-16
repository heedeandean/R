library(ggplot2)

# 산점도
ggplot(mpg) + geom_point(mapping=aes(x=displ, y=hwy))
ggplot(mpg) + geom_point(aes(displ, hwy))
ggplot(mpg) + geom_point(aes(displ, hwy, color=class))
ggplot(mpg) + geom_point(aes(displ, hwy), color='blue')

# 평활 곡선 그래프
ggplot(mpg) + geom_smooth(aes(displ, hwy))

ggplot(mpg) + geom_point(aes(displ, hwy)) + geom_smooth(aes(displ, hwy))
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth()
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(se=F) # 신뢰구간 제거.
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(span=0.2) 
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(span=1) # 값이 클수록 완만, 매끄러움.

ggplot(mpg, aes(displ, hwy)) + geom_point(aes(color=drv)) + geom_smooth()
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(aes(color=drv))
ggplot(mpg, aes(displ, hwy, color=drv)) + geom_point() + geom_smooth()
ggplot(mpg, aes(displ, hwy)) + geom_point() + geom_smooth(method='lm') # 회귀직선

ggplot(mpg, aes(drv, hwy)) + geom_point()
ggplot(mpg, aes(drv, hwy)) + geom_boxplot()
ggplot(mpg, aes(drv, hwy)) + geom_violin()
ggplot(mpg, aes(hwy)) + geom_histogram()
ggplot(mpg, aes(hwy)) + geom_freqpoly() # 도수분포 다각형
ggplot(mpg, aes(hwy, color=drv)) + geom_freqpoly()

#
ggplot(mpg, aes(hwy)) + geom_histogram() + facet_wrap(~drv)
ggplot(mpg, aes(hwy)) + geom_histogram() + facet_wrap(~drv, nrow = 3)
ggplot(mpg, aes(hwy)) + geom_histogram() + facet_grid(.~drv)
ggplot(mpg, aes(hwy)) + geom_histogram() + facet_grid(drv~.)
ggplot(mpg, aes(hwy)) + geom_histogram(fill='skyblue', color='black')
ggplot(mpg, aes(hwy)) + geom_histogram(fill='skyblue', 
                                       color='black',
                                       breaks=seq(10,45,5))

#
ggplot(mpg, aes(displ, hwy)) + geom_point()
ggplot(mpg, aes(displ, hwy)) + geom_jitter() # 흩뜨려진 산점도.
ggplot(mpg, aes(displ, hwy)) + geom_point(alpha=0.3) # 투명도
ggplot(mpg, aes(displ, hwy)) + geom_point(alpha=0.3) +
  xlab('Displacement') + ylab('Highway MPG')
ggplot(mpg, aes(displ, hwy)) + geom_point(alpha=0.3) + 
  xlab('') + ylab(NULL)
ggplot(mpg, aes(displ, hwy)) + geom_point(alpha=0.3) + 
  ggtitle('Scatterplot') # 제목
ggplot(mpg, aes(displ, hwy)) + geom_point(alpha=0.3) + 
  xlim(1,7) + ylim(0,60) # 가로축, 세로축 범위
ggplot(mpg, aes(displ, hwy)) + geom_point(alpha=0.3) + 
  scale_x_continuous(breaks = c(2,4,6)) # 축 눈금
ggplot(mpg, aes(displ, hwy)) + geom_point(alpha=0.3) + 
  scale_x_continuous(breaks = c(2,4,6), labels = c('2.0', '4.0', '6.0'))
ggplot(mpg, aes(displ, log(hwy))) + geom_point(alpha=0.3) # 축 변환
ggplot(mpg, aes(displ, hwy)) + geom_point(alpha=0.3) + 
  scale_y_continuous(trans='log')
ggplot(mpg, aes(displ, hwy)) + geom_point(alpha=0.3) + 
  scale_y_continuous(trans='log', breaks=seq(15, 40, 5))

ggplot(mpg) + geom_point(aes(displ, hwy, color=drv)) # 범주형 색상 표현.
ggplot(mpg) + geom_point(aes(displ, hwy, color=drv)) +
  scale_color_manual(values = c('tomato', 'skyblue', 'limegreen')) # 색상 지정.

library(RColorBrewer)
display.brewer.all()

ggplot(mpg) + geom_point(aes(displ, hwy, color=drv)) + 
  scale_color_brewer(palette = 'Dark2')

ggplot(mpg) + geom_point(aes(displ, hwy, shape=drv)) # 범주형 모양 표현.
ggplot(mpg) + geom_point(aes(displ, hwy, shape=drv)) +
  scale_shape_manual(values = c(1,2,3))


# 막대그래프
ggplot(mpg, aes(class)) + geom_bar()  
ggplot(mpg, aes(class)) + geom_bar() +
  scale_x_discrete(guide = guide_axis(angle = 90))
ggplot(mpg, aes(class, fill=class)) + geom_bar() 
ggplot(mpg, aes(class, fill=class)) + geom_bar() +
  scale_fill_manual(values = c('tomato', 'skyblue', 'limegreen', 
                              'orange', 'forestgreen', 'plum', 'mediumpurple'))
ggplot(mpg, aes(class, fill=class)) + geom_bar() +
  scale_fill_brewer(palette = 'Set3')

#
ggplot(mpg, aes(drv, hwy)) + geom_boxplot() +
  scale_x_discrete(labels=c('f'='Front', 'r'='Rear', '4'='4 wheels')) # 눈금 이름 바꾸기.
ggplot(mpg, aes(drv, hwy)) + geom_boxplot() + xlim('f', 'r', '4') # 눈금 순서 설정.
ggplot(mpg, aes(drv, hwy)) + geom_boxplot() + xlim('f', 'r')

# 그래프 저장.
gg <- ggplot(mpg, aes(displ, hwy)) + geom_point(alpha=0.3) 
getwd()
ggsave('Scatterplot.png', gg, width=5, height=5, units='cm') # pdf, jpeg, tiff