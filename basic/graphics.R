setwd('/Users/hj/git/R/basic')

library(MASS)
head(Cars93)

###############################
## 단변수 범주형 

# 막대 그림
(tab <- with(Cars93, table(Type))) # 빈도표
tab
barplot(tab, main='Type of Car', xlab='Type', ylab='Number of Car', col=1:6, legend=c('Compact', 'Large', 'Midsize', 'Small', 'Sporty', 'Van'), names.arg=c('Compact', 'Large', 'Midsize', 'Small', 'Sporty', 'Van'))

# 막대 그림(side)
tab <- with(Cars93, xtabs(~Type+AirBags)) # 교차표
tab
barplot(tab, col=rainbow(6), legend=c('Compact', 'Large', 'Midsize', 'Small', 'Sporty', 'Van'), xlab='AirBags', ylab='Number of Cars', beside=TRUE)

# 막대 그림(stacked)
barplot(tab, col=rainbow(6), legend=c('Compact', 'Large', 'Midsize', 'Small', 'Sporty', 'Van'), xlab='AirBags', ylab='Number of Cars', beside=FALSE, xlim=c(0, ncol(tab)+2), args.legend=list(x=ncol(tab)+2, y=max(colSums(tab)), bty='n'))

# pie chart
tab <- with(Cars93, table(Type)) # 빈도표
pie(tab, col=topo.colors(6))

names(tab) <- c('COMPACT', 'LARGE', 'MIDSIZE', 'SMALL', 'SPORTY', 'VAN')
pie(tab, col=topo.colors(6))

###############################
## 단변수 연속형

# histogram
with(Cars93, hist(MPG.highway, xlab='MPG in Highway', main='MPG in Highway'))

# 확률밀도함수 그림(density plot)
install.packages('vcd')
library(vcd)
summary(Arthritis)
head(Arthritis)

with(Arthritis, plot(density(Age)))

# 확률밀도함수 그림(density plot) - overlay
with(Cars93, hist(MPG.highway, xlab='MPG in Highway', main='MPG in Highway', probability=T))
with(Cars93, lines(density(MPG.highway), col='red', lwd=2))

colors()

# Q-Q(Quantile-Quantile) 그림
with(Cars93, qqnorm(Turn.circle, main='Q-Q plot of Turn.circle(U-turn space, feet)'))
with(Cars93, qqline(Turn.circle, col='orange', lwd=2)) # 45도 직선 추가

###############################
# 상자 그림(box plot) : 단변수 (연속형 ~ 범주형)
boxplot(Min.Price~AirBags, data=Cars93)[] 
boxplot(Min.Price~AirBags, at=c(3,2,1), data=Cars93, names=c('Driver & Passenger','Driver only', 'None'), col=c('orange', 'cyan', 'yellow', ylab='Minimum Price', xlab='Airbag', ylim=c(0,50), boxwex=0.25))

summary(subset(Cars93, AirBags=='Driver only')$Min.Price)

install.packages('ggplot2')
library(ggplot2)
qplot(AirBags, Min.Price, data=Cars93, geom=c('boxplot', 'jitter'), fill=AirBags, ylab='Minimum Price', xlab='AirBags', alpha=I(.2))

p <- ggplot(Cars93, aes(x=AirBags, y=Min.Price)) +
  geom_boxplot(aes(fill=AirBags)) +
  scale_fill_viridis_d()
p
p + 
  theme(legend.position='none') +
  xlab('Airbags') +
  ylab('Minimum Price')

# pirate 그림  
install.packages('yarrr')
library(yarrr)
pirateplot(formula=Price~AirBags, point.o=0.1, data=Cars93, main='Price by AirBag type', inf.method='iqr')

# pirate 그림 : 2개의 범주형(명목형) 변수 ⭐️⭐️⭐️⭐️⭐
pirateplot(formula=MPG.city~Origin+DriveTrain, point.o=0.5, data=Cars93, main='City MPG by Origin and Drive Train', inf.method='iqr')

?ggplot
# 그룹별 확률밀도함수 그림
p1 <- ggplot(Cars93, aes(x=MPG.highway)) +
  theme_bw() + # 흰색 배경
  geom_density(aes(group=Type, colour=Type)) +
  labs(x='MPG.highway', y='Density') +
  ggtitle('Density of MPG in Highway by Type') +
  theme(plot.title=element_text(hjust=0.5)) # 타이틀 정중앙 위치
p1

p2 <- ggplot(Cars93, aes(x=MPG.highway)) +
  theme_bw() + # 흰색 배경
  geom_density(aes(group=Origin, colour=Origin)) +
  labs(x='MPG.highway', y='Density') +
  ggtitle('Density of MPG in Highway by Origin') +
  theme(plot.title=element_text(hjust=0.5)) # 타이틀 정중앙 위치
p2

install.packages('gridExtra')
library(gridExtra)
grid.arrange(p1, p2, ncol=2)

# 호흡곡선(spinogram) : 세로 높이 1
library(vcd)
spine(Improved~Age, data=Arthritis, breaks=3) # 범주형~연속형 변수 / 나이 변수 3구간으로 나눔

with(Arthritis, spine(Improved~Age, breaks=quantile(Age))) # 나이 변수 4등분

spine(Improved~Age, data=Arthritis, breaks='Scott') # 단위 구간 자동 설정(자료가 많은 경우(10)/적은 경우(5))

# 조건부 밀도함수 그림(conditional density plot)
cdplot(Improved~Age, data=Arthritis)
with(Arthritis, rug(jitter(Age), col='white', quiet=TRUE)) # 경고 메시지 출력 X

###############################
?pch

## 연속형 이변수(bivariate)
# 산점도 : 두 변수 간의 관계 표현 / 모두 연속형
with(Cars93, plot(Price, MPG.city, main='Price vs MPG.city', xlab='Price', ylab='MPG in City', pch=19)) # pch(plot character)
with(Cars93, abline(lm(MPG.city~Price), col='red', lwd=2)) # 회귀 직선(regression line)
with(Cars93, lines(lowess(Price, MPG.city), col='blue', lwd=2)) # 직선
legend(40, 40, lty=1, col=c('red', 'blue'), c('regression', 'lowess'), lwd=2, bty='n') # 좌표(40, 40) 위치 / lty(line type) / 박스 생략

###############################
## 다변수

# 모자이크 그림(mosaic plot) : 모두 범주형
library(vcd)
summary(Arthritis)
art <- xtabs(~Treatment+Improved, data=Arthritis, subset=Sex=='Female') # 교차표
art
mosaic(art, gp=shading_max)

mosaic(~Sex+Age+Survived, data=Titanic, main='Survival on the Titanic', shade=TRUE, legend=TRUE)

# 다중 산점도 
dat1 <- subset(Cars93, select=c(Min.Price, Price, Max.Price, MPG.city, MPG.highway))
head(dat1)
pairs(dat1)

# 단순 산점도
with(Cars93, plot(Price, MPG.city, xlab='Price', ylab='MPG in City', main='Mileage'))

with(Cars93, plot(Price, MPG.city, xlab='Price', ylab='MPG in City', type='n'))

with(subset(Cars93, DriveTrain=='Front'), points(Price, MPG.city, col='orange', pch=19))
with(subset(Cars93, DriveTrain=='Rear'), points(Price, MPG.city, col='firebrick', pch=17))
with(subset(Cars93, DriveTrain=='4WD'), points(Price, MPG.city, col='black', pch=8))

legend('topright', legend=c('Front', 'Rear', '4WD'), col=c('orange', 'firebrick', 'black'), pch=c(19,17,8), bty='n')

# 단순 산점도 + 회귀선
fit1 <- with(subset(Cars93, DriveTrain=='Front'), lm(MPG.city~Price))
fit1
fit2 <- with(subset(Cars93, DriveTrain=='Rear'), lm(MPG.city~Price))
fit3 <- with(subset(Cars93, DriveTrain=='4WD'), lm(MPG.city~Price))

xx1 <- subset(Cars93, DriveTrain=='Front')$Price
xx1
yy1 <- fit1$coef[1]+fit1$coef[2]*xx1 # coef : 회귀 계수
yy1

xx2 <- subset(Cars93, DriveTrain=='Rear')$Price
yy2 <- fit2$coef[1]+fit2$coef[2]*xx2

xx3 <- subset(Cars93, DriveTrain=='4WD')$Price
yy3 <- fit3$coef[1]+fit3$coef[2]*xx3

with(Cars93, plot(Price, MPG.city, xlab='Price', ylab='MPG in City', type='n'))
with(subset(Cars93, DriveTrain=='Front'), points(Price, MPG.city, col='orange', pch=19))
with(subset(Cars93, DriveTrain=='Rear'), points(Price, MPG.city, col='firebrick', pch=17))
with(subset(Cars93, DriveTrain=='4WD'), points(Price, MPG.city, col='black', pch=8))
legend('topright', legend=c('Front', 'Rear', '4WD'), col=c('orange', 'firebrick', 'black'), pch=c(19,17,8), bty='n')

lines(xx1, yy1, col='orange', lwd=2)
lines(xx2, yy2, col='firebrick', lwd=2)
lines(xx3, yy3, col='black', lwd=2)

# 그룹별 산점도
library(ggplot2)
qplot(Wheelbase, Width, data=Cars93, shape=Type, color=Type, facets=Origin~AirBags, size=I(2), xlab='Whellbase', ylab='Car Width')

# 나무지도 그림
install.packages('treemap')
library(treemap)
data(GNI2014)
head(GNI2014)
treemap(GNI2014, index=c('continent', 'iso3'), vSize='population', vColor='GNI', type='value') # vColor=연속형 변수 

treemap(Cars93, index=c('Manufacturer', 'Make'), vSize='Price', vColor='AirBags', type='categorical') # vColor=범주형 변수

# 풍선 그림(ballon plot) : 명목형 변수의 교차표를 그림으로 표현
install.packages('gplots')
library(gplots)
dt <- with(Cars93, xtabs(~AirBags+Type)) # 교차표
dt

balloonplot(dt, main='Airbags by Car type', xlab='', ylab='', label=FALSE, show.margins=FALSE)
balloonplot(dt, main='Airbags by Car type', xlab='', ylab='', label=TRUE, show.margins=TRUE) # label 값 표시

# 두변수 모자이크 그림
library(graphics) # base 패키지
mosaicplot(dt, color=TRUE, las=1, main='Airbags by Car type') # las=1 가로형 <-> las=2 세로형

# 다변수 모자이크 그림
mosaicplot(~DriveTrain+AirBags+Origin, las=1, main='Drive Train by Airbags and Origin', ylab='Airbag type', xlab='Drive Train', data=Cars93, color=TRUE)

with(Cars93, xtabs(~DriveTrain+AirBags+Origin))