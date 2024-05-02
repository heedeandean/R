setwd('/Users/hj/Downloads/visualization2023-main/CH8/')

# 한글 설정
par(family = "Apple SD Gothic Neo") 
theme_set(theme_gray(base_family = "Apple SD Gothic Neo"))

library(readxl)
library(ggplot2)

gdp <- read_excel('GDP.xlsx')
gdp

# ⏰ 시계열 데이터 - x축: 시간 

# 1. 시계열 객체 
# 1-1. ts: 일정한 간격으로 측정
gdp_ts <- gdp[,2:3] %>% ts(start=1960, frequency=4)
head(gdp_ts)
plot(gdp_ts/1000, main='', ylab='GDP(조원)', xlab='')

# 1-2. zoo (xts와 유사.)
library(zoo)
# 1-2-1.
date_q <- seq(as.Date('1960-01-01'), as.Date('2022-01-01'), 'quarter')
head(date_q)
gdp_zoo <- zoo(gdp[,2:3], date_q)
plot(gdp_zoo/1000,screens=1, col=1:2, ylab='GDP(조원)', xlab='')
legend('topleft', col=1:2, lty=1, c('원계열', '계정조정계열'), bty='n')

# 1-2-2.
autoplot(gdp_zoo/1000, facets = NULL) + theme(legend.position = 'bottom') +
  labs(x='', y='GDP(조원)') + scale_colour_hue('GDP')

# 1-3. xts
library(xts)

gdp_xts <- xts(gdp[,2:3], date_q)
head(gdp_xts)
plot(gdp_xts/1000, ylab='GDP(조원)', xlab='')

# 1-4. tsibble
library(tsibble)
#install.packages('feasts')
library(feasts)

gdp_tsibble <- gdp_ts %>% as_tsibble()
gdp_tsibble
gdp_tsibble %>% autoplot(value/1000) + 
  labs(x='', y='GDP(조원)') +
  guides(color=guide_legend(title='GDP'))

detach('package:tsibble', unload = TRUE)

####
# 2. 그래프 종류
# 2-1. 선 그래프: 경향성 파악
#install.packages('reshape2')
library(reshape2)

gdp_melt <- cbind(gdp[,2:3], date_q) %>% melt(id='date_q')
head(gdp_melt)

ggplot(gdp_melt, aes(x=date_q)) + 
  geom_line(aes(y=value/1000, colour=variable)) +
  labs(x=NULL, y='GDP(조원)') + scale_colour_hue(NULL) +
  theme(legend.position = c(0,1), legend.justification = c(0,1), legend.background = element_rect(fill = 'transparent')) 

# 2-1-1. 후방 이동평균선 (cf. 중심화 이동평균선)
# 기술적 분석: 시세흐름, 패턴을 파악하여 주식, 외환 등의 매매시점 파악.
#install.packages('quantmod')
library(quantmod)
getSymbols('^KS11', src='yahoo') # 한국 종합주가지수
head(KS11)
chartSeries(KS11, name='KOSPI', theme = chartTheme('white', up.col='red', dn.col='blue', subset='2020-01-01::2022-06')) # 캔들도표

addSMA(n=5, col='red')
addSMA(n=20, col='blue')
addSMA(n=60, col='green')
addSMA(n=120, col='black')
addRSI() # Relative Strength Index: 주가 상승폭의 상대적 위치를 방향성 및 강도로 나타냄.

#
#install.packages('COVID19')
#install.packages('TSstudio')
library(COVID19)
library(TSstudio)

cov <- covid19('KR', verbose=FALSE)
head(cov)
date_d <- seq(as.Date('2020-01-01'), as.Date('2020-12-31'), 'day')
head(date_d)
con <- xts(cov[,c('confirmed', 'tests')], order.by = cov$date)
head(con)
ts <- con[date_d, ]
head(ts)
ts_plot(diff(ts$confirmed), title='한국의 일별 확진자 수', Xtitle=NULL, Ytitle='확진자 수')

# 계절 변동: 단기적 
#install.packages('forecast', type='binary')
library(forecast)
library(tidyverse)

gdp_ts <- gdp[,2] %>% ts(start=1960, frequency=4) %>% 
  window(start=c(2010,1), end=c(2021,4))
head(gdp_ts)

ggseasonplot(gdp_ts/1000, year.labels = TRUE, xlab='분기', ylab='GDP(조원)')
ggsubseriesplot(gdp_ts/1000, xlab='분기', ylab='GDP(조원)')

# 순환변동: 장기적
library(scales)

cycle1 <- read_excel('경기종합지수.xlsx')
head(cycle1)
연도 <- seq(as.Date('1970-01-01'), as.Date('2022-04-01'), 'month')
cycle <- xts(cycle1[,3], 연도)
head(cycle)

refdate <- read.csv('refdate1.csv', header=TRUE)
head(refdate)
yrng <- range(cycle)
yrng
datebreaks <- seq(as.Date('1970-01-01'), as.Date('2023-01-01'), '5 year')
datebreaks

p <- autoplot(cycle, facets = NULL) +
  theme(panel.background = element_rect(fill = 'white', colour = 'gray'), legend.position = 'bottom') +
  geom_rect(aes(NULL, NULL, xmin = as.Date(start), xmax=as.Date(end), fill=경기순환), ymin=yrng[1], ymax=yrng[2], data=refdate) +
  scale_fill_manual(values=alpha(c('yellow', 'darkblue'), 0.1)) +
  ylab(NULL) + xlab(NULL) + 
  geom_hline(yintercept = 100, colour='gray') +
  geom_text(aes(x=as.Date(start), y=yrng[2], label=name1), data = refdate, size=2, hjust=0.5, vjust=-0.5) +
  geom_text(aes(x=as.Date(end), y=yrng[2], label=name2), data = refdate, size=2, hjust=0.5, vjust=-0.5) 

p + scale_x_date(breaks=datebreaks,labels=date_format('%Y'), expand=c(0.01,0.01))

# 제주시 강수량
climate <- read_excel('기온강수량.xlsx', sheet = '강수량')
head(climate)
연도 <- seq(as.Date('2000-01-01'), as.Date('2020-12-01'), 'month')
climate_xts <- xts(climate[,4], 연도)
head(climate_xts)
climate_xts %>% ggplot(aes(x=연도, y=제주)) +
  geom_area(colour='black', fill='blue', alpha=.2) +
  ylab('강수량(mm)')

#
climate <- read_excel('기온강수량.xlsx', sheet = '기온')
head(climate)
climate_1 <- cbind(연도, climate[,2:4])
head(climate_1)
climate_2 <- melt(climate_1, id='연도')
head(climate_2)
climate_2 %>% ggplot(aes(x=연도)) + facet_grid(variable~.) +
  geom_line(aes(y=value), colour='red') + 
  ylab(NULL) + xlab(NULL)

####
# 2-2. 막대그래프: 명확하게 구분, 양적 크기 비교.
# (cf. 막대 수가 많을 경우, 선그래프 유용.)
cb <- read_excel('경상수지.xlsx')
head(cb)
cb$경상수지 <- cb$경상수지/100
cb$pos <- ifelse(cb$경상수지>0, '흑자', '적자')
연도 <- seq(as.Date('1980-01-01'), as.Date('2021-01-01'), 'year')
cb_xts <- xts(cb, 연도)

ggplot(cb, aes(x=연도, y=경상수지, fill=pos)) +
  geom_bar(stat='identity', position='identity', colour='black', size=0.25) +
  scale_fill_manual(values=c('red', 'darkgray')) +
  ylab('경상수지(억달러)') +
  theme(panel.background = element_rect(fill='white', colour='gray'), legend.position = 'bottom') +
  guides(fill=guide_legend(title='경상수지'))


####
# 2-3. 누적그래프
library(ggstream)

# 2-3-1. 면적그래프
pop_age <- read_excel('연령별_추계인구.xlsx')
head(pop_age)
pop_age_1 <- pop_age[,1:10] %>% melt(id='시점')
head(pop_age_1)
ggplot(pop_age_1, aes(x=시점, y=value/10000, fill=variable)) +
  geom_stream(type='ridge', 
              color=1, lwd=0.25) +
  scale_fill_brewer(palette='Blues') + 
  ylab('인구(만 명)') + xlab(NULL) +
  scale_x_continuous(breaks = seq(1960, 2070, 10), expand = c(0,0)) +
  guides(fill=guide_legend(title='연령대'))
# => 우리나라 총 인구는 2020년 이후 감소하기 시작함.

ggplot(pop_age_1, aes(x=시점, y=value, fill=variable)) +
  geom_stream(type='proportional', # 비율
              color=1, lwd=0.25) +
  scale_fill_brewer(palette='Blues') + 
  ylab('인구비중') + xlab(NULL) +
  scale_x_continuous(breaks = seq(1960, 2070, 10), expand = c(0,0)) +
  guides(fill=guide_legend(title='연령대'))

# 2-3-2. 막대그래프 
gdp_s1 <- read_excel('경제활동별_GDP.xlsx')
head(gdp_s1)
gdp_s <- gdp_s1[1+1:6*10, 1:7] %>% melt(id='시점')
head(gdp_s)
names(gdp_s) <- c('연도', '산업', '비중')
head(gdp_s)
ggplot(gdp_s, aes(x=연도, y=비중, fill=산업)) +
  geom_bar(stat = 'identity', position='fill') +
  scale_x_continuous(breaks=seq(1970, 2020, 10), expand=c(0,0)) +
  theme(panel.background = element_rect(fill='white', colour='gray'), legend.position = 'bottom') +
  xlab(NULL)
# => 농림어업 감소
# => 서비스업 증가

####
# 2-4. 경로그래프: 좌표점들을 시간에 따라 연결.
# 재고출하순환도
inven1 <- read_excel('재고출하지수.xlsx')
head(inven1)
연도 <- seq(as.Date('2000-01-01'), as.Date('2022-04-01'), 'month')
inven <- xts(inven1[,4:5], 연도)
head(inven)
inven_1 <- inven[index(inven) > as.Date('2020-03-01')] 
head(inven_1)
ggplot(inven_1, aes(x=출하지수증감률, y=재고지수증감률)) +
  geom_path() + geom_point() + 
  ylim(-10,15) + xlim(-10,15) + geom_text(aes(label=substr(index(inven_1), 3, 7)), size=3, hjust=-0.2, vjust=-0.3, colour='blue') +
  geom_abline(intercept = 0, slope = 1, colour='red') +
  geom_hline(yintercept = 0, colour='gray') +
  geom_vline(xintercept = 0, colour='gray') 

####
# 2-5. 채색 달력 그래프: 일별 데이터값을 달력에 색으로 대응.
library(plyr)

getSymbols('^KS11', src='yahoo', from='2019-01-01') 
KS11$주가변동 <- abs(diff(KS11$KS11.Close)/lag.xts(KS11$KS11.Close)) * 100
dat <- data.frame(date=index(KS11), KS11)
head(dat)
dat$year <- as.numeric(as.POSIXlt(dat$date)$year+1900)
dat$year
dat$month <- as.numeric(as.POSIXlt(dat$date)$mon+1)
dat$monthf <- factor(dat$month, levels=as.character(1:12), labels=c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월"), ordered=TRUE)
dat$monthf
dat$weekday <- as.POSIXlt(dat$date)$wday
dat$weekday
dat$weekdayf <- factor(dat$weekday, levels=rev(1:7), labels = rev(c("월","화","수","목","금","토","일")), ordered = TRUE)
dat$yearmonth <- as.yearmon(dat$date)
dat$yearmonth
dat$yearmonthf <- factor(dat$yearmonth)
dat$week <- as.numeric(format(dat$date, '%W'))
dat$week
dat <- ddply(dat,.(yearmonthf), transform, monthweek=1+week-min(week))
head(dat)
ggplot(dat, aes(monthweek, weekdayf, fill=주가변동)) +
  geom_tile(colour ='white') + facet_grid(year~monthf) +
  xlab('') + ylab('') +
  scale_fill_gradient(limits=c(0,12), low='lightgray', high='darkred') +
  theme(panel.background = element_rect(fill = 'white', colour = 'gray'))