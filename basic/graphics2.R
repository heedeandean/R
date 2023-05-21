setwd('/Users/hj/git/R/basic/data/')

library(magrittr)
mtcars %>% head

install.packages('corrr')
library(corrr)
mtcars %>% correlate() %>% fashion()
mtcars %>% correlate() %>% rplot() # 빨강 : 음의 상관관계 / 파랑 : 양의 상관관계 / 색이 진할수록 강한 상관관계

mtcars %>% correlate() %>% network_plot(min_cor=.3) # 상관도 네트워크 그림(Correlation Network Plot)

install.packages('Hmisc')
library(Hmisc)
library(MASS)
temp <- Cars93[,c('Price', 'MPG.city', 'Horsepower', 'RPM', 'Length', 'Wheelbase')]
head(temp)

# 변수 군집 그림(Variable Clustering Plot)
library(jpeg)
jpeg(file='test.jpg', width=6, height=6, units='in', res=600, bg='white') # jpg로 저장
pdf(file='test.pdf', width=6, height=6, bg='white', paper='special') # pdf로 저장

plot(v <- varclus(~., data=temp, similarity='spear'))
sales.amount <- c(1.5, 2.3, 5.4, 7.5, 9, 8)
img <- readJPEG('car.jpg')
plot(c(0.5, 6.5), c(0,10), axes=F, cex.lab=1.3, type='n', xlab='Months', ylab='Sales (in millian dollars)')
axis(1, at=c(1,2,3,4,5,6), labels=c('January', 'Feburary', 'March', 'April', 'May', 'June'), cex.axis=1.2)
axis(2, at=c(0,2,4,6,8,10), labels=c('0', '2', '4', '6', '8', '10'), cex.axis=1.2)

lines(1:6, sales.amount, lwd=2, col='orange')
for(jj in 1:6) {
  rasterImage(img, jj-0.3, sales.amount[jj]-0.3, jj+0.3, sales.amount[jj]+0.3)
}
dev.off()

install.packages('png')
library(png)
img <- readPNG('car2.png')
plot(c(1, 6), c(15,46), axes=F, cex.lab=1.3, type='n', xlab='Engine Size', ylab='MPG in City')
rasterImage(img, 0.7, 14.5, 6.1, 46.0)
axis(1, at=c(1,2,3,4,5,6), labels=c('1', '2', '3', '4', '5', '6'), cex.axis=1.2)
axis(2, at=seq(15,45,by=5), labels=seq(15,45,by=5), cex.axis=1.2)
with(subset(Cars93, Origin=='non-USA'), points(EngineSize, MPG.city, col=2, pch=16))
with(subset(Cars93, Origin=='USA'), points(EngineSize, MPG.city, col=4, pch=16))
legend('topright', bty='n', c('non-USA', 'USA'), col=c(2,4), lwd=2, pch=16)
text(2.3, 35, pos=4, 'Cars in USA have low MPGs in City, \nwhile having large engines compared \n to non-USA.', col=1)

library(ggplot2)
qplot(Horsepower, Price, data=Cars93, colour=AirBags, size=AirBags)

Cars93$manual <- with(Cars93, ifelse(Man.trans.avail=='No', 'Manual_Trans_No', 'Manual_Trans_Yes'))
with(Cars93, qplot(Horsepower, Price, data=Cars93, facets=manual~Origin))

qplot(Fuel.tank.capacity, data=Cars93, geom='density', fill=Origin, alpha=I(.2), main='Fuel tank capacity by Origin', xlab='Fuel tank capacity (US gallons)', ylab='Density') +
  theme(plot.title=element_text(hjust=0.5))

ggplot(Cars93, aes(x=Horsepower, y=Price)) + 
  geom_point(shape=16)+geom_smooth(method=lm)

ggplot(Cars93, aes(x=Horsepower, y=Price)) +
  geom_point(shape=16) +
  geom_smooth(method=lm, se=FALSE) # 95% confidence region 제외

qplot(Horsepower, Price, data=Cars93, geom=c('point', 'smooth'), color=Origin, main='Price vs Horsepower by Origin', xlab='Horsepower', ylab='Price') +
  theme(plot.title=element_text(hjust=0.5))

ggplot(Cars93, aes(x=Horsepower, y=Price, color=Width)) +
  geom_point(shape=16) +
  scale_color_gradient(low='yellow', high='red')

ggplot(Cars93, aes(x=Horsepower, y=Price, color=Width)) +
  geom_point(shape=16) +
  scale_color_gradientn(colours=rainbow(5))

library(RColorBrewer)
brewer.pal.info
ggplot(Cars93, aes(x=Horsepower, y=Price, shape=AirBags, color=AirBags)) +
  geom_point(size=3) +
  scale_shape_manual(values=c(16,17,18)) +
  scale_color_brewer(palette='Dark2')

ggplot(Cars93, aes(x=Horsepower, y=Price, shape=AirBags, color=AirBags)) +
  geom_point(size=3) +
  scale_shape_manual(values=c(16,17,18)) +
  scale_color_brewer(palette='Dark2') + 
  geom_smooth(method=lm, se=FALSE)

ggplot(Cars93, aes(x=Horsepower, y=Price, shape=AirBags, color=AirBags)) +
  geom_point(size=3) +
  scale_shape_manual(values=c(16,17,18)) +
  scale_color_brewer(palette='Dark2') + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

install.packages('doBy')
library(doBy)
mean_by_Type2 <- summaryBy(MPG.highway+RPM+Horsepower+Weight+Length+Price~Type, data=Cars93, FUN=c(mean))
mean_by_Type2

df2 <- mean_by_Type2[,c(2:7)]
df_radarchart <- function(df) {
  df <- data.frame(df)
  dfmax <- apply(df, 2, max)
  dfmin <- apply(df, 2, min)
  as.data.frame(rbind(dfmax, dfmin, df))
}
mean_by_Type <- df_radarchart(df2)
row.names(mean_by_Type) <- c('max', 'min', names(table(Cars93$Type)))
mean_by_Type

install.packages('fmsb')
library(fmsb)

# 육각 레이터 차트
radarchart(df=mean_by_Type,
           seg=6, # segment 수
           pty=16, # 폰트 타입
           pcol=1:6, # 선 색깔
           plty=1,
           plwd=2, # 선 굵기
           title=c('Radar chart by Car Types')
          )

# 오각 레이터 차트
dat <- Cars93[2:6, c('Price', 'Horsepower', 'Turn.circle', 'Rear.seat.room', 'Luggage.room')]
head(dat)
datmax <- apply(dat, 2, max)
datmin <- apply(dat, 2, min)

dat <- rbind(datmax, datmin, dat)
radarchart(dat, seg=5, plty=1, vlabels=c('Price', 'Horsepower', 'U-turn space\n(feet)', 'Rear seat room\n(inches)', 'Luggage capacity\n(cubic feet)'), titile='5 segments, with specified vlabels', vlcex=0.8, pcol=rainbow(5))
legend('topleft', legend=Cars93[2:6, 'Make'], col=rainbow(5), lty=1, lwd=1)

summary(diamonds)
head(diamonds)
ggplot(diamonds, aes(carat, price)) +
  stat_bin2d(bins = 25, colour='grey50')

ggplot(diamonds, aes(carat, price)) +
  stat_bin2d(bins = 40, colour='grey50') +
  scale_x_continuous(limits = c(0,6))

ggplot(diamonds, aes(carat, price)) +
  stat_bin2d(bins = 40, colour='grey50') +
  scale_x_continuous(limits = c(0,6)) +
  ggtitle('Price vs Carat') +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_fill_gradientn(colours = c('yellow', 'green', 'blue', 'red')) +
  labs(x='Carat', y='Price')

install.packages('SwissAir')
library(SwissAir)
dim(AirQual)
head(AirQual)
with(AirQual, plot(ad.WS~ad.O3, xlab='O3', ylab='WS'))
with(AirQual, smoothScatter(ad.WS~ad.O3, main='Scatter plot by smoothed densities', xlab='O3', ylab='WS'))

install.packages('hexbin')
library(hexbin)
# 육면 상자그림
with(AirQual, plot(hexbin(ad.O3, ad.WS, xbins=100), main='Hexagonal binning(bins=100)', xlab='O3', ylab='WS'))

with(AirQual, plot(hexbin(ad.O3, ad.WS, xbins=30), main='Hexagonal binning(bins=30)', xlab='O3', ylab='WS'))

install.packages('IDPmisc')
library(IDPmisc)
# 이미지 산점도
with(AirQual, iplot(ad.O3, ad.WS, xlab='O3', ylab='WS', main='Image Scatter Plot with \n Color Indicating Density'))

ipairs(subset(AirQual, select=c(ad.O3, ad.WS, ad.WD)))

library(igraph)
#dev.off()
split.screen(figs=c(1,2))
screen(1)
g1 <- graph(edges = c(1,2, 2,3, 3,1), n=3, directed = F)
plot(g1)
screen(2)
g1 <- graph(edges = c(1,2, 2,3, 3,1), n=3, directed = T)
plot(g1)

split.screen(figs=c(1,2))
screen(1)
g1 <- graph(edges = c(1,2, 2,3, 3,1, 1,3), n=3)
plot(g1, edge.arrow.size=0.5)
screen(2)
g2 <- graph(edges = c(1,2, 2,3, 3,1), n=7)
plot(g2, edge.arrow.size=0.5)
g3 <- graph(c('S', 'B', 'B', 'G', 'G', 'S'))
plot(g3)
g4 <- graph(c('S', 'B', 'B', 'G', 'G', 'S', 'S', 'D', 'S', 'D'), isolates = c('S', 'U'))
plot(g4, edge.arrow.sizw=1.5, vertex.color='gold', vertex.size=15, vertex.frame.color='gray', vertex.label.color='black', vertex.label.cex=1.2, vertex.label.dist=2, edge.curved=0.2)