setwd('/Users/hj/Downloads/rpy') 

# 분산 분석: 각 그룹별 평균이 다른지 검정.

# 1. 일원 분산분석: 반응값에 영향을 미치는 실험요인이 1개.
cotton <- read.csv('cotton.csv', header=T)
head(cotton)
cotton$pct <- as.factor(cotton$pct)
fit <- aov(tensile~pct, data=cotton)
summary(fit)
tukey.test <- TukeyHSD(fit)
tukey.test
plot(tukey.test)

#
fit <- lm(tensile~pct, data=cotton)
summary(fit)
anova(fit)

# 2. 이원 분산분석: 반응값에 영향을 미치는 실험요인이 2개.
glass <- read.csv('glass.csv', header=T)
head(glass)

library(car)
glass$temperature <- recode(glass$temperature, '100=1; 125=2; 150=3')
head(glass)

class(glass$glass)
glass$glass <- factor(glass$glass)
glass$temperature <- factor(glass$temperature)
class(glass$glass)

fit <- aov(strength~glass+temperature+glass:temperature, data=glass)
#fit <- aov(strength~glass*temperature, data=glass) # 위와 동일
summary(fit)

# 
mg <- aggregate(glass$strength, 
                list(glass=glass$glass, 
                     temp=glass$temperature), 
                mean)
colnames(mg) <- c('glass', 'temp', 'mstr')
mg

#교호작용
interaction.plot(mg$glass, mg$temp, mg$mstr, type='b', col=c(1:3), leg.bty='o', leg.bg='beige', lwd=2, pch=c(18,24,22), xlab='glass', ylab='strength of glass', main='Interaction Plot')

#dev.new()
#dev.off()