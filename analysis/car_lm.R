lm.model <- lm(as.numeric(Species)~., iris)
summary(lm.model)

car::avPlots(lm.model)
