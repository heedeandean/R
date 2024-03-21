# 빈도론적 확률
# 시행횟수가 많아지면 상대도수 값이 일정해짐. (상대도수 극한값) 
library(ggplot2); library(magrittr)
RDice <- function(nn) {sample(1:6, nn, replace = T) %>% table()/nn}
set.seed(1234567)
dice_1 <- RDice(12)
dice_2 <- RDice(120)
dice_3 <- RDice(1200)
dice_4 <- RDice(12000)

dice <- c(as.numeric(dice_1),
          as.numeric(dice_2),
          as.numeric(dice_3),
          as.numeric(dice_4))
dice

nn <- c(rep("(a) n=12", 6),
        rep("(b) n=120", 6),
        rep("(c) n=1200", 6),
        rep("(d) n=12000", 6))

num <- c(rep(1:6, 4))
num

dice_result <- data.frame(nn, num, dice)
head(dice_result) 

ggplot(dice_result, aes(num, dice)) +
  geom_bar(stat = 'identity', alpha = 0.8) +
  xlab('\n Result on Die') + ylab('Relative Frequency\n') +
  geom_hline(yintercept=1/6, colour=2, lty=2) +
  ylim(0, 0.3) + facet_wrap(~nn, ncol=2)