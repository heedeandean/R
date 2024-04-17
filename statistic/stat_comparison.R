## 1. 두 모평균 비교
pre <- c(72,80,83,63,66,76,82)
post <- c(78,82,82,68,70,75,88)
exam1 <- data.frame(pre, post)
exam1

# t검정
t.test(exam1$pre, exam1$post, 
       mu=0, alternative='less', # 대립가설: 두 모평균의 차가 0보다 작다.
       paired=T) # 대응표본

####
# 2. 분산분석(반응값~요인)
# 2-1. 일원배치법 
x <- c(84,83,82,85,89,86,93,94,96,89,89,87)
A <- c(rep(1,3), rep(2,3),rep(3,3),rep(4,3))
A
A <- factor(A) # 명목적O, 수치적X
A
aovdat1 <- data.frame(x, A)
aovmodel1 <- aov(x~A, data=aovdat1) 
summary(aovmodel1)

# 2-2. 일원배치법 
y <- c(97.8, 97.5, 96.9, 98.5, 98.8, 97.1, 99.2, 98.4, 98.1, 98.2, 97.5, 96.8)
surface <- c(rep(1,3), rep(2,3),rep(3,3),rep(4,3))
manu <- rep(c(1,2,3), 4)
manu
surface <- factor(surface)
manu <- factor(manu)

aovdat2 <- data.frame(surface, manu)
aovmodel2 <- aov(y~surface+manu, data=aovdat2) 
summary(aovmodel2)

####
# 3. 범주형 데이터
# 3-1. 독립성 검정
dept <- c(rep("Stat",50),rep("DS",25))
regi <- c(rep("Y",20),rep("N",30),rep("Y",13),rep("N",12))
deptregi <- data.frame(dept, regi)
rtable <- xtabs(~dept+regi, data=deptregi) # 분할표
rtable

# x^2 검정
ctest <- chisq.test(rtable, 
                    correct=F) # 연속성 수정: 이산형, 연속형과 비교할때 보정. 
ctest

# 3-2. 적합도 검정
catnum <- c(0:3)
obs <-c(33, 15, 9, 3)
m <- sum(catnum*obs)/sum(obs)
pprob <- round(dpois(catnum, m), 3)
pprob
pprob[4] <- 1-sum(pprob[1:3])
pprob
pprob*60

# 범주 < 5 => 범주 병합
obs1 <-c(33, 15, 12)
pprob1 <- pprob[1:3]
pprob1
pprob1[3] <- 1-sum(pprob[1:2])
pprob1
ctest1 <- chisq.test(obs1, p=pprob1)
ctest1$statistic > qchisq(0.95, 1) # 자유도=3(범주개수)-1-1(추정모수개수)=1 / 검정통계량 vs x^2 / F: 귀무가설 채택