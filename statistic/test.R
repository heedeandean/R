# 1. 모평균 검정
# ex1.
n <- 10; s <- 0.2; bar_x <- 12.2
alpha <- 0.05
ttest <- (bar_x-12)/(0.2/sqrt(10)) # 검정통계량
ttest_cr <- qt(1-alpha, n-1) # 기각역
ttest_pv <- (1-pt(ttest, n-1)) # 유의확률
cat(ttest, ttest_cr, ttest_pv)

# ex2.
book <- c(5, 23, 20, 1, 10, 15, 15, 10, 9, 13, 18, 11, 18, 20, 19, 19)
t.test(book, mu=11, # 귀무가설
       alternative='greater') # 대립가설: 단측검정

# 2. 모비율 검정
p0 <- 0.6; n <- 50; hat_p <- 0.7 # 비율; 표본수; 유의수준
ptest <- (hat_p-p0)/sqrt(p0*(1-p0)/n) # 검정통계량
ptest_cr <- qnorm(1-alpha) # 기각역
ptest_pv <- 1-pnorm(ptest) # 유의확률
cat(ptest, ptest_cr, ptest_pv)

# 3. 모분산 검정
n <- 12
book <- c(5, 23, 20, 1, 10, 15, 15, 10, 9, 13, 18, 11)
vtest <- var(book)*(12-1)/4^2 # 검정통계량
vtest_cr <- qchisq(1-alpha, n-1) # 기각역
vtest_pv <- 1-pchisq(vtest, n-1) # 유의확률
cat(vtest, vtest_cr, vtest_pv)