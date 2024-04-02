# 1. 모평균 
score = c( 88, 83, 83, 85, 94, 88, 91, 96, 
           89, 83, 81, 80, 84, 89, 83, 79)
bar_x <- mean(score)
s <- sd(score)
n <- length(score) # 표본수

# 95% 신뢰구간
qt(0.975, 15) # (1-0.05/2, 자유도(n-1))
qt(0.025, 15, lower.tail = FALSE) 

qt_95 <- qt(0.975, 15) 
c(bar_x - qt_95*s/sqrt(n), bar_x + qt_95*s/sqrt(n))
t.test(score)$conf.int

# 99% 신뢰구간
qt_99 <- qt(0.995, 15) # (1-0.01/2, 자유도(n-1))
c(bar_x - qt_99*s/sqrt(n), bar_x + qt_99*s/sqrt(n))
t.test(score, conf.level = 0.99)$conf.int

# 2. 모비율
n <- 500; X <- 200
p_hat <- X/n
alpha <- 0.05
z_1 <- qnorm(1-alpha/2)
c(p_hat-z_1*sqrt(p_hat*(1-p_hat)/n), p_hat+z_1*sqrt(p_hat*(1-p_hat)/n))
prop.test(X, n)$conf.int

# 3. 모분산
s2 <- 4^2
n <- 40
q_1 <- qchisq(1-alpha/2, n-1)
q_2 <- qchisq(alpha/2, n-1)
c((n-1)*s2/q_1, (n-1)*s2/q_2)