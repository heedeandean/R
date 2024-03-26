# 베이즈 정리
install.packages('LaplacesDemon')
library(LaplacesDemon)

# 진단 키트 결과에 따른 질병 여부 계산.
D <- 0.05 # 질병 확률 
Dc <- 1-D

# 진단 키트가 맞을 확률
Tp_D <- 0.98 # 양성
Tm_Dc <- 0.90 # 음성

# 진단 키트가 틀릴 확률
Tm_D <- 1-Tp_D  # 음성
Tp_Dc <- 1-Tm_Dc # 양성

Dp <- c(D, Dc)

# 찐 양성
D_Tp <- c(Tp_D, Tp_Dc)

BayesTheorem(Dp, D_Tp)[1] 
Tp_D*D/(Tp_D*D+Tp_Dc*Dc) # 직접 계산

# 찐 음성
Dc_Tm <- c(Tm_D, Tm_Dc)
BayesTheorem(Dp, Dc_Tm)[2] 
Tm_Dc*Dc/(Tm_D*D+Tm_Dc*Dc) # 직접 계산
