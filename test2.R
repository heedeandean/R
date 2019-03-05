options(encoding='utf-8')
data = read.csv("data/성적.csv")

# 1)
# data$group 컬럼에 A조~C조 랜덤으로 160명씩 고르게 분포시키시오.

data$group = sample(c('A', 'B', 'C'), size = nrow(data), replace = T, prob = c(160, 160, 160))
table(data$group) # 데이터 검증.


# 2)
# fibonacci.R 파일을 작성하고 console에서 실행하시오.

source('data/fibonacci.R')


# 3)
# apply를 이용하여 smdt에 과목별 평균점수 행을 추가하고, 

smdt = data.frame(stuno = 1:5, 
                  Korean=sample(60:100, 5),
                  English=sample((5:10) * 10, 5),
                  Math=sample(50:100, 5))

smdt_avg = apply(smdt[, 2:4], MARGIN = 2, FUN = mean)
smdt_avg

smdt
rbind(smdt[, 2:4], smdt_avg)

# 4)
# 총점과 평균 변수(컬럼)을 추가하시오.
# stuno Korean English Math total avg
# 1     1     81      70   71   222  74
# 2     2     82      50   95   227  76
# 3     3    100      60   76   236  79
# 4     4     77     100   54   231  77
# 5     5     78      80   86   244  81
# 6    계     84      72   76   232  77

smdt$total = apply(smdt[, 2:4], MARGIN = 1, FUN = sum)
smdt$avg = apply(smdt[, 2:4], MARGIN = 1, FUN = mean)
smdt_t = apply(smdt[, 2:6], MARGIN = 2, FUN = mean)
smdt_t
rbind(smdt[, 2:6], smdt_t)
