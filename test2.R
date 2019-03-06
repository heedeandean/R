options(encoding='utf-8')
data = read.csv("data/성적.csv")

# 1)
# data$group 컬럼에 A조~C조 랜덤으로 160명씩 고르게 분포시키시오.

data$group = sample(rep(LETTERS[1:3], times=1, length.out=480), size = nrow(data), replace = F)
table(data$group) # 데이터 검증.


# 2)
# fibonacci.R 파일을 작성하고 console에서 실행하시오.

#(shift + click)
# https://github.com/heedeandean/R/blob/master/data/fibonacci.R

source('data/fibonacci.R')


# 3)
# apply를 이용하여 smdt에 과목별 (총)평균점수 행을 추가하고, 
# 총점과 평균 변수(컬럼)을 추가하시오.

# > smdt
#   stuno Korean English Math total avg
# 1     1     81      70   71   222  74
# 2     2     82      50   95   227  76
# ...
# 6    계     84      72   76   232  77

# 임의의 smdt 데이터 생성.

smdt = data.frame(stuno = 1:5, 
                  Korean = sample(60:100, 5),
                  English = sample((5:10) * 10, 5),
                  Math = sample(50:100, 5))

# 과목별 (총)평균점수 행 추가.

col_mean = round(apply(smdt, MARGIN = 2, FUN = mean))

smdt = rbind(smdt, col_mean)
smdt
smdt[6, 1] = '계'

smdt
str(smdt)

# total, avg 컬럼 추가.

smdt$total = apply(smdt[, 2:4], MARGIN = 1, FUN = sum)
smdt$avg = round(apply(smdt[, 2:4], MARGIN = 1, FUN = mean))
smdt

# 데이터 검증.

smdt$Korean + smdt$English + smdt$Math
smdt$total

round(smdt$total / 3)
smdt$avg


# 4)
# 2016~2019년 연도별 1월(Jan) ~ 12월(Dec) 매출액 데이터를
# `no year Jan Feb … Dec` 형태로 만든 다음, 아래와 같이 출력하시오.

#     year month saleamt
# 1   2016   Jan   49900
# 2   2017   Jan   48300
# 3   2018   Jan   70200

library('reshape2')

df_year = cbind(data.frame(no=1:4, year=2016:2019),
                  matrix(round(runif(16), 3) * 100000, ncol=12, dimnames = list(NULL, month.abb))) 
df_year

melt_year = melt(data=df_year[,2:14], id.vars = "year", variable.name = 'month', value.name = 'saleamt')
melt_year



