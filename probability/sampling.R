library(gtools)
x <- c('a', 'b', 'c')

## 표본공간에서 표본 추출법. (ex. 표본공간=3, 표본=2)
# 1. 순열: 순서O 
## 1-1. 중복 순열(permutation with repetition): 복원 추출 
rr1 <- permutations(n=3, r=2, v=x, repeats.allowed = T)
rr1
nrow(rr1)

## 1-2. 비복원 추출(걍 순열..)
rr2 <- permutations(n=3, r=2, v=x, repeats.allowed = F)
rr2
nrow(rr2)

# 2. 조합: 순서X, 비복원 추출
rr3 <- combinations(n=3, r=2, v=x, repeats.allowed = F)
rr3
nrow(rr3)