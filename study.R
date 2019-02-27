# 문제1. 1,2,3,4를 입력하면 해당 혈액형(A, B, O, AB)을 factor로 출력하는 함수를 작성하시오. 
f1 = function(n=1) {
  f = factor(n, levels=1:4, labels=c('A', 'B', 'O', 'AB'))
  as.vector(f)
}
f1(4)

# 문제2. 1차원 vector와 값을 입력받아 vector에 값을 추가하는 append 함수를 작성하시오.
f2 = function(v, val) {
  v[length(v) + 1] = val
  v
}
v1 = 1:3
v1 = f2(v1, 9)
v1

# 문제3. 숫자로 된 10x20 matrix를 정의하고,
# 열 이름을 알파벳 대문자순으로, 행 이름을 소문자로 변경하시오.
# (단, 10번째, 20번째 컬럼명은 알파벳 뒤에 10, 20으로 하시오)
# FYI: > paste('A', 10, sep='')
#      > paste0('A', 10)
m = matrix(1:200, ncol = 20, byrow = TRUE)
colnames(m) = LETTERS[1:ncol(m)]
rownames(m) = letters[1:nrow(m)]

colnames(m)[10] = c(paste(colnames(m)[10], 10, sep=""))
colnames(m)[20] = c(paste(colnames(m)[20], 20, sep=""))

colnames(m[, c(10, 20)])

