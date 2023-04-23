setwd('/Users/hj/git/R/basic')

# 전역 변수
u <- 1 
v <- 8

g <- function(x) {
  # 지역 변수
  x <- x+1 
  u <- u+x
  return(u)
}
g(v)
u
v

## 함수 생성법
# 1.
d.mean <- function(data) {
  sum <- 0
  for (i in 1:length(data)) sum <- sum+data[i]
  sum/length(data)
}
x <- rnorm(100, mean=3, sd=1.5) # 정규분포 100개 샘플 추출
x
d.mean(x)

# 2. fix(함수이름)
# 3. 외부 파일 읽기
source('rangefunc.txt')
d.range(x)

## 4. '%함수명%' - 연산자 생성
'%ab%' <- function(a,b) return(2*a+b)
3 %ab% 5


## 함수 편집 : fix/edit(함수이름)
edit(d.range)


## 함수를 위한 함수 : 함수의 속성을 파악하기 위한 함수
# 1. 
is.function(d.mean) # 함수인지 아닌지

# 2.
args(log) # 함수의 매개변수 반환 - 내장 함수도 가능

# 3.
attributes(d.range) # 함수 소스 코드 반환