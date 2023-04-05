# R : 프로그래밍 언어

## 1. 산술연산자
v1 <- c(1, 2) # 벡터
v2 <- c(3, 4)
v1+v2
v1*v2 # 동일 위치 원소 간의 곱
v1/v2 # 동일 위치 원소 간의 나눗셈

A <- matrix(c(1,2,3,4), ncol=2) # 행렬
B <- matrix(c(3,4,5,6), ncol=2)
A+B
A*B
B/A
A^B # 제곱연산자
B%/%A # 몫
A%*%B # 행렬의 곱
solve(A) # 역행렬

# 2. 비교연산자 : ==, !=, ...
# 3. 논리연산자 
## and
2==2 && c(2==2, 3>4) # 출력은 되지만 원하는 결과가 아닐 수 있다.
2==2 & c(2==2, 3>4)
## or
2!=2 || c(2==2, 3>4)
2!=2 | c(2==2, 3>4)

# 4. 집합연산자
x <- c(1,2,5)
y <- c(5,1,7,8)
union(x,y) # 합집합(중복x)
intersect(x,y) # 교집합
setdiff(x,y) # 차집합
setequal(x,y) # 집합x,y가 같은지 여부
setequal(x,c(1,5,2)) 
5 %in% x # 원소가 집합 x에 속하는지 여부
choose(5,2) # 5개의 원소를 가진 집합에서 2개의 원소를 추출하여 만들 수 있는 부분집합의 수

#####################
pi
options(digits = 20)
pi

log(2) # 자연로그
log10(10) # 상용로그
exp(10) # 지수로그
sqrt(4) # 루트, 제곱근

x <- c(1,2,-3,4)
range(x)
c(min(x), max(x))

y <- c(2,4,-6,7)
pmin(x,y)
pmax(x,y)
#####################
x <- c(1,2,3,4)
y <- c(2,1,4,5)
ifelse(x<y, x, y) # = pmin
#####################
# 반복문
for(i in 1:5) print(rep(i,i))

i <- 1
while(i<=5) {
  print(rep(i,i))
  i <- i+1
}

i <- 1
repeat {
  if(i>5) break
  print(rep(i,i))
  i <- i+1
}

## 1~10 합
x <- 0
for(i in 1:10) x <- x+i
x

i <- 1
x <- 0
while(i <= 10) {
  x <- x+i
  i <- i+1
}
x

i <- 1
x <- 0
repeat {
  if(i>10) break
  x <- x+i
  i <- i+1
}
x

# Q : while을 사용하여 1~10 숫자 중 홀수의 합을 구해라.
i <- 1
x <- 0
while(i <= 10) {
  x <- x+i
  i <- i+2
}
x