setwd('/Users/hj/git/R/basic/data') # 작업 디렉토리 설정 - \\  or / 사용 가능
getwd() # 작업 디렉토리 확인

##### 자료구조 ##### 

# 1. 벡터 : 가장 기본
# (1). c() 
## 수치형, 문자형, 논리형 중 원소는 한가지 형태만 가능
## 수치형
v1 <- c(3:7)
length(v1) # 원소의 개수

## 논리형 - 대문자로 표시
c(F, F, T)
c(FALSE, TRUE)

# (2). scan()

# (3) 수열
seq(from=3, to=7, by=1) # from=시작값, to=종료값, by=증가분
seq(3, 7, 1) 
seq(-pi, pi, 5)

## 조건 지정
seq(from=3, to=7, length=3) 
seq(from=1, by=0.05, along=1:5) 
seq(from=1, to=5, along=1:6) 
seq(1, 5, along=1:6) 

# (4). 반복
rep(c(1,2), times=2) 
rep(c(1,2), 2) 
rep(c(1,2), times=c(2,1)) # 앞의 원소는 2회, 뒤의 원소는 1회 반복
rep(c(1,2), each=2) # 각 원소를 2회 반복
rep(c(1,2), length=5) # 벡터를 반복하되 벡터의 크기를 5로 한정

## 가공
v1 <- c(11:20)
v1
v1[c(3,5)] # 3, 5번째 원소 출력
v1>15
v1[v1>15] 
v1[c(-2,-4)] # 2, 4번째 원소 삭제

v2 <- c(3:7)
v2
replace(v2,2,10) # v2의 2번째 원소 값을 10으로 변경
append(v2,8,after=5) # v2의 5번째 원소 뒤에 8 추가

x <- c(rep(3,3),seq(3,7,by=2),rev(seq(3,7,length=3)),rep(4,3))
x
sort(x) # 오름차순 정렬 
rank(x)
order(x)

logical(3)
numeric(3)
complex(3)
character(3)


# 2. 행렬
## 2차원의 데이터 구조
## 벡터의 속성 포함
matr <- matrix(1:9, nrow=3) # 3 x 3
matr
length(matr) # 원소 개수
mode(matr) # 자료형
dim(matr) # 행 열

c1 <- c(1:3)
c2 <- c(4:6)
c3 <- c(7:9)
rbind(c1,c2,c3) # 행을 기준으로 결합
cbind(c1,c2,c3) # 열을 기준으로 결합

m1 <- 1:9 # = c(1:9)
dim(m1) <- c(3, 3)
m1

mat <- matrix(c(1:9), ncol=3, byrow=T) # 행 기준 3열
mat
mat[1,] # 1행
mat[,3] # 3열
mat[,3] > 4
mat[mat[,3] > 4, 2] 
mat[2,3] # 2행 3열

height <- c(140,155,142,175)
size.1 <- matrix(c(130,26,110,24,118,25,112,25), ncol=2, byrow=T,
                 dimnames=list(c('Lee', 'Kim', 'Park', 'Choi'), c('Weight', 'Waist'))) # 행, 열 이름 부여
size.1
size <- cbind(size.1, height)
size
colmean <- apply(size, 2, mean) # 1: 행, 2: 열 / var...
colmean
rowmean <- apply(size, 1, mean) 
rowmean

sweep(size, 2, colmean)
size
sweep(size, 1, c(1,2,3,4), '+')

m1 <- matrix(1:4, nrow=2)
m1
m2 <- matrix(5:8, nrow=2)
m2
m1*m2 
m1%*%m2 # 행렬 곱셈
solve(m1) # 역행렬
t(m1) # 전치행렬


# 3. 배열 : 2차원 이상(행렬) / 일반적으론 3차원 이상
arr <- array(1:24, c(3,3,2))
arr
dimnames(arr) <- list(paste('row', c(1:3)), paste('col', c(1:3)), paste('ar', c(1:2)))
arr
length(arr) # 자료 개수
mode(arr) # 자료형
dim(arr) # 각 차원 벡터 크기
dimnames(arr)

array(1:6)
array(1:6, c(2,3))
array(1:6, c(2,3))
array(1:8, c(2,2,2))

arr <- c(1:24)
dim(arr) <- c(3,4,2)
arr

ary1 <- array(1:8, dim = c(2,2,2))
ary1
ary2 <- array(8:1, dim = c(2,2,2))
ary2
ary1+ary2
ary1*ary2
ary1%*%ary2 # 두 배열 원소들의 곱의 합
sum(ary1*ary2)

ary1
ary1[,,1]
ary1[1,1,]
ary1[1,,-2]


# 4. 리스트 : 원소가 여러 자료형을 가질 수 있음. (cf. 행렬, 배열 - 단일 자료형)
a <- 1:10
b <- 11:15
klist <- list(vec1=a, vec2=b, descrip='ex')
klist
length(klist)
mode(klist)
names(klist)

list1 <- list('A', 1:8)
list1
list1[[3]] <- list(c(T, F)) # 3번째 성분 추가
list1
list1[[2]][9] <- 9
list1
list1[3] <- NULL # 3번째 성분 삭제
list1[[3]] <- NULL # 3번째 성분 삭제
list1
list1[[2]] <- list1[[2]][-9] # 2번째 성분의 9번째 원소 삭제
list1

a <- 1:10
b <- 11:15
nlist <- list(vec1=a, vec2=b, descrip='ex')
nlist
nlist[[2]][5]
nlist$vec2[c(2,3)]


# 5. 데이터 프레임
## 각 열(=변수)은 서로 다른 자료형 OK
d2 <- read.table('./story.txt', row.names='num', header=T)
d2

char1 <- rep(LETTERS[1:3], c(2,2,1))
char1
num1 <- rep(1:3, c(2,2,1))
num1
test1 <- data.frame(char1, num1)
test1

a1 <- c(letters[1:15])
a1
dim(a1) <- c(5,3) # 5x3 행렬 생성
a1
test3 <- as.data.frame(a1)
test3

test1
test3
cbind(test1, test3)

char1
num1
test4 <- data.frame(char1, num1)
test4

test1
test4
rbind(test1, test4)
cbind(test1, test4)
merge(test1, test4)

# 저장 시 변수명 차이 존재
write.table(test1, 'test1.txt') 
write.csv(test1, 'test1.csv') 

#####################################
# cf. 인덱스 시작: R(1부터~)/Python(0부터~)

# 1. vector: 1차원
# cf. 자료형 우선순위: 문자형 > 숫자형 > 논리형(F->0/T->1)
xv <- c(1, 2)
is.vector(xv)
xv[c(1,2)]
length(xv) # 원소 개수

xv <- seq(1, 11, by=2)
xv
# 원소 삭제
xv[-3]
xv[-c(1,3,4)]

# 2. 행렬(matrix) = data frame: 2차원
xm <- matrix(data = 1:9, nrow = 3, byrow = T, dimnames = list(c('r1', 'r2', 'r3'), c('col1', 'col2', 'col3')))
xm
xm[1,2]
xm[1,]
xm[1, 2:3]
length(xm)
dim(xm) # 차원(행/열 개수) / 결과 vector

xm <- matrix(1:8, nrow=2, byrow=T)
xm
y1 <- c(9:12)
xm2 <- rbind(xm, y1)
xm2

y2 <- c(10, 20, 30)
xm3 <- cbind(y2, xm2)
xm3
xm2[-2,]
xm2[,-c(1,3)]

mx1 <- matrix(1:6, ncol=3, byrow=T)
mx2 <- cbind(rep(1,3), rep(2,3))
mx1 %*% mx2 # 행렬 곱

# 3. 배열(array): 3차원 이상
# vector, 행렬: 원소들은 모두 같은 형(mode)

# 4. data frame 
x <- seq(1, 10, by=2)
x
y <- letters[1:length(x)]
y
df <- data.frame(id=x, name=y)
df

df$id