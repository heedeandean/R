setwd('/Users/hj/git/R/basic/data') # 작업 디렉토리 설정 - \\  or / 사용 가능
getwd() # 작업 디렉토리 확인

## 데이터 입력
x <- c(1, 2, 3, 4, 5) # 벡터 자료구조 
(y <- c(10, 20, 30, 40, 50)) # (결과를 바로 화면에 출력)
dat <- cbind(x, y) # 두 벡터를 열(column) 방향으로 결합하는 함수
dat

w1 <- scan() # 자료 입력, 마지막 엔터 두번
w1

#dat3 <- data.frame()
#dat3 = edit(dat3)


## 데이터 저장
sink('iris.txt') # 저장할 파일명 지정
summary(iris)
sink() # 끝

write.csv(dat, 'dat.csv')
write.table(dat, 'dat.txt') # 탭 구분
write.table(dat, 'dat2.txt', sep=',') 


## 데이터 불러오기
dat2 <- read.csv('dat.csv')
dat2

us_dat <- read.csv('USArrestd.csv') # default 옵션: header=T, stringsAsFactors=F
head(us_dat)
str(us_dat) # 데이터 구조 확인

read.table('dat.txt')

nadat <- read.table('dat.txt', na.strings = '10') # 특정한 문자를 결측치로 인식
nadat

ls() # 모든 객체(object) 확인
rm(list = ls()) # 모든 객체 삭제