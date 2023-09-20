birthyear <- function() {
  age = readline(prompt = 'Enter age: ') # 입력 함수
  curyear = as.numeric(format(Sys.Date(), '%Y'))
  b.year = curyear - as.numeric(age)
  list(born.year=b.year)
}
birthyear()

# 텍스트 파일 
## 읽기
setwd('/Users/hj/git/R/basic/data') 

myiris <- read.table(file.choose(), header=T, sep='\t')
myiris <- read.table('./iris.txt', header=T, sep='\t')
myiris <- read.table(url("http://jupiter.hallym.ac.kr/ftpdata/data/iris.txt"), 
                     skip=9, 
                     col.names=c("No", "SepalLength"," SepalWidth","PetalLength","PetalWidth","Species"))

head(myiris)
tail(myiris)

## 쓰기
write.table(mtcars, file.choose(), row.names=T, quote=F, sep=',', fileEncoding='UTF-8')
write.table(mtcars, './mtcars.csv', row.names=T, quote=F, sep=',', fileEncoding='UTF-8')

# 엑셀
## 쓰기
install.packages('xlsx')
library(xlsx)

name <- c("강대성", "한준호", "김종욱", "박상호", "김소현")
number <- c(87, 73, 53, 65, 69)
df <- data.frame(name, number)
df

write.xlsx(df, './score.xlsx', sheetName='data', row.names=FALSE)

# 문자열
nchar('abc') # 문자 개수
strsplit('abcabab', split='b')
paste('abc', 'b', sep='')
rep('a', 5)

a <- 'abcdefghijakalmn'
substr(a, 4, 7)

# regexpr: 패턴 처음 위치 반환(numeric) 
# gregexpr: 패턴 모든 위치 반환(list)
regexpr('a', a, fixed=T, useB=F)[1] 
unlist(gregexpr('a', a, fixed=T, useB=F))

# grep: 패턴이 포함된 인덱스 반환
# grepl: T/F 반환
strvec <- c('a', 'aa', 'c', 'd')
grep('a', strvec)
grepl('a', strvec)

# sub: 처음 패턴만 바꾸기
# gsub: 모든 패턴 바꾸기
sub('a', 'AA', a)
gsub('a', 'AA', a)

sub('[^0-9]', '-', c('12.5', 'ab', 'abc', ''))
sub('010\\-[0-9]{4}\\-1234', 'OK', c('010-1234-1234', '016-4321-4321', '010-123-1234', '010-0000-1234'))