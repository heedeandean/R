# purrr 문제
# ---------------------------------------------------------------------
library(tidymodels)
library(stringr)

# 데이터들
a <- c(1, 22, 333, 4444, 55555)
b <- c('abc', 'de', 'zzd')
c <- c(1, 1, 1, 1, 1)
aa <- c(5,4,3,2,1); bb <- c(1,2,3,4,5); cc <- c(3)
lr <- list( list(a = 1, b = "A"), list(a = 3, b = "D", c=c(1, 2)))
df <- data.frame(
  x = 1:3, 
  y = 6:4
)
k <- map(1:4, ~ sample(1:10, 15, replace=T))
df2 <- data.frame(x=1:3, y=c("a", "b", "c"), stringsAsFactors = T)
df3 <- data.frame(
  num1 = c(0, 10, 20),
  num2 = c(5, 6, 7),
  chr1 = c('a', 'b', 'c'),
  stringsAsFactors = FALSE
)
mt <- matrix(1:20, nrow=5)
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(1000)
mydata <- read.csv("https://raw.githubusercontent.com/deepanshu88/data/master/sampledata.csv")
mydata <- mydata %>% as_tibble()

# =============Start==================
# a <- c(1, 22, 333, 4444, 55555)

# 각 vector의 길이구하기
map_int(a, str_length)

# 각 벡터에 aa <- c(5,4,3,2,1) 벡터값을 weight로 가중평균을 구하기 (NA제거)
pmap_dbl(list(a, aa), weighted.mean, na.rm=TRUE)
map2_dbl(a, aa, ~weighted.mean(.x, .y), na.rm=T)

# ===============================
# b <- c('abc', 'de', 'zzd') 

# 'z'를 각 vector의 뒤에 붙이기
map_chr(b, ~paste0(.x, 'z'))

# 각 벡터에 'string'문자를 추가해서 라인별로 프린트
walk(b, ~cat(.x, 'string\n'))

# ===============================
# aa <- c(5,4,3,2,1); bb <- c(1,2,3,4,5) 

# aa, bb 각 벡터들을 더하기
map2(aa, bb, sum)

# aa, bb 각 벡터들을 더한 후 벡터로
map2_dbl(aa, bb, sum)

# aa, bb 각 벡터들을 더한 후 2를 곱할것
map2_dbl(aa, bb, ~(.x+.y)*2)


# cc <- c(3) 

# aa,bb 각 벡터들을 더한 후 cc를 곱해줄것
pmap_dbl(list(aa, bb, cc), ~(..1+..2)*..3)

# ===============================
# mydata의 숫자형 column들의 sum을 구할것
mydata %>% select_if(is.numeric) %>% map_df(sum)
# mydata %>% modify_if(is.numeric, mean)
# mydata %>% keep(is.numeric) %>% map_df(mean)

# mydata의 숫자형 column들의 값을 1000으로 나누기
mydata %>% select_if(is.numeric) %>% map_df(~.x/1000) 

# mydata의 숫자형 column들의 값을 1000으로 나누어 원래 자료형 그대로 출력
mydata %>% select_if(is.numeric) %>% modify(~.x/1000) 

# mtcars 변수들의 평균을 NA제외하고 구하기
mtcars %>% map_dbl(mean, na.rm=T)

# mtcars 변수들의 mean, median, sd를 구하기
map(list(mean, median, sd), ~mtcars %>% map_dbl(.x))

# ===============================
# lr <- list( list(a = 1, b = "A"), list(a = 3, b = "D", c=c(1, 2))) 

# 각 list의 a 집합만 선택하기
map_dbl(lr, 'a')

# 각 두번째 집합만 선택하기
map_chr(lr, 2)

# 세번째 집합 선택
map(lr, 3)

# 세번째 집합 선택 (값이 없는 경우 NA로 채울것)
map(lr, 3, .default=NA)

# ===============================
# 1~ 10, 3을 곱하여 벡터로 리턴
map_dbl(1:10, ~.x*3)

# 1~ 10, 3을 곱하여 벡터로 리스트로 리턴
map(1:10, ~.x*3)

# mtcars column값들이 character인지 벡터로 리턴
mtcars %>% map_lgl(is.character)

# mtcars column들의 unique한 값들의 개수
map_dbl(mtcars, ~length(unique(.x)))
mtcars %>% map_int(~length(unique(.x)))

# 1~10, 각 값들의 쌍으로 구성된 값/mea들을 리턴
map(1:10, ~c(.x, .x))

# 5개의 난수 값을 3개 만들것
map(1:3, ~runif(5))

# c <- c(1, 1, 1, 1, 1), 각 vector에 같은 난수를 더할것
map_dbl(c, ~.x+.y, runif(1)) 

# c <- c(1, 1, 1, 1, 1), 각 vector에 다른 난수를 더할것
map_dbl(c, ~(.x + runif(1)))

# mtcars의 cyl별로, wt에 따른 mpg의 선형관계의 wt계수를 구하기
mtcars %>% 
  split(mtcars$cyl) %>% 
  map(~lm(mpg~wt, data=.x)) %>% 
  map(coef) %>% 
  map_dbl(2)

# mtcars의 cyl 별로, wt에 따른 mpg 선형회귀 결과 중, p.value가 0.05인 wt변수들의 계수와 p.value를 구하기
mtcars %>% 
  split(mtcars$cyl) %>% 
  map(~lm(mpg~wt, data=.x)) %>% 
  map(tidy) %>% 
  map(~filter(., p.value < 0.05)) %>% 
  map(~filter(., term == 'wt')) %>% 
  map(~select(., estimate, p.value))

# ===============================
# df <- data.frame( x = 1:3,  y = 6:4 )

# 각 column에 2를 곱하여 data.frame으로 리턴
modify(df, ~.x*2)

# ===============================
# The first value of Sepal.Length is 5.1(첫번째 value임)를 출력하기
imap_chr(iris, ~paste0('The first value of ', .y, ' is ', .x[[1]]))

# ===============================
# trims <- c(0, 0.1, 0.2, 0.5) x <- rcauchy(1000)

# x의 평균을 각 비율로 절사평균 구하기 
pmap_dbl(list(trim=trims), mean, x=x)

# ===============================
# 난수를 1개, 0~1로, 2개 10~100, 3개 100~1000 조건으로 생성할것
params <- tibble::tribble(
  ~n, ~min, ~max,
  1L, 0, 1,
  2L, 10, 100,
  3L, 100, 1000
)
pmap(params, runif)

# ===============================
# k <- map(1:4, ~ sample(1:10, 15, replace=T)) 

# k의 각 list에서 모두 나타나는 값을 찾기.
reduce(k, intersect)

# k의 각 list에서 적어도 한번 나타나는 값을 찾기.
reduce(k, union)

# ===============================
# 1~10 값을 순서대로 더한값을 구하기
accumulate(1:10, `+`)

# ===============================
# df2 <- data.frame(x=1:3, y=c("a", "b", "c"), stringsAsFactors = T) 

# df2에서 factor인 값들을 리턴
df2 %>% detect(is.factor)

# df2에서 factor인 위치를 리턴
df2 %>% detect_index(is.factor)

# df2에서 factor인 값을 입력타입으로 리턴
df2 %>% keep(is.factor)

# df2에서 factor가 아닌 값을 입력타입으로 리턴
df2 %>% discard(is.factor)

# ===============================
# df3 <- data.frame( num1 = c(0, 10, 20), 
                   # num2 = c(5, 6, 7), 
                   # chr1 = c('a', 'b', 'c'), stringsAsFactors = FALSE )

# df3에서 숫자만 평균을 구하고 나머지는 그대로 리턴
df3 %>% map_if(is.numeric, mean)

# df3에서 숫자만 평균을 구해서 입력데이터의 형태를 유지하여 리턴(나머지는 그대로)
df3 %>% modify_if(is.numeric, mean)

# df3에서 숫자만 평균을 구할것(나머지는 생략)
map(keep(df3, is.numeric), mean)

# ===============================
# mt <- matrix(1:20, nrow=5)

# row 방향 평균
apply(mt, 1, mean)

# column 방향 평균
apply(mt, 2, mean)

# mt의 역행렬 구하기
apply(mt, 1, identity)
t(mt)
