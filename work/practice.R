library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)

mydata <- read.csv('./data/sampledata.csv')
mydata <- read.csv("https://raw.githubusercontent.com/deepanshu88/data/master/sampledata.csv", stringsAsFactors = FALSE)

glimpse(mydata)
getwd()

x <- data.frame(ID=1:6, ID1=1:6)
y <- data.frame(ID=6:7, ID1=6:7)
df1 <- data.frame(ID=1:6, x=letters[1:6])
df2 <- data.frame(ID=7:12, x=letters[7:12])
k <- c("a", "b", "", "d")
df <- data.frame(x=c(1, 5, 6, NA))
x1 <- c(1, 1, 1, 5, 5, 9, 7)
df3 <- c(-10, 2, NA)
mydf <- data.frame(x = c(1:5, NA))
mtcars$model <- rownames(mtcars)
first <- mtcars[1:20, ]
second <- mtcars[10:32, ]
x3 <- data.frame(N=1:10)
data <- c(1:10)
mydata2 <- data.frame(X1=sample(1:100, 100), X2=runif(100))

# =============
# 랜덤한 3개의 row 선택하여 출력
mydata %>% 
  sample_n(3)

# 0.1의 비율(10%)의 row 만 선택하여 출력
nrow(mydata)

mydata %>% 
  sample_frac(0.1)

# 중복되는 행 제거 
mydata %>% 
  distinct()

# Index column을 기준으로 중복되는 row 제거하고 출력.(두가지버전)
mydata %>% 
  distinct(Index) 

mydata %>% 
  distinct(Index, .keep_all = TRUE)

# Index와 Y2010 column을 기준으로 중복되는 row를 제거하고 출력
mydata %>% 
  distinct(Index, Y2010)

# Index와 State~Y2008 까지 선택.
mydata %>% 
  select(Index, State:Y2008) 

# Index와 State column제거하고 출력
mydata %>% 
  select(-Index, -State)

# Index와 State~Y2008 column을 제거하고 출력.
mydata %>% 
  select(-Index, -(State:Y2008)) 

# "Y"로 시작하는 Column만 출력
mydata %>% 
  select(starts_with('Y'))

# "Y"로 시작하는 Column만 제거하고 출력
mydata %>% 
  select(-starts_with('Y'))

# "x" 로 끝나는 column 만 출력
mydata %>% 
  select(ends_with('x'))

# "00" 을 포함하는 column만 출력
mydata %>% 
  select(contains('00'))

# "Y2003" 부터 "Y2008" 까지의 column만 출력
mydata %>% 
  select(Y2003:Y2008) 

# "Y2001", "Y2002", "X2010" 셋중하나라도 해당하는 column 출력
mydata %>% 
  select(one_of(c("Y2001", "Y2002", "X2010"))) 

# 모든 column 을 출력
mydata %>% select(everything())

# "I" 문자를 포함하는 column명만 출력
mydata %>% 
  select(contains('I'))

# "State" column을 가장 앞에 출력
mydata %>% 
  select(State, everything())

# "Index" column을 "Index1"으로 변경해서 출력
mydata %>% 
  rename(Index1 = Index) 

# Index column의 값이 "A" 인 것들만 출력
mydata %>% 
  filter(Index == 'A')

# Index가 'A' or 'C' 인 row만 출력
mydata %>% 
  filter(Index %in% c('A', 'C'))

# Index가 'A' or 'C' 이며,  Y2002 column이 1300000보다 큰 row만 출력
mydata %>% 
  filter(Index %in% c('A', 'C') & Y2002 > 1300000)

# Index가 'A' or 'C' 이거나, Y2002 column이 1300000보다 큰 row만 출력
mydata %>% 
  filter(Index %in% c('A', 'C') | Y2002 > 1300000)

# Index가 "A" 도 아니고 "C"도 아닌 row들만 출력
mydata %>% 
  filter(!Index %in% c('A', 'C')) 

# State column의 값이 "Ar"을 포함하고 있는 row 만 출력
mydata %>% 
  filter(grepl('Ar', State))

# Y2015 column의 평균과 중앙값.
mydata %>% 
  summarise(Y2015_mean = mean(Y2015), Y2015_median = median(Y2015))

# Y2005, Y2006 column에 대한 개수, mean, median 을 계산하여라.
mydata %>% 
  summarise_at(vars(Y2005, Y2006), 
               list(n=~n(), mean=~mean(.), median=~median(.)))

# Y2011, Y2012 column에 대한 mean, median 을 계산하되, NA 값이 있으면 제외할것.
mydata %>% 
  summarise_at(vars(Y2011, Y2012), 
               list(mean=~mean(., na.rm=TRUE), median=~median(., na.rm=TRUE)))

mydata %>% 
  summarise_at(vars(Y2011, Y2012),
               list(mean=mean, median=median),
               na.rm=TRUE)

# 원래 값(X1, X2)에서 평균을 빼고, 그 분산구하기)
mydata2 <- data.frame(X1=sample(1:100, 100), X2=runif(100))

mydata2 %>% 
  summarise_at(vars(X1, X2), list(function(x) var(x-mean(x))))

# 숫자인 값들만 평균과 중앙값을 구할것.
mydata %>% 
  mutate_if(is.numeric, list(mean=~mean(.), median=~median(.)))

mydata %>% 
  summarise_if(is.numeric, list(mean=~mean(.), median=~median(.)))

# 숫자인 값들만 평균을 구하는데 NA 값은 제외시키고 구할것.
mydata %>% 
  mutate_if(is.numeric, list(mean=~mean(., na.rm=TRUE)))

mydata %>% 
  summarise_if(is.numeric, list(mean=~mean(., na.rm = TRUE)))

# iris데이터 Species별로, mean, med, sd 를 각각 적용한 변수를 생성
iris %>% 
  group_by(Species) %>% 
  summarise_all(list(mean=mean, med=median, sd=sd))

# Index와 그리고 Y2011 column으로 sorting
mydata %>% 
  arrange(Index, Y2011)

# Index는 내림차순으로 Y2012는 올림차순으로 sorting.
mydata %>% 
  arrange(desc(Index), Y2012)

# Index 별로, Y2011~Y2015까지의 column을 각각 개수(n())와 평균(mean(., na.rm=TRUE))을 구할것.
mydata %>% 
  group_by(Index) %>% 
  summarise_at(vars(Y2011:Y2015),
               list(n=~n(), mean=~mean(., na.rm=TRUE)))

# Index column에서 A, C 그리고 I 를 각각 2개씩만 뽑을것.
mydata %>% 
  filter(Index %in% c('A', 'C', 'I')) %>% 
  group_by(Index) %>% 
  do(head(., 2))

# Index별로, Y2015 column 값중 세번째로 큰값을 구하라
mydata %>% 
  group_by(Index) %>% 
  filter(min_rank(Y2015) == 3) %>% 
  select(Y2015)

# test
mydata %>% 
  select(Index, Y2015) %>% 
  filter(Index == 'A')

# min_rank() 를 이용, Index별로 Y2015 column값중 2번째로 작은 값을 구하기.
mydata %>% 
  group_by(Index) %>% 
  filter(min_rank(Y2015) == 2) %>% 
  select(Y2015)

x <- c(1, 1, 1, 5, 5, 9, 7)

# 결과가 1 2 3 4 5 7 6이 나오도록
row_number(x)

# 결과가 1 1 1 4 4 7 6이 나오도록
min_rank(x)

# 결과가 1 1 1 2 2 4 3이 나오도록
dense_rank(x)

# Index 별 데이터를, Y2014와 Y2015에 대한 평균을 NULL을 제외하고 구한 후, # Y2015에 대한 평균의 결과를 내림차순으로 정렬해서 출력하기.
mydata %>% 
  group_by(Index) %>% 
  summarise_at(vars(Y2014, Y2015), list(mean=~mean(., na.rm=TRUE))) %>% 
  arrange(desc(Y2015_mean))

# Y2015 column 값을 Y2014 column 값으로 나누어 기존변수에 change column으로 추가할것.
mydata %>% 
  mutate(change = Y2015/Y2014)

# Y2015 column 값을 Y2014 column 값으로 나누어 change column만으로 표현
mydata %>% 
  transmute(change = Y2015/Y2014)

# 모든 변수 값에 1000을 곱하여 새로운 (변수명_new)변수로 생성할것.
mydata %>% 
  mutate_if(is.numeric, list(new=~.*1000))

# 모든 변수 값에 1000을 곱하여 기존변수에 넣기.
mydata %>% 
  mutate_if(is.numeric, list(~.*1000))

# Y2008~Y2010 변수값들의 순위를 계산해서 새로운변수로 추가한다.
mydata %>% 
  mutate_at(vars(Y2008:Y2010), list(rank=~min_rank(.)))

# Y2008~Y2010 변수값들의 내림차순 순위를 계산해서 새로운변수로 추가한다.
mydata %>% 
  mutate_at(vars(Y2008:Y2010), list(rank=~min_rank(desc(.))))

# Index별, Y2015년도에 가장높은 값을 가진 State를 출력하기.
mydata %>% 
  group_by(Index) %>% 
  filter(min_rank(desc(Y2015)) == 1) %>% 
  select(Y2015, State)

# State column의 첫글자를 이용해 Index2 column을 생성하기
mydata %>% 
  mutate(Index2 = substr(State, 1, 1))

# Index별로 Y2015의 값을 누적해서 Total column명으로 출력하기.
mydata %>% 
  group_by(Index) %>% 
  mutate(Total = cumsum(Y2015)) %>% 
  select(Y2015, Total)

#(data <- c(1:10)), data에 누적값 column을 생성하고, 다시 원래 column을 구하기.
data <- c(1:10)

data_sum <- data.frame(a=data, b=cumsum(data))
data_sum

data_sum %>% 
  transmute(c = lag(b, 1, 0)) -> data_lag

cbind(data_sum$b - data_lag$c)

# df1, df2 공통의  column으로 join (key는 ID column)
df1
df2
inner_join(df1, df2, by='ID')

# df1, df2 에서 df1을 기준으로 join (key는 ID column)
left_join(df1, df2, by='ID')


mtcars$model <- rownames(mtcars)
first <- mtcars[1:20, ] 
second <- mtcars[10:32, ]
# 1. row방향으로 같은 데이터만.
intersect(first, second)

# 2. row방향으로 first에서second빼기
setdiff(first, second)

x <- data.frame(ID=1:6, ID1=1:6)
y <- data.frame(ID=6:7, ID1=6:7)
# 1. row방향으로 합치기(겹치는데이터한번만출력)
union(x, y)

# 2. row방향으로 합치기(겹치는데이터도모두출력)
union_all(x, y)
bind_rows(x, y)


df3 <- c(-10, 2, NA)
# 음수는 negative, 양수는 positive, NA 는 missing value로 표시할것
if_else(df3 < 0, 'negative', 'positive', 'missing value')


df <- data.frame(x=c(1, 5, 6, NA))
# 5보다작으면 +1, 같거나크면+2, 둘다아니면0으로 column 추가
df %>% 
  mutate(new_var = if_else(x < 5, x+1, x+2, 0))

mydf <- data.frame(x = c(1:5, NA))
# 1은 one, 2는 two, 나머지 숫자는 other로 표시할것, na는 'missing'으로 표기
mydf %>% 
  mutate(new_var = if_else(is.na(x), 'missing', if_else(x == 1, 'one', if_else(x == 2, 'two', 'other'))))

# Y2012~Y2015 중 가장 큰값들로 구성되어있는 Max라는 새로운 column을 표시할것
mydata %>% 
  rowwise() %>% 
  mutate(Max = max(Y2012, Y2013, Y2014, Y2015)) %>% 
  select(Y2012:Y2015, Max)


df1 <- data.frame(ID=1:6, x=letters[1:6])
df2 <- data.frame(ID=7:12, x=letters[7:12])
# 1. row방향 합치기
bind_rows(df1, df2)

# 2. column방향 합치기
bind_cols(df1, df2)


# mydata 에서 Index 별로, Y2012 column의 25%, 50%, 75%, 99% 에 해당하는 값을 구할것 
mydata %>% 
  group_by(Index) %>% 
  summarise(percentile_25 = quantile(Y2012, prob = 0.25),
            percentile_50 = quantile(Y2012, prob = 0.50),
            percentile_75 = quantile(Y2012, prob = 0.75),
            percentile_99 = quantile(Y2012, prob = 0.99))


x3 <- data.frame(N=1:10)
# 1부터 10까지의 데이터 x3을 순서대로 5개의 index column을 추가할것
x3 %>% 
  mutate(index = ntile(N, 5))

# mydata, 숫자형 data의 column만 선택하기.
mydata %>% 
  select_if(is.numeric)

# iris data에서 factor형 data의 column만 선택하기.
iris %>% 
  select_if(is.factor)

# mydata data의 character형 데이터만 factor형 변수로 바꾼후, 그 변수의 level수를 표시하기.
mydata %>% 
  mutate_if(is.character, list(as.factor)) %>% 
  summarise_if(is.factor, list(n_level = nlevels))

# numeric 변수에 1000을 곱하여 새로운변수 생성하기.
mydata %>% 
  mutate_if(is.numeric, list(new_var=~.*1000))


k <- c("a", "b", "", "d")
# ""를 NA로 변환하기
k %>% 
  na_if("")
  
# iris데이터의 Sepal.Length column data를 세가지 표현방법으로 나타내기.
iris$Sepal.Length
iris[['Sepal.Length']]
iris %>% pull(Sepal.Length)

# Sepal.Length가 5.5보다 큰 Species column만 출력하기.
iris %>% 
  filter(Sepal.Length > 5.5) %>% 
  pull(Species)

# iris data의 Species column 의 값이 "setosa" 인 data만 출력하도록
iris %>% 
  filter(Species == "setosa")

# Index 별로, Y2015 변수의 가장 큰 값 순서(rank)를 구한후, Index와 rank 순으로 정렬하기
mydata %>% 
  group_by(Index) %>% 
  mutate(rank=min_rank(desc(Y2015))) %>% 
  arrange(Index, rank) %>% 
  select(Y2015, rank)

# carat별 price를 산점도로.
diamonds %>% 
  ggplot(aes(x=carat, y=price)) +
  geom_point()

# carat 별 price를, bar graph로
diamonds %>% 
  ggplot(aes(x=carat, y=price)) +
  geom_bar(stat = 'identity')

# carat 별 count를, bar graph로.
diamonds %>% 
  ggplot(aes(x=carat)) +
  geom_bar(stat = 'count')

# carat별 price를, 색이 cut인 산점도로.
diamonds %>% 
  ggplot(aes(x=carat, y=price)) +
  geom_point(aes(color=cut))

# carat별 price를, 색이 cut이고 size가 table인 산점도로.
diamonds %>% 
  ggplot(aes(x=carat, y=price)) +
  geom_point(aes(color=cut, size=table))

# carat별 price를, 색이 cut이고 group이 cut인 boxplot으로
diamonds %>% 
  ggplot(aes(x=carat, y=price)) + 
  geom_boxplot(aes(color=cut, group=cut))

# carat별 price를, 산점도로, 색이 cut이고, cut별로 가로로 세분화.
diamonds %>% 
  ggplot(aes(x=carat, y=price)) +
  geom_point(aes(color=cut)) +
  facet_grid(~cut)

# carat별 price를, 산점도로, 색이 cut이고, cut과 color로 가로로 세분화
diamonds %>% 
  ggplot(aes(x=carat, y=price)) +
  geom_point(aes(color=cut)) +
  facet_grid(~cut + color)

# carat별 price를, 산점도로, cut별로 가로세로 세분화
diamonds %>% 
  ggplot(aes(x=carat, y=price)) +
  geom_point() +
  facet_wrap(~cut)

# cut별 carat을, 색이 cut이고, y축이 평균값인 bar graph로.
diamonds %>% 
  ggplot(aes(x=cut, y=carat, fill=cut)) +
  stat_summary_bin(fun = 'mean', geom = 'bar')

# carat별 price를, color가 cut인 산점도로, x축은 0~3, y축은 0~20000
diamonds %>% 
  ggplot(aes(x=carat, y=price)) +
  geom_point(aes(color=cut)) +
  coord_cartesian(xlim = c(0, 3), ylim = c(0, 20000))

# carat별 price를, color가 cut이고 group이 cut인 boxplot으로, 90도 돌려서표현.
diamonds %>% 
  ggplot(aes(x=carat, y=price)) +
  geom_boxplot(aes(color=cut, group=cut)) +
  coord_flip()

# carat별 price를, color가 cut인 산점도,배경은흰색,title은'carat과 price의 관계' x축은'carat'y축은'price',legend는 아래로,패널의grid는없애고,y축값에'$'와천원단위로','추가해서시각화.
diamonds %>% 
  ggplot(aes(x=carat, y=price)) +
  geom_point(aes(color=cut)) +
  theme_bw() +
  labs(title = 'carat과 price의 관계', x = 'carat', y = 'price') +
  theme(legend.position = 'bottom',
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  scale_y_continuous(
    labels = function(x) {
      paste0('$', format(x, big.mark = ','))
    }
  )

# iris의 column값(Species제외)들의 분포를 histogram으로.
iris %>% 
  rowid_to_column() %>% 
  pivot_longer(-c(Species, rowid), 
               names_to = 'name', values_to = 'value') %>% 
  ggplot(aes(x=value, fill=name)) +
  geom_histogram() +
  facet_grid(~name)

# iris의 Sepal.Length의 밀도 그래프 그리기. Species별로 구분.
iris %>% 
  ggplot(aes(x=Sepal.Length, fill=Species)) +
  geom_density(alpha=.5)
  
# cume_dist()를 이용, Y2002 column값 중 하위 0.1이하인 값들을 구하기.
cume_dist(mydata$Y2002)

mydata %>% 
  filter(cume_dist(Y2002) <= 0.1) %>% 
  select(Y2002)

# percent_rank()를 이용, Y2002 column값 중 상위 0.1 이상인값들을 구하기.
mydata %>% 
  filter(percent_rank(desc(Y2002)) <= 0.1) %>% 
  select(Y2002)