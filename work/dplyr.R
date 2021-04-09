getwd()

library(dplyr)
library(ggplot2)

mydata <- read.csv("./Data/sampledata.csv")

head(mydata)
str(mydata)

## 1) column 선택 : select

# Index와 State~Y2008 까지 선택.
mydata %>% 
  select(Index, State:Y2008) %>% 
  head()

# Index와 State column제거하고 출력
mydata %>% 
  select(-Index, -State) %>% 
  head()
  
# Index와 State~Y2008 column을 제거하고 출력.
mydata %>% 
  select(-Index, -(State:Y2008)) %>% 
  head()

# 모든 column 을 출력
mydata %>% 
  select(everything())

# 상관분석할때 목적변수를 앞에 놔야 하기 때문에, 그때 everything()사용
mydata %>% 
  select(State, everything()) %>% 
  head()

# iris data에서 factor형 data의 column만 선택하기.
str(iris)

iris %>% select_if(is.factor)

# =========================================================
## 2) rename

# "Index" column을 "Index1"으로 변경해서 출력
mydata %>% 
  rename(Index1 = Index) %>% 
  head()

# =========================================================

## 3) row 선택 : filter()

# Index column의 값이 "A" 인 것들만 출력
mydata %>% 
  filter(Index == 'A')

# Index가 'A' or 'C' 인 row 만 추려냄.
mydata %>% 
  filter(Index %in% c('A', 'C'))

# 
mydata %>% 
  filter(!Index %in% c('A', 'C')) %>% 
  head()


# Index가 'A' or 'C' 이며,  Y2002 column이 1300000보다 큰 row만 출력
mydata %>% 
  filter(Index %in% c('A', 'C') & Y2002 > 1300000)

# =========================================================

## 3) summarise

# Y2015 column의 평균과 중앙값.
mydata %>% 
  summarise(Y2015_mean = mean(Y2015)
            , Y2015_median = median(Y2015))

# Y2005, Y2006 column에 대한 개수, mean, median 을 계산하여라.
mydata %>% 
  summarise_at(vars(Y2005, Y2006)
               , list(mean=~mean(.), median=~median(.)))


# Index 별로, Y2011~Y2015까지의 column을 각각 개수(n())와 평균(mean(., na.rm=TRUE))을 구할것.
mydata %>% 
  group_by(Index) %>% 
  summarise_at(vars(Y2011:Y2015)
               , list(n = ~n(), mean=~mean(., na.rm=TRUE))) %>% 
  head()

# =========================================================

## 4) sorting : arrange()

# Index와 그리고 Y2011 column으로 sorting
mydata %>% 
  arrange(Index, Y2011) %>% 
  head()

# Index는 내림차순으로 Y2012는 올림차순으로 sorting.
mydata %>% 
  arrange(desc(Index), Y2012) %>% 
  head()

# =========================================================

## 5) rank
# min_rank() 를 이용, Index별로 Y2015 column값중 2번째로 작은 값을 구하기.

x <- c(1, 1, 1, 5, 5, 9, 7)

row_number(x) # 결과가 1 2 3 4 5 7 6

min_rank(x) #결과가 1 1 1 4 4 7 6이 나오도록

dense_rank(x) # 결과가 1 1 1 2 2 4 3이 나오도록

# =========================================================

## 6) mutate
# Y2015 column 값을 Y2014 column 값으로 나누어 기존변수에 change column으로 추가할것.
mydata %>% 
  mutate(change=Y2015/Y2014) %>% 
  head()
  
# Y2015 column 값을 Y2014 column 값으로 나누어 change column만으로 표현
mydata %>% 
  transmute(change=Y2015/Y2014) %>% 
  head()

# 모든 변수 값에 1000을 곱하여 새로운 (new_변수명)변수로 생성할것.
mydata %>% 
  mutate_if(is.numeric, list(new=~.*1000)) %>% 
  head()

# 모든 변수 값에 1000을 곱하여 기존변수에 넣기.
mydata %>% 
  mutate_if(is.numeric, list(~.*1000)) %>% 
  head()