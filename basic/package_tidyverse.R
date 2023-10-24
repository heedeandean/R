install.packages('tidyverse')
library(tidyverse)

# tibble: 개선된 데이터 프레임
tb1 <- tibble(x=1:3, y=x+2, z=3)
tb1
tb1+2

tb2 <- tribble(~x, ~y, ~z,
               4, "a", T,
               5, "b", F,
               6, "c", T)
tb2

mpg # head() 자동
print(mpg, n=5, width=Inf)
as.data.frame(mpg)

cars
as_tibble(cars)

?mtcars
head(mtcars)
mtcars1 <- rownames_to_column(mtcars)
mtcars1
mtcars1 <- as_tibble(mtcars1)
mtcars1
class(mtcars)
class(mtcars1)

# dplyr 패키지
# filter(): 조건 나열
filter(mtcars1, mpg<25, cyl %in% c(4,6), am==0) 
filter(mtcars1, mpg>=quantile(mpg, 0.25) & mpg<=quantile(mpg, 0.75))

# arrange(): 오름차순(default)
arrange(mtcars1, mpg)  
arrange(mtcars1, cyl, desc(mpg))
  
# select(): 지정 변수만 추출 
select(mtcars1, rowname, cyl:wt) 
select(mtcars1, -rowname) 

# 
gm <- group_by(mtcars1, am)

# summarize(): 통계량에 조건 부여 (cf. 기존 데이터 프레임: summary() 조건X)
summary(mtcars1)
summarize(mtcars1, 
          n=n(), # 관측치 수
          mean_of_mpg=mean(mpg))
summarize(gm, n=n(), mean_of_mpg=mean(mpg))

# pipe 연산자
mtcars1$mpg %>% mean
mtcars1$mpg %>% sample(5) # 랜덤 추출

#
x <- 3
tibble(x=1:2, x*x, '$'=x^3, !!x) # !!: 외부 변수
tibble(letters[1:2])
tibble(a=1:2, b=diag(2), c=cov(iris[1:2]))

mtcars %>% head
mtcars %>% as_tibble
mtcars %>% as_tibble %>% as.data.frame

mtcars %>% 
  filter(am==0) %>% 
  group_by(carb) %>% 
  summarize(mean(mpg))

mtcars %>% 
  filter(gear!=5) %>% 
  group_by(gear) %>% 
  summarize(
    Avg_mpg = mean(mpg),
    Median_hp = as.numeric(median(hp)),
    Count = n()
  ) %>% 
  arrange(desc(Count))

install.packages('nycflights13')
library(nycflights13)
ls('package:nycflights13')
flights %>% dim
not_cancelled <- flights %>% filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled %>% dim
not_cancelled %>% head(3) %>% .[,1:6]

not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r=min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r)) %>% 
  .[,1:6]

not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))