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