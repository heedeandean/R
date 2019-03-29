library(ggplot2)
mpg <- as.data.frame(ggplot2::mpg)

# column
# manufacturer: 제조사
# displ: 배기량(displacement)
# trans: 변속기 종류
# cyl: 실린더 개수
# drv: 구동방식(drive wheel)
# cty: 도시 연비
# hwy: 고속도로 연비
# fl: 연료종류(fuel)

# 1)
# mpg데이터에서 차종(class)가 suv, compact인 자동차의 모델과 연비관련 변수만 출력하세요.

mpg %>% 
  filter(class %in% c('suv', 'compact')) %>%
  select(model, cty, hwy) 


# 2)
# mpg데이터에서 고속도로연비(hwy) 1 ~ 5위에 해당하는 자동차의 데이터를 출력하세요.

mpg %>%
  arrange(desc(hwy)) %>%
  head(5)


# 3)
# 회사별로 suv 차들의 통합연비(평균) 구해 1 ~ 5위를 출력하세요.

mpg %>%
  filter(class == 'suv') %>%
  group_by(manufacturer) %>%
  summarise(ty = mean(cty + hwy) / 2 ) %>%
  arrange(desc(ty)) %>%
  head(5)


# 4)
# 다음과 같이 연료별 가격이 정해져 있을 때, mpg에 fl_price라는 컬럼을 추가하세요.
# fl   type    price
# c   CNG     1.33
# d   diesel  1.02
# e   E85     0.92
# p   Premi   1.99
# r   Reqular 1.22

# data reset.

mpg <- as.data.frame(ggplot2::mpg)
rm(mpg_t)
rm(mt)

t <- table(mpg$fl)
n <- names(t)
n
dput(n)

fp <- data.frame(fl = c("c", "d", "e", "p", "r"), 
                type = c('CNG', 'diesel', 'E85', 'Premi', 'Reqular'),
                price = c(1.33, 1.02, 0.92, 1.99, 1.22), stringsAsFactors = F)
fp

# class 같아야 join 할 때 warning 안뜸.

class(fp$fl)
class(mpg$fl)

# 1안.

mpg_t <- left_join(mpg, fp[, c(1, 3)], by = c('fl' = 'fl')) %>% rename(fl_price = price)
head(mpg_t)

# 2안.

mt <- inner_join(mpg, fp[, c(1, 3)], by = c('fl' = 'fl')) %>% rename(fl_price = price)
head(mt)

dim(mpg_t)
dim(mt)
