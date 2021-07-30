rm(list=ls())
setwd('C:/Users/82102/OneDrive/바탕 화면/git/R/analysis/data/')

library(tidyverse)
# ==========================================
a <- c(1, 2, 3, 4, 5)

# To Do! 모든 요소에 1을 더해라

# 1안.
a + 1

# 2안.
for (i in 1:5) {
  print(a[i] + 1)
}

# 3안. *apply(데이터, 함수)
lapply(a, function(x){x+1}) # return list
sapply(a, function(x){x+1}) # return vector 


# 4안. purrr / 가독성++
map(a, function(x){x+1}) # return list
map(a, ~.x+1)

map_dbl(a, function(x){x+1}) # return vector 
map_dbl(a, ~.x+1)

# ==========================================
b <- c(1, 22, 333, 4444, 55555)

# 각 요소 길이
map_int(b, str_length) # return integer
map_dbl(b, str_length) # return numeric

# logical
map_lgl(b, is.numeric)

# ==========================================
c <- c('abc', 'def', 'ghi')

map_chr(c, ~paste0(.x, 'z'))

# ==========================================
d <- c(5, 4, 3, 2, 1)

# 변수 2개 이상 : map2_*()
map2(a, d, sum) # return list 
map2_dbl(a, d, sum) # return vector 
map2_dbl(a, d, ~.x+.y)

# 변수 3개 이상 : pmap_*()
pmap_dbl(list(a, b, d), ~..2-..1+..3) # b-a+d

# ==========================================
e <- list(
  list(-1, x=1, y=c(1), z='a'),
  list(-2, x=4, y=c(2, 3), z='b'),
  list(-3, x=9, y=c(4, 5, 6))
)

map_dbl(e, 1) # -1 -2 -3
map_dbl(e, 2) # 1 4 9
map_dbl(e, 'x') # 1 4 9

map(e, 'y') # return list - 데이터 개수가 다를때 
map(e, list('y', 1)) 

map_chr(e, 'z', .default=NA)

# ==========================================
f <- tibble(a=c(17, 23, 4, 10, 11), 
            b=c(24, 5, 6, 12, 18), 
            c=c(1, 7, 13, 19, 25), 
            d=c(8, 14, 20, 21, 2), 
            e=c(15, 16, 22, 3, 9))

# 각 행의 max는?
f %>% 
  rowwise() %>% 
  mutate(max = max(a, b, c, d, e))

# 각 열 합
map(f, sum)
map_df(f, sum) # return data.frame

# 모든 요소 + 1
map_df(f, ~.x+1)
modify(f, ~.x+1) # return 변수 자료형

# ==========================================
kbo <- read.csv('kbo.csv') %>% as_tibble()

# X10년대 : R은 변수이름이 숫자로 시작할 수 없기 때문에 자동으로 변수명 앞에 X가 붙음.
kbo %>% glimpse()

lm.model <- lm(타석당득점~타율, kbo) # 선형 회귀분석
summary(lm.model) # Multiple R-squared = 결정계수(R²)
summary(lm.model) %>% .$r.squared 

# purrr
# 1안.
kbo %>% 
  select(타율, 출루율, 장타력, OPS) %>% 
  map(~lm(kbo$타석당득점~.x)) %>% 
  map(summary) %>% 
  map_df('r.squared') 

# 2안.
kbo %>% 
  select(타율, 출루율, 장타력, OPS) %>% 
  map_df(~lm(kbo$타석당득점~.x) %>% 
           summary() %>% 
           .$r.squared) 
# ==========================================
# wide -> long

# 1안. 예전 방식
kbo %>% 
  gather(key=기록, value=값, 타율:OPS)

# 2안.
kbo2 <- kbo %>% 
  pivot_longer(cols=타율:OPS, names_to='기록', values_to='값') %>% 
# => cols에 선언한 변수들이 두개의 변수(name, value)로 묶임 
  group_by(기록) %>% 
  nest() # 중첩

kbo2

# 2-1.
kbo2$data %>% 
  set_names(., kbo2$기록) %>% 
  map_df(~lm(타석당득점~값, data=.) %>% 
           summary() %>% 
           .$r.squared)

# 2-2.
kbo2 %>% 
  mutate(model=map(data, ~lm(타석당득점~값, data=.) %>% 
                     summary() %>% 
                     .$r.squared)) %>% 
  unnest(model) %>% 
  select(-data)

# ============
kbo3 <- kbo %>% 
  pivot_longer(cols=타율:OPS, names_to='기록', values_to='값') %>% 
  group_by(X10년대, 기록) %>% 
  nest() %>% 
  mutate(model=map(data, ~lm(타석당득점~값, data=.) %>% 
                     summary() %>% 
                     .$r.squared)) %>% 
  unnest(model) %>% 
  select(-data)

# long -> wide
kbo3 %>% 
  # spread(key=기록, value=model) # 1안. 예전방식
  pivot_wider(names_from=기록, values_from=model) # 2안.

# ============
kbo3 %>% 
  ggplot(aes(x=X10년대, y=model, fill=기록)) +
  geom_bar(stat = 'identity', position = position_dodge2(reverse = T)) +
  scale_fill_viridis_d()

# ==========================================
# 국가별 평균 키 데이터

height <- read_csv('http://ncdrisc.org/downloads/height/NCD_RisC_eLife_2016_height_age18_countries.csv')

glimpse(height)

height %>% 
  map_df(~(tibble(class=class(.x),
                  count=n_distinct(.x))),
         .id='variable')
# => 101년 동안 200개 나라 남녀별 평균 키


# 태어난 연도와 평균 키 사이에 어떤 관계가 있을까?
height %>% 
  group_by(Country, Sex) %>% 
  nest() %>% 
  mutate(model=map_dbl(data, ~lm(`Mean height (cm)`~`Year of birth`, data=.) %>% 
                         summary() %>% 
                         .$r.squared)) # %>% 
  # system.time()

# => 위 코드를 함수화
height_model <- function(x) {
  lm(`Mean height (cm)`~`Year of birth`, data=x) %>% 
    summary() %>% 
    .$r.squared
}

# 어떤 나라 어떤 성별이 R²가 제일 높을까?
height %>% 
  group_by(Country, Sex) %>% 
  nest() %>% 
  mutate(model=map_dbl(data, height_model)) %>% 
  arrange(-model)

# 북한, 한국 남성의 평균 키가 어떻게 변했을까?
height %>% 
  filter(Country %in% c('North Korea', 'South Korea') & Sex == 'Men') %>% 
  ggplot(aes(x=`Year of birth`, y=`Mean height (cm)`, color=Country)) +
  geom_line()
# =================



# ==========================================
# ==========================================
# ==========================================
# ==========================================





