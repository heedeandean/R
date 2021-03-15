library(ggplot2) 
library(dplyr)
mpg <- as.data.frame(ggplot2::mpg)

# 1) 자동차 배기량에 따라 고속도로 연비가 다른지 알아보려고 합니다. 
#     displ(배기량)이 4이하인 자동차와 5이상인 자동차 중 
#     어떤 자동차의 hwy(고속도로 연비)가 평균적으로 더 높은지 알아보세요.

head(mpg)
m1 <- mpg %>% filter(displ <= 4)
mean(m1$hwy)

m2 <- mpg %>% filter(displ >= 5)
mean(m2$hwy)

# displ(배기량)이 5이상인 자동차 보다, 4이하인 자동차의 hwy(고속도로 연비)가 평균적으로 높다.


# 2) 자동차 제조 회사에 따라 도시 연비가 다른지 알아보려고 합니다.
#    "audi"와 "toyota"중 어느 manufacturer(자동차 제조 회사)의 
#     cty(도시 연비)가 평균적으로 더 높은지 알아보세요.

am <- mpg %>% filter(manufacturer == 'audi')
mean(am$cty)

tm <- mpg %>% filter(manufacturer == 'toyota')
mean(tm$cty)

# toyota의 cty 평균 > audi의 cty 평균


# 3) "chevrolet", "ford", "honda" 자동차의 고속도로 연비(hwy) 평균?

cfh_hwy <- mpg %>% filter(manufacturer %in% c("chevrolet", "ford", "honda"))
mean(cfh_hwy$hwy)


# 4) mpg 데이터에서 class(자동차 종류), cty(도시 연비) 변수를 추출하세요.

cc <- mpg %>% select(class, cty)


# 5) 4번에서 추출한 데이터를 이용해 class(자동차 종류)가 "suv"인 자동차와 "compact"인 자동차 중
#    어떤 자동차의 cty(도시 연비)가 더 높은지 알아보세요.

cs <- cc %>% filter(class == "suv")
mean(cs$cty)

ccom <- cc %>% filter(class == "compact")
mean(ccom$cty)

# suv의 cty 평균 < compact의 cty 평균


# 6) "audi"에서 생산한 자동차 중 hwy가 1~5위에 해당하는 자동차의 데이터를 출력하세요.

mpg %>% 
  filter(manufacturer == "audi") %>% 
  arrange(desc(hwy)) %>% 
  head(5)


# 7) mpg데이터 복사본을 만들고, cty와 hwy를 더한 '합산 연비 변수'를 추가하세요.

mpg2 <- mpg %>% mutate(chy_sum = cty + hwy)


# 8) 7번에서 만든 '합산 연비 변수'를 2로 나눠 '평균 연비 변수'를 추가하세요.

mpg2 <- mpg2 %>% mutate(chy_mean = chy_sum / 2)
head(mpg2)


# 9) '평균 연비 변수'가 가장 높은 자동차 3종의 데이터를 출력하세요.

mpg2 %>% arrange(desc(chy_mean)) %>% head(3)


# 10) 7~9번 문제를 해결할 수 있는 하나로 연결된 dplyr구문을 만들어보세요.
#   (데이터는 mpg 원본을 이용.)

mpg %>% 
  mutate(chy_sum = cty + hwy,
         chy_mean = chy_sum / 2) %>% 
  arrange(desc(chy_mean)) %>% 
  head(3)


# 11) 회사별로 "suv"자동차의 도시 및 고속도로 통합 연비 평균을 구해 내림차순으로 정렬하고,
#     1~5위까지 출력.

mpg %>% 
  group_by(manufacturer) %>% 
  filter(class == "suv") %>% 
  summarise(mean = mean(cty + hwy) / 2) %>% 
  arrange(desc(mean)) %>% 
  head(5)


# 12) 차종(class)별 도시연비(cty)평균을 구해보세요. (cty 평균이 높은 순으로 출력.)

mpg %>% 
  group_by(class) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty))


# 13) hwy평균이 가장 높은 회사 세 곳을 출력하세요.

mpg %>% 
  group_by(manufacturer) %>% 
  summarise(mean_hwy = mean(hwy)) %>% 
  arrange(desc(mean_hwy)) %>% 
  head(3)


# 14) 각 회사별 "compact"(경차) 차종 수를 내림차순으로 출력하세요.

mpg %>%  
  filter(class == "compact") %>% 
  group_by(manufacturer) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))


# 15) fuel 데이터를 이용해 mpg 데이터에 price_fl(연료 가격) 변수를 추가하세요.

fuel <- data.frame(fl = c("c", "d", "e", "p", "r"),
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22),
                   stringsAsFactors = F)
fuel
str(fuel)

mpg <- left_join(mpg, fuel, by = "fl")


# 16) 연료 가격 변수가 잘 추가됐는지 확인하기 위해
#     model, fl, price_fl 변수를 추출해 앞부분 5행을 출력해 보세요.

mpg %>% 
  select(model, fl, price_fl) %>% 
  head(5)


# -------------------------------------------------------------------------
# 결측치<NA>.

mpg[c(65, 124, 131, 153, 212), "hwy"] <- NA  # NA 할당.

# 17) drv(구동 방식) 변수와 hwy 변수에 결측치가 몇 개 있는지 알아보세요.

table(is.na(mpg$drv))
table(is.na(mpg$hwy))


# 18) filter()를 이용해 hwy 변수의 결측치를 제외하고, 어떤 drv(구동 방식)의 hwy 평균이 높은지 알아보세요.
#     (하나의 dplyr 구문으로 만들어야 함.)

mpg %>% 
  filter(!is.na(hwy)) %>% 
  group_by(drv) %>% 
  summarise(mean_hwy = mean(hwy))

# drv 중 f의 hwy 평균이 가장 높습니다.


# -------------------------------------------------------------------------
mpg <- as.data.frame(ggplot2::mpg)

# 이상치 만들기.

mpg[c(10, 14, 58, 93), "drv"] <- "k"
mpg[c(29, 43, 129, 203), "cty"] <- c(3, 4, 39, 42)

# 19) drv에 이상치가 있는지 확인하세요. 이상치를 결측 처리한 후 이상치가 사라졌는지 확인하세요.
#     결측 처리를 할 때는 %in% 기호를 활용하세요.

table(mpg$drv) # 이상치 확인.

mpg$drv <- ifelse(mpg$drv %in% c('4', 'f', 'r'), mpg$drv, NA)

mpg %>% filter(is.na(drv))

table(mpg$drv) # 이상치 확인.


# 20) 상자 그림을 이용해 cty에 이상치가 있는지 확인하세요.
#     상자 그림의 통계치를 이용해 정상 범위를 벗어난 값을 결측 처리한 후 
#     다시 상자 그림을 만들어 이상치가 사라졌는지 확인하세요.

boxplot(mpg$cty)
boxplot(mpg$cty)$stats

mpg$cty <- ifelse(mpg$cty < 9 | mpg$cty > 26, NA, mpg$cty)

boxplot(mpg$cty)


# 21) 이상치를 제외한 다음 drv별로 cty 평균이 어떻게 다른지 알아보세요.
#     (하나의 dplyr 구문으로 만들어야 함.)

mpg %>% 
  filter(!is.na(drv) & !is.na(cty)) %>% 
  group_by(drv) %>% 
  summarise(mean_cty = mean(cty))










