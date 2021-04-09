library(dplyr)
library(ggplot2)

# 순서
# 1. Data: 시각화에 사용될 데이터
# 2. Aesthetics: 데이터를 나타내는 시각적인 요소(x축, y축, 사이즈, 색깔, 모양 등)
# 3. Geometrics: 데이터를 나타내는 도형
# 4. Facets: 하위 집합으로 분할하여 시각화
# 5. Statistics: 통계값을 표현
# 6. Coordinates: 데이터를 표현 할 이차원 좌표계
# 7. Theme: 그래프를 꾸밈

data(diamonds)
head(diamonds)

# carat별 price를 산점도로.
diamonds %>% 
  ggplot(aes(x=carat, y=price)) +  # Aesthetics
  geom_point() # Geometrics

# carat 별 price를, bar graph로
diamonds %>% 
  ggplot(aes(x=carat, y=price)) +  # Aesthetics
  geom_bar(stat = 'identity') # Geometrics

# carat 별 count를, bar graph로.
diamonds %>% 
  ggplot(aes(x=carat)) +  # Aesthetics
  geom_bar(stat = 'count') # Geometrics

# carat별 price를, 색이 cut인 산점도로.
diamonds %>% 
  ggplot(aes(x=carat, y=price)) +  # Aesthetics
  geom_point(aes(color=cut)) # Geometrics

# carat별 price를, 색이 cut이고 size가 table인 산점도로.
diamonds %>% 
  ggplot(aes(x=carat, y=price)) +  # Aesthetics
  geom_point(aes(color=cut, size=table)) # Geometrics


# carat별 price를, group이 cut인 boxplot으로
diamonds %>% 
  ggplot(aes(x=carat, y=price)) +  # Aesthetics
  geom_boxplot(aes(group=cut, color=cut)) # Geometrics

# carat별 price를, 산점도로, cut별로 가로로 세분화.
diamonds %>% 
  ggplot(aes(x=carat, y=price)) +  # Aesthetics
  geom_point() + # Geometrics
  facet_grid(~cut)
  
# carat별 price를, 산점도로, cut과 color로 가로로 세분화
diamonds %>% 
  ggplot(aes(x=carat, y=price)) +  # Aesthetics
  geom_point() + # Geometrics
  facet_grid(~cut + color)

# carat별 price를, 산점도로, cut별로 가로세로 세분화
diamonds %>% 
  ggplot(aes(x=carat, y=price)) +  # Aesthetics
  geom_point() + # Geometrics
  facet_wrap(~cut)

# cut별 carat을, y축이 평균값인 bar graph로.
# 1안
diamonds %>% 
  ggplot(aes(x=cut, y=carat)) +  # Aesthetics
  stat_summary_bin(fun='mean', geom='bar')

# 2안
diamonds %>% 
  group_by(cut) %>% 
  summarise(carat_mean = mean(carat)) %>% 
  ggplot(aes(x=cut, y=carat_mean, fill=cut)) + 
  geom_bar(stat="identity")
  

# cut별 price를, y축이 평균값인 bar graph로.
diamonds %>% 
  ggplot(aes(x=cut, y=price)) +  # Aesthetics
  stat_summary_bin(fun='mean', geom='bar')


# carat별 price를, color가 cut인 산점도로, x축은 0~3, y축은 0~20000
diamonds %>% 
  ggplot(aes(x=carat, y=price)) +  # Aesthetics
  geom_point(aes(color=cut)) + # Geometrics
  coord_cartesian(xlim=c(0, 3), ylim=c(0, 20000))


# carat별 price를, group이 cut인 boxplot으로, 90도 돌려서표현.
diamonds %>% 
  ggplot(aes(x=carat, y=price)) +  # Aesthetics
  geom_boxplot(aes(group=cut)) +
  coord_flip()

# carat별 price를, color가 cut인 산점도,배경은흰색, title은'carat과 price의 관계 x축은'carat'y축은'price', legend는 아래로, y축값에'$'와천원단위로','추가해서시각화.
diamonds %>% 
  ggplot(aes(x=carat, y=price)) +  # Aesthetics
  geom_point(aes(color=cut)) +
  theme_bw() +
  labs(title="carat과 price의 관계", x="carat", y="price") +
  theme(legend.position = 'bottom') + # 'none' : legend 없앰
  scale_y_continuous(
    labels = function(x)
      paste0('$', format(x, big.mark=','))
  )
  
# ==================================================

# carat별 price를 산점도로.
ggplot(data=diamonds, aes(x=carat, y=price)) + 
  geom_point()

# carat 별 price를, bar graph로
diamonds %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_bar(stat = 'identity')
# carat 별 count를, bar graph로.
diamonds %>% 
  ggplot(aes(x = carat)) +
  geom_bar()

# carat별 price를, 색이 cut인 산점도로.
diamonds %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point(aes(color = cut))

# carat별 price를, 색이 cut이고 size가 table인 산점도로.
diamonds %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point(aes(color = cut, size = table))

# box plot, catat 별 price, 단 cut 을 group으로 묶음.
# carat별 price를, group이 cut인 boxplot으로
diamonds %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut))

# 4. Facets
# cut 별로 가로축으로 분리
# carat별 price를, 산점도로, cut별로 가로로 세분화.
diamonds %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  facet_grid(.~ cut)

# cut, color 별로 가로축으로 분리
# carat별 price를, 산점도로, cut과 color로 가로로 세분화
diamonds %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  facet_grid(.~ cut+color)

# carat별 price를, 산점도로, cut별로 가로세로 세분화
diamonds %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  facet_wrap(.~ cut)

# 5. Statistics
# cut별 carat을, y축이 평균값인 bar graph로.
diamonds %>% 
  ggplot(aes(x = cut, y = carat)) +
  stat_summary_bin(fun = 'mean', geom = 'bar')

# cut별 price를, y축이 평균값인 bar graph로.
diamonds %>% 
  ggplot(aes(x = cut, y = price)) +
  stat_summary_bin(fun = 'mean', geom = 'bar')

# 6. Coordinates (좌표)
# x축 0~3, y축 0~20000까지 표시
# carat별 price를, color가 cut인 산점도로, x축은 0~3, y축은 0~20000
diamonds %>% 
  ggplot(aes(x= carat, y = price)) +
  geom_point(aes(color=cut)) +
  coord_cartesian(xlim = c(0, 3), ylim = c(0, 20000))

# carat별 price를, group이 cut인 boxplot으로, 90도 돌려서표현.
diamonds %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut)) +
  coord_flip()

# 7. Theme
# carat별 price를, color가 cut인 산점도로,
# 배경은 흰색,
# title은 'carat과 price의 관계, x축은 'carat', y축은 'price'
# legend는 아래로, 패널의 grid는 없애고,
# y축값에 '$'와 천원단위로 ',' 추가해서 시각화.
diamonds %>% 
  ggplot(aes(x = carat, y = price)) +
  geom_point(aes(color = cut)) +
  theme_bw() +
  labs(title = 'carat과 price의 관계',
       x = 'carat', y = 'price') +
  theme(legend.position = 'bottom',
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
        # legend를 아래로, 그리고 사각형 grid 제거
  ) +
  scale_y_continuous(
    labels = function(x) {
      paste0('$', format(x, big.mark = ','))
      # y축에 천원단위로 콤마(,)추가, '$'표시와 합쳐서표현
    }
  )
