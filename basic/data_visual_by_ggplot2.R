library(tidyverse)

# 데이터 시각화
mtcars1 <- as_tibble(rownames_to_column(mtcars))
mtcars1

# 결과는 동일
ggplot(mtcars1, aes(hp, mpg, color=factor(cyl))) + geom_point()
ggplot(mtcars1) + geom_point(aes(hp, mpg, color=factor(cyl)))
ggplot(mtcars1, aes(hp, mpg)) + geom_point(aes(color=factor(cyl)))

ggplot(mtcars1, aes(hp, mpg, color=factor(cyl))) + geom_point() +
  geom_smooth(aes(group=123), se=FALSE) # 비선형 곡선
       
ggplot(mtcars1, aes(hp, mpg, color=factor(cyl))) + geom_point() +
  labs(title='Relationship between Fuel efficiency and Horse power', x='Horse Power', y='Mile per Gallon', caption='Data Source: R', color='Cylinder')

ggplot(mtcars1, aes(hp, mpg, color=factor(cyl))) + geom_point() +
  labs(title='Relationship between Fuel efficiency and Horse power', x='Horse Power', y='Mile per Gallon', caption='Data Source: R', color='Cylinder') +
  geom_text(aes(label=rowname), show.legend = FALSE)

ggplot(mtcars1, aes(hp, mpg, color=factor(cyl))) + geom_point() +
  labs(title='Relationship between Fuel efficiency and Horse power', x='Horse Power', y='Mile per Gallon', caption='Data Source: R', color='Cylinder') +
  geom_label(aes(label=rowname), nudge_x=2.5, nudge_y=1.5, label.size=0.25, show.legend = FALSE) # 상자 표시


hmpg <- mtcars1 %>% group_by(cyl) %>% filter(row_number(desc(mpg))==1)
hmpg

ggplot(mtcars1, aes(hp, mpg, color=factor(cyl))) + geom_point() +
  labs(title='Relationship between Fuel efficiency and Horse power', x='Horse Power', y='Mile per Gallon', caption='Data Source: R', color='Cylinder') +
  geom_label(aes(label=rowname), data=hmpg, nudge_x=2.5, nudge_y=1.5, label.size=0.25, show.legend = FALSE) # 상자 표시

ggplot(mtcars1, aes(hp, mpg, color=factor(cyl))) + geom_point() +
  labs(title='Relationship between Fuel efficiency and Horse power', x='Horse Power', y='Mile per Gallon', caption='Data Source: R', color='Cylinder') +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL) +
  scale_color_discrete(labels = NULL)

ggplot(mtcars1, aes(hp, mpg, color=factor(cyl))) + geom_point() +
  labs(title='Relationship between Fuel efficiency and Horse power', x='Horse Power', y='Mile per Gallon', caption='Data Source: R', color='Cylinder') +
  scale_y_continuous(breaks=seq(10, 40, by=10))

x_axis <- scale_x_continuous(limits = range(mtcars1$hp))
y_axis <- scale_y_continuous(limits = range(mtcars1$mpg))
x_axis
y_axis

col_legend <- scale_colour_discrete(limits=unique(factor(mtcars1$cyl)))
col_legend

ggplot(mtcars1, aes(hp, mpg, color=factor(cyl))) + geom_point() +
  geom_smooth(aes(group=123), se=FALSE) + # 비선형 곡선 
  labs(title='Relationship between Fuel efficiency and Horse power', x='Horse Power', y='Mile per Gallon', caption='Data Source: R', color='Cylinder') +
  coord_cartesian(xlim = c(100,250), ylim = c(10,30)) # 일부만 선택; 전체 관측치 중 일부 범위를 자세히 확대(zooming) 출력
  
ggplot(mtcars1, aes(hp, mpg, color=factor(cyl))) + geom_point() +
  labs(title='Relationship between Fuel efficiency and Horse power', x='Horse Power', y='Mile per Gallon', caption='Data Source: R', color='Cylinder') +
  theme(legend.position = 'top') # 범례 위치 조정

ggplot(mtcars1, aes(hp, mpg, color=factor(cyl))) + geom_point() +
  labs(title='Relationship between Fuel efficiency and Horse power', x='Horse Power', y='Mile per Gallon', caption='Data Source: R', color='Cylinder') +
  theme(legend.position = 'bottom') +
  guides(color=guide_legend(nrow = 3, override.aes = list(size = 5)))

ggplot(mtcars1, aes(hp, mpg, color=factor(cyl))) + geom_point() +
  labs(title='Relationship between Fuel efficiency and Horse power', x='Horse Power', y='Mile per Gallon', caption='Data Source: R', color='Cylinder') +
  guides(color=guide_legend(nrow = 3, override.aes = list(size = 5))) + 
  theme_classic() + 
  theme(legend.position = 'bottom') 

getwd()
ggsave('result.pdf') # 마지막 그래프 저장