# 1. 네트워크 데이터
# node: 객체
# edge: node(객체)들 간 관계

library(dplyr)
#install.packages('tidygraph', type='binary')
library(tidygraph)

# tibble: 기존 data frame을 개선한 데이터 구조.
edge.data <- tibble(from=c(1,2,2,3,4,4), to=c(2,3,4,2,1,5))
edge.data
node.data <- tibble(id=1:5) 
node.data
net.1 <- tbl_graph(nodes = node.data, edges = edge.data, directed = TRUE) # 방향(시작점, 끝점) 정보 저장.
net.1

#install.packages('ggraph', type='binary')
library(ggraph)
class(highschool)
head(highschool)
net.hi <- as_tbl_graph(highschool) # tibble 구조 변환

iris.clust <- hclust(dist(iris[,1:4])) # 계층적 군집화(hierarchical clustering) => 덴드로그램: 객체 간의 계층적 관계를 나타낸 그림.
net.iris <- as_tbl_graph(iris.clust)
net.iris

ggraph(net.1) + # *default: layout='stress'
  geom_edge_link() + # 직선
  geom_node_point() # 점 / 생략 가능

ggraph(net.hi) + geom_edge_link() + geom_node_point() + theme_graph() 
ggraph(net.hi, layout='eigen') + # 방향성 X
  geom_edge_link() + geom_node_point() + theme_graph()
ggraph(net.hi, layout='linear', circular=TRUE) + geom_edge_link() + geom_node_point() + coord_fixed() # 동그란 원

set.seed(100)
ggraph(net.hi, layout='circlepack') + # 중심 노드(랜덤)에 따라 다른 시각화 결과.
  geom_edge_link() + geom_node_point() 
ggraph(net.iris, layout='dendrogram') + geom_edge_link() + geom_node_point() 
ggraph(net.iris, layout='dendrogram', height=height) + geom_edge_link() + geom_node_point() # 군집 간 거리 반영.

# 1-1. node
ggraph(net.1) + geom_edge_link() + geom_node_point(color='red', size=4) + theme_graph() 
ggraph(net.1) + geom_edge_link() + geom_node_point(aes(color=centrality_power()), size=4) + # 중심성 측도 함수: centrality_pagerank(), centrality_degree()  
  labs(color='Centrality') + theme_graph() 
ggraph(net.1) + geom_edge_link() + geom_node_label(aes(label=id)) + theme_graph() # 네모

ggraph(net.hi, layout='stress') + geom_edge_link() + geom_node_point(aes(filter=centrality_degree() > 3), color='red') 
ggraph(net.iris, layout='dendrogram', circular=TRUE) + geom_edge_link() + 
  geom_node_point(aes(filter=leaf)) + # 맨 하단 terminal node만 표시
  coord_fixed() 
ggraph(net.iris, layout='dendrogram', height=height) + geom_edge_link() # node X

# 1-2. edge
ggraph(net.1) + 
  geom_edge_link(arrow=arrow(), 
                 start_cap=circle(3, 'mm'), end_cap=circle(3, 'mm')) # 화살표의 시작점, 끝점을 노드로부터 떨어지게 함.
+ geom_node_point(size=4) 

edge.data2 <- tibble(from=c(1,2,2,3,4,4), to=c(2,3,4,2,1,5),
                     amount=c(100,40,50,60,80,30))
edge.data2

net.2 <- tbl_graph(nodes = node.data, edges = edge.data2, directed = TRUE) 
net.2

ggraph(net.2) + 
  geom_edge_parallel(aes(edge_width=amount), arrow=arrow(), start_cap=circle(5, 'mm'), end_cap=circle(5, 'mm')) + 
  geom_node_label(aes(label=id)) +
  scale_edge_width(range=c(0.2, 2)) + theme_graph()

net.hi2 <- net.hi %>% 
  activate(edges) %>% # tibble: 수정하려는 것이 에지 or 노드 데이터인지 선언 ⭐️
  mutate(year=as.factor(year))
ggraph(net.hi2) + geom_edge_link(aes(color=year)) + geom_node_point()

# edge가 곂쳐지지 않게 함.
ggraph(net.hi2) + geom_edge_fan(aes(color=year)) + # 부챗살
  geom_node_point() 
ggraph(net.hi2, layout='linear') + geom_edge_arc(aes(color=year))  # 아치형 곡선
  
ggraph(net.iris, layout='dendrogram', height=height) + geom_edge_elbow() # 직각