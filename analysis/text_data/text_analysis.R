#rm(list = ls()) # 변수 초기화
#dev.off() # plot창 초기화

#install.packages("tm")
library(tm)

#install.packages("XML")
library('XML')

#install.packages("tm.corpus.Reuters21578", repos = "http://datacube.wu.ac.at")
library(tm.corpus.Reuters21578)

data("Reuters21578")
Reuters21578

Reut_lists <- lapply(Reuters21578, FUN = unlist)
class(Reut_lists)
names(Reut_lists[[1]])

Reut_content <- lapply(Reut_lists, function(x) x[names(x) == 'content']) # 기사 내용
class(Reut_content)

Reut_topics <- lapply(Reut_lists, function(x) x[names(x) == 'meta.topics_cat']) # 메타데이터: 주제 범주

sort(table(unlist(Reut_topics)), decreasing = T)

Reut_content <- Reut_content[Reut_topics=='money-fx' | Reut_topics=='interest']
Reut_topics <- Reut_topics[Reut_topics=='money-fx' | Reut_topics=='interest']

# 텍스트 데이터 전처리
Reut_content[1]
Reut_content <- gsub(Reut_content, pattern='\n', replacement=' ')
Reut_content <- gsub(Reut_content, pattern="'s", replacement='')
Reut_content <- gsub(Reut_content, pattern="([^[:alnum:][:blank:]'-])", replacement='')
Reut_content <- tolower(Reut_content)
Reut_content <- strsplit(Reut_content, ' ')
Reut_content

which(Reut_content == 'character0')
Reut_topics <- Reut_topics[Reut_content != 'character0']
Reut_content <- Reut_content[Reut_content != 'character0']

library(stopwords) # 불용어
Reut_content <- lapply(Reut_content, function(x) x[! x %in% c(stopwords(), '')])
Reut_content <- lapply(Reut_content, function(x) gsub(x, pattern="'", replacement=''))

library(textstem)
Reut_content <- lapply(Reut_content, lemmatize_strings)

# 문서-단어 행렬
Reut_lev <- sort(unique(unlist(Reut_content)))
Reut_DTM <- lapply(Reut_content, function(x, lev) {table(factor(x, lev, ordered = T))}, lev=Reut_lev)
Reut_DTM <- matrix(unlist(Reut_DTM), nrow = length(Reut_DTM), byrow = T)

dim(Reut_DTM)
sum(Reut_DTM>0)
sum(Reut_DTM==0)
1-sum(Reut_DTM>0)/sum(Reut_DTM>=0) # 희소도

colnames(Reut_DTM) <- Reut_lev
Reut_DTM

########################
# 탐색적 자료 분석 
# (전체)
# 1. 도수분포표
Reut_table <- sort(table(unlist(Reut_content)), decreasing=T) 
Reut_table

# 2. 시각화
barplot(Reut_table[1:32]) # 막대그래프

#install.packages('wordcloud')
library(wordcloud)
wordcloud(words=names(Reut_table), freq = Reut_table, 
          max.words = 200, # 보여줄 단어 수
          random.order = F)


# (주제별)
Reut_fx <- Reut_content[Reut_topics == 'money-fx']
Reut_int <- Reut_content[Reut_topics == 'interest']
Reut_fx_tab <- table(factor(unlist(Reut_fx), levels = Reut_lev, ordered = T))
Reut_int_tab <- table(factor(unlist(Reut_int), levels = Reut_lev, ordered = T))

Reut_fx_int <- sort(Reut_fx_tab-Reut_int_tab)
Reut_fx_int <- Reut_fx_int[abs(Reut_fx_int)>=60]
Reut_fx_int

barplot(Reut_fx_int) # 주제별 출현 빈도

Reut_fx_int_mat <- cbind(Reut_fx_tab, Reut_int_tab)
commonality.cloud(Reut_fx_int_mat, max.words = 200, random.order = F) # 공통 단어 O
comparison.cloud(Reut_fx_int_mat, max.words = 200, random.order = F,
                 colors = c('grey', 'black')) # 공통 단어 X
Reut_fx_int_mat <- cbind(Reut_fx_int_mat, Reut_fx_int_mat[,1]-Reut_fx_int_mat[,2])

head(Reut_fx_int_mat)
Reut_fx_int_col <- ifelse(Reut_fx_int_mat[,3]>=60, 'red', 
                          ifelse(Reut_fx_int_mat[,3]<=-60, 'blue', 'grey'))
class(Reut_fx_int_col) # character

# 산점도(x축: 외환, y축: 금리)
plot(Reut_fx_int_mat[,1], Reut_fx_int_mat[,2], type='n', xlab='', ylab='')
text(Reut_fx_int_mat[,1], Reut_fx_int_mat[,2], 
     row.names(Reut_fx_int_mat), col = Reut_fx_int_col)

# 외환, 금리 순으로 재정렬
Reut_topics <- Reut_topics[c(which(Reut_topics=='money-fx'), which(Reut_topics=='interest'))] # list
Reut_content <- Reut_content[c(which(Reut_topics=='money-fx'), which(Reut_topics=='interest'))] # list

# 문서-단어 행렬
Reut_DTM <- lapply(Reut_content, function(x, lev) {table(factor(x, lev, ordered = T))}, lev=Reut_lev)
Reut_DTM <- matrix(unlist(Reut_DTM), nrow = length(Reut_DTM), byrow = T)
class(Reut_DTM)

colnames(Reut_DTM) <- Reut_lev
Reut_DTM


# 코사인 유사도 행렬 - 기사들 간 유사성
Reut_DTMsqr <- Reut_DTM %*% t(Reut_DTM) # 각 기사 벡터간 내적
Reut_DTMsqr
Reut_CosSim <- Reut_DTMsqr / sqrt(diag(Reut_DTMsqr) %*% t(diag(Reut_DTMsqr))) 
class(Reut_CosSim)
dim(Reut_CosSim) # 470X470 대칭행렬

set.seed(1)

# 각 20건 기사 랜덤 추출
FXsample <- sort(sample.int(n=sum(Reut_topics=='money-fx'), # 259
                            size=20)) 
FXsample
INTsample <- sort(sample.int(n=sum(Reut_topics=='interest'), # 211
                             size=20))
smpl <- c(FXsample, INTsample+sum(Reut_topics=='money-fx'))

library(corrplot)
corrplot(Reut_CosSim[smpl, smpl])


##################################
# 데이터 마이닝
# 1. 계층적 군집 분석(완전 연결법(기본값)) - 응집분석
# 1-1. 유클리드 거리
Reut_euclidean <- hclust(dist(Reut_DTM[smpl,]))
class(Reut_euclidean)

Reut_euclidean$labels <- c(paste('FX', c(1:20)), paste('INnt', c(1:20)))
plot(Reut_euclidean, main='Cluster Dendrogram - Eucldean Distance', xlab='', sub='') # 군집화가 잘 이루어지지 않음.

# 1-2. 코사인 비유사성 행렬
Reut_clusters <- hclust(as.dist(1-Reut_CosSim[smpl, smpl])) # 타입을 dist로 변환
Reut_clusters$labels <- c(paste('FX', c(1:20)), paste('INnt', c(1:20)))
plot(Reut_clusters, main='Cluster Dendrogram - Cosine Dissimilarity', xlab='', sub='') 


# 2. 분류 분석
head(Reut_fx_int_mat)
Reut_selected <- Reut_fx_int_mat[abs(Reut_fx_int_mat[,1] - Reut_fx_int_mat[,2]) >= 60, ]
rownames(Reut_selected)

Reut_selected_DTM <- Reut_DTM[, colnames(Reut_DTM) %in% rownames(Reut_selected)]

library(rpart)
ctrl <- rpart.control(minsplit = 30, 
                      cp = -0.01, # 비용-복잡 함수가 최소가 되어도 계속 분할
                      xval = 10) # 10겹 교차검증
fit_tree <- rpart(unlist(Reut_topics)~., data = data.frame(Reut_selected_DTM), 
                  method = 'class', # 분류나무모형
                  control = ctrl)
plot(fit_tree)
text(fit_tree)
fit_tree # 총평: 별로임

# 가지치기
prune_tree <- prune(fit_tree, cp=0) # 필요 이상으로 분활된 노드들 정리

length(fit_tree$frame$var)
sum(fit_tree$frame$var == '<leaf>')

length(prune_tree$frame$var)
sum(prune_tree$frame$var == '<leaf>')

plot(prune_tree)
text(prune_tree)
prune_tree

# 결과
pred <- predict(prune_tree, type = 'class')
pred
class(pred) # factor
table(pred)

# 정오 분류표(confusion matrix)
confmat <- table(unlist(Reut_topics), pred)
confmat

# 분류모형 성능 평가지표
# > 정확도(precision): 전체 중 정확히 예측.
# > 민감도(sensitivity): 실제로 양(+)인 개체
# > 특이도(specificity): 실제로 음(-)인 개체