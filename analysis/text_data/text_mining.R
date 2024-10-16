# 코사인 유사도: 문서간 유사성, 두 문서 벡터 사이의 각도.
# > 0~1: 값이 클 수록 유사도가 높은 것
RC <- scan("http://www.gutenberg.org/files/521/521-0.txt",
           what = 'character', encoding = 'UTF-8', sep='\n') # 로빈슨 크루소
LW <- scan("http://www.gutenberg.org/cache/epub/514/pg514.txt",
           what = 'character', encoding = 'UTF-8', sep='\n') # 작은 아씨들

RC
LW

RC_Chpt <- grep(RC, pattern = 'CHAPTER') 
LW_Chpt <- grep(LW, pattern = 'CHAPTER') 
LW_Chpt

RC_End <- grep(tolower(RC), pattern='end of the project gutenberg')-1
LW_End <- grep(tolower(LW), pattern='end of the project gutenberg')-1
LW_End

RC_body <- RC[(RC_Chpt[21]):RC_End]
LW_body <- LW[(LW_Chpt[48]):LW_End]
RCLW_body <- c(RC_body, LW_body)

RCLW_by_Chpt <- unlist(strsplit(paste(RCLW_body, collapse=' '), 'CHAPTER'))[-1] 
class(RCLW_by_Chpt) # character

# 전처리
RCLW_by_Chpt <- gsub(x=RCLW_by_Chpt, pattern = "'s", replacement = '')
RCLW_by_Chpt <- gsub(x=RCLW_by_Chpt, pattern = "([^[:alnum:][:blank:]'-])", 
                     replacement = '') # 문장부호 삭제
RCLW_by_Chpt <- tolower(RCLW_by_Chpt)
RCLW_by_Chpt <- strsplit(RCLW_by_Chpt, ' ') # 토큰화, list

#install.packages('stopwords')
library(stopwords) # 불용어
RCLW_by_Chpt <- lapply(RCLW_by_Chpt, function(x) x[! x %in% c(stopwords(), '')])


#install.packages('textstem')
library(textstem) # 원형복원
RCLW_by_Chpt <- lapply(RCLW_by_Chpt, lemmatize_strings)

RCLW_lev <- sort(unique(unlist(RCLW_by_Chpt))) # 중복 단어 제거

# 문서-단어 행렬
RCLW_DTM <- lapply(RCLW_by_Chpt, FUN=function(x, lev) {table(factor(x, lev, ordered = T))}, lev=RCLW_lev)
class(RCLW_DTM) # list
RCLW_DTM <- matrix(unlist(RCLW_DTM), nrow = length(RCLW_DTM), byrow = TRUE) 
RCLW_DTM
class(RCLW_DTM)

dim(RCLW_DTM>0) # 문서, 단어
sum(RCLW_DTM>0)
sum(RCLW_DTM==0) # 희소 행렬: 90% 이상의 셀이 0으로 채워짐.

RCLW_DTMsqr <- RCLW_DTM %*% t(RCLW_DTM)
RCLW_DTMsqr

# 코사인 유사도
RCLW_CosSim <- RCLW_DTMsqr / sqrt(diag(RCLW_DTMsqr) %*% t(diag(RCLW_DTMsqr))) 
set.seed(1)
RCsample <- sort(sample.int(n=20, size=5))
RCsample
LWsample <- sort(sample.int(n=47, size=5))
LWsample
RCLW_CosSim_smpl <- RCLW_CosSim[c(RCsample, LWsample+20), c(RCsample, LWsample+20)]

rownames(RCLW_CosSim_smpl) <- c(paste0('Robinson', RCsample), paste0('Women', LWsample))
colnames(RCLW_CosSim_smpl) <- c(paste0('Robinson', RCsample), paste0('Women', LWsample))
RCLW_CosSim_smpl

# 시각화
#install.packages('corrplot')
library(corrplot)
corrplot(RCLW_CosSim_smpl)

## 텍스트 마이닝
# 1. 군집 분석
# > 목표 변수 X, 비지도 학습(unsupervised learning)

# 1-1. k-평균 군집 분석(k-means clustering)
# > 사전에 군집 수 지정 O
# > 개체 수 많아도 ok

# 1-2. 계층적 군집 분석(hierarchical clustering)
# > 사전에 군집 수 지정 X
# > 개체 수가 많으면, 시간이 오래 걸림.(-)
# 1-2-1. 분할분석(divisice clustering): 1개의 군집 => N개 세분화.
# 1-2-2. 응집분석(agglomerative clustering): N개 => 1개의 군집으로 응집화.
# > 1-2-2-1. 군집화 기준
# > 1) 코사인 비유사성 행렬 기준
# > 완전연결법(complete linkage method)
RCLW_clusters <- hclust(as.dist(1-RCLW_CosSim)) 
RCLW_clusters$labels <- c(paste('Robinson', c(1:20)), paste('Women', c(1:47)))
RCLW_clusters
plot(RCLW_clusters) # 덴드로그램

# > 단일연결법
RCLW_single <- hclust(as.dist(1-RCLW_CosSim), method='single') 
RCLW_single$labels <- c(paste('Robinson', c(1:20)), paste('Women', c(1:47)))
plot(RCLW_single, main = 'Cluster Dendrogram - Single', xlab = ' ', sub = ' ') 

# > 평균연결법
RCLW_average <- hclust(as.dist(1-RCLW_CosSim), method='average') 
RCLW_average$labels <- c(paste('Robinson', c(1:20)), paste('Women', c(1:47)))
plot(RCLW_average, main = 'Cluster Dendrogram - Average', xlab = ' ', sub = ' ') 

# > 2) 유클리드 거리 기준
# > 절대도수: 단일연결법
RCLW_euclidean <- hclust(dist(RCLW_DTM)) 
RCLW_euclidean$labels <- c(paste('Robinson', c(1:20)), paste('Women', c(1:47)))
plot(RCLW_euclidean, main = 'Cluster Dendrogram - Euclidean Distance', xlab = ' ', sub = ' ') 

# > 단어 수가 많으면, 유사성이 낮게 판단됨.
barplot(unlist(lapply(RCLW_by_Chpt, FUN = length)))

# > 상대도수 행렬: 절대도수보다 나음, 코사인 비유사성과 비슷해짐.
RCLW_relfreq <- hclust(dist(RCLW_DTM/rowSums(RCLW_DTM))) # 행별 단어 수의 합계로 나눔.
RCLW_relfreq$labels <- c(paste('Robinson', c(1:20)), paste('Women', c(1:47)))
plot(RCLW_relfreq, xlab = '', sub = '') 


# 2. 분류(classification) 분석
# > 목표 변수 O, 지도 학습(supervised learning)
# > 분류모형: 목표변수가 범주형(categorical)
# > 통계적 기법: 로지스틱 회귀모형, 분류나무모형, 인공신경망 모형, SVM(support vector machine)
# > 분류나무모형
# : 전체 1개의 집단에서 불순도(impurity)를 낮추는 변수를 찾아 나가며 분할 과정 반복.
# : 직관적, 여러 변수의 상호작용 파악 용이.

RCLW_DTM[1:20, RCLW_lev == 'jo'] 
RCLW_DTM[21:67, RCLW_lev == 'jo'] 

RCLW_DTMs <- rbind(colSums(RCLW_DTM[1:20,]), colSums(RCLW_DTM[21:67,]))
sum(RCLW_DTMs[1,]>0 & RCLW_DTMs[2,]>0) # 두 소설에 공통으로 등장한 단어

sample_words <- sample(which(RCLW_DTMs[1,]*RCLW_DTMs[2,]>0), 25) # 공통 단어 25개 랜덤 추출
RCLW_DTMs[1:2, sort(sample_words)]

RCLW_DTM_smpl <- RCLW_DTM[,sample_words]
colnames(RCLW_DTM_smpl) <- RCLW_lev[sample_words]
head(RCLW_DTM_smpl)
RCLW_target <- c(rep('Robin',20), rep('Women',47))

library(rpart)
ctrl <- rpart.control(minsplit = 5, cp=-0.01, xval = 10) # 제약조건
fit_tree <- rpart(RCLW_target~., # 목표변수~설명변수 
                  data = data.frame(RCLW_DTM_smpl), 
                  method = 'class', # 분류나무모형 
                  control = ctrl)
fit_tree
plot(fit_tree)
text(fit_tree)

prune_tree <- prune(fit_tree, cp=0)
prune_tree
plot(prune_tree, margin=0.1)
text(prune_tree, cex=2)