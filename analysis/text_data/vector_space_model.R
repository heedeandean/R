# 🍎 벡터 공간 모형(Vector Space Model): 텍스트 데이터를 벡터 공간상의 한점인 벡터로 표현 => 텍스트 데이터 수치화

# 1. 문서-단어 행렬(Document-Term Matrix, DTM)
# 행: 문서, 열: 단어
# 단어 출현 빈도만 이용 => 문법, 출현 순서 정보 사라짐

library(textstem)
x <- c("the best theater in New_York", "the best hotel in New_York", "the best gift for kids")

# 전처리
x <- tolower(x)
x <- gsub(x, pattern="([^[:alnum:][:blank:]'-])", replacement='') # 문장부호 삭제
x <- lemmatize_strings(x) # 원형 복원 

bows <- strsplit(x, ' ') # bag of words
bows
mode(bows)

lev <- sort(unique(unlist(bows)))
lev

DTM <- lapply(bows, 
              FUN=function(y, lev) {table(factor(y, lev, ordered = T))},
              lev=lev)
mode(DTM)

DTM <- matrix(unlist(DTM), nrow = length(DTM), byrow = T)
mode(DTM)
colnames(DTM) <- lev
rownames(DTM) <- paste('doc', 1:dim(DTM)[1], sep='')
DTM


# 2. 단어빈도-역문서빈도(Term Frequency-Inverse Document Frequency, TF-IDF)
# = 단어빈도(TF) X  문서빈도(DF)의 역수(IDF)
# 문서-단어 행렬의 가중치 조정
# 불용어 삭제 과정을 거친 것과 같은 결과

# 단어 빈도
TF <- 1+log(DTM) # 로그를 취함으로써 출현빈도의 중요성 축소.
TF
TF[TF==-Inf] <- 0 

# 문서 빈도
# DTM[DTM>0] <- 1
DF <- colSums(DTM)
DF

# 역문서빈도
IDF <- log(dim(DTM)[1]/DF) 
IDF

# 방안 1)
TF
t(TF) # 전치 행렬
TFIDF <- t(t(TF)*IDF)
TFIDF

# 방안 2)
IDFmat <- matrix(IDF, 
                 nrow = dim(TF)[1],
                 ncol = dim(TF)[2], 
                 byrow = T) # 행부터 채움.
IDFmat
TFIDF <- TF*IDFmat
TFIDF