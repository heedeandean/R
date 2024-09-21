# 분석하려면, 텍스트 데이터 => 수치형 데이터로 나타내야함.

# 1. 단어주머니 가설(Bag of Words Hypothesis)
# multiset = 중복집합 = 다중집합: 중복 허용
# 단어 빈도만 평가 => 다른 정보들(ex. 문장구조, 단어 배열 순서) 무시

x <- c('Kim was loved by everybody.', 'Everybody loved Kim','Kim loved everybody.') # 문자열 벡터
strsplit(x, split = ' ')
strsplit(x, split = ' ')[[1]]
strsplit(x, split = ' ')[[1]][3]

# 1-1. 토큰화(tokenization)
# *token: 고유한 의미를 가진 최소 단위.
sub(pattern = "ouse", replacement = "ay", x = "mouse in the house")
gsub(pattern = "ouse", replacement = "ay", x = "mouse in the house")

# 1-1-1.축약(contraction)
contraction_dict <- list(c("don't", "it's", "you're"), c("do not", "it is", "you are"))
contraction_dict
dictlen <- length(contraction_dict[[1]])
dictlen

datstr <- "I don't think you're ready."
for (stri in 1:dictlen) {
  datstr <- gsub(pattern = contraction_dict[[1]][stri],
                 replacement = contraction_dict[[2]][stri], 
                 x=datstr) 
}
datstr
strsplit(datstr, ' ')

# 1-1-2. n-gram: 의미가 있으면 둘 이상의 단어를 하나의 토큰으로 인식.(ex. 숙어, 관용어)
ngram_dict <- list(c("bed and breakfast", "grab and go", "New York"), c("bed_and_breakfast", "grab_and_go", "New_York"))
ndictlen <- length(ngram_dict[[1]])

datstr <- "It's one of the best bed and breakfast in New York."
for (stri in 1:ndictlen) {
  datstr <- gsub(pattern = ngram_dict[[1]][stri],
                 replacement = ngram_dict[[2]][stri], 
                 x=datstr) 
}
datstr

for (stri in 1:dictlen) {
  datstr <- gsub(pattern = contraction_dict[[1]][stri],
                 replacement = contraction_dict[[2]][stri], 
                 x=datstr, ignore.case = T) 
}
datstr
strsplit(datstr, ' ')

# 1-2. 대소문자 변환
x <- c("Kim was a 12-year-old boy.", "It's 11 O'clock.",
       "Everybody loved Kim.")

tolower(x) # toupper()

# 1-3. 문장부호 제거
gsub(x, pattern='([[:punct:]])', replacement='')

x <- gsub(x, pattern='([[:upper:]])', 
          replacement='\\L\\1', # 첫 문자 1개를 소문자로 변환
          perl = T)
x

x <- gsub(x, pattern="([^[:alnum:][:blank:]'-])", # ^(caret): 해당 패턴 제외⭐️
     replacement='')
x
strsplit(x, ' ')

# 1-4. 어간추출(stemming)
library(textstem)
txts <- c("The Williams sisters are leaving this tennis centre.")
stem_strings(txts, language = 'porter')

# 1-5. 원형복원(lemmatization)
lemmatize_strings(txts)

# 1-6. 불용어: 출현 빈도는 높지만 텍스트 데이터 분석에 도움이 되지 않는 단어들. 기능어(ex. 관사, 전치사, 접속사)  - cf. 내용어 
library(stopwords)
stopwords()

txt <- c("He decided to quit his job.", "I do not deny it.", "Can you hear me?")
txt <- tolower(txt)
txt <- gsub(txt, pattern="([^[:alnum:][:blank:]'-])",
            replacement='')
txt <- strsplit(txt, ' ')         

newstop <- c(stopwords(), 'can')
newstop <- setdiff(newstop, c('no', 'not')) 
lapply(txt, setdiff, y=newstop) # 차집합

# 1-7. 전처리 실습
RC <- scan('http://www.gutenberg.org/files/521/521-0.txt',
           what = 'character', encoding = 'UTF-8', sep = '\n') 
RC
# scan(): 비정형 데이터
# read.csv(), read.table(): 정형 데이터 

RC_Chpt <- grep(RC, patter='CHAPTER')
RC_Chpt
RC_End <- grep(RC, patter='END OF THE PROJECT GUTENBERG EBOOK')-1
RC_End
RC_body <- RC[RC_Chpt[1]:RC_End]
RC_body
RC_all <- paste(RC_body, collapse = ' ')
RC_all
RC_all <- gsub(RC_all, pattern="'s", replacement='')
RC_all <- gsub(RC_all, pattern="([^[:alnum:][:blank:]'-])",
               replacement='') 
RC_all <- tolower(RC_all)
RC_all
RC_all <- unlist(strsplit(RC_all, ' '))
RC_all
RC_all <- RC_all[! RC_all %in% c(stopwords(), '')]
RC_all
RC_all <- gsub(RC_all, pattern="'", replacement='')
RC_all <- lemmatize_strings(RC_all)

RC_all_table <- sort(table(RC_all), decreasing = T) # 도수분포표
RC_all_table
RC_all_table
RC_all_proptable <- sort(prop.table(table(RC_all)), decreasing = T) # 상대도수분포표
RC_all_proptable

###############
# 2. 탐색적 자료 분석(Exploratory Data Analysis, EDA): 데이터 특징 파악
library(tidytext)
bingsent <- get_sentiments('bing')
bingsent
bingpos <- bingsent[bingsent$sentiment=='positive',]
bingneg <- bingsent[bingsent$sentiment=='negative',]
bingpos$word[1:30]

RC_all_table
RC_all_table[names(RC_all_table) %in% bingpos$word]

# 2-1. 도수분포표: 단어 출현 빈도
RC_all_table[1:32]  

# 2-1-1. 막대그래프
barplot(RC_all_table[1:32], las=2) 

RC_all # 로빈슨 크루소 - 어휘들을 등장 순서대로 저장한 벡터
RC_sent <- ifelse(RC_all %in% bingpos$word, 1, 
                  ifelse(RC_all %in% bingneg$word, -1, 0))
RC_sent

# 감성 변화 양상
barplot(tapply(RC_sent, (seq_along(RC_sent)-1) %/% 1000, sum)) # 1000개씩 묶음


# 2-1-2. wordcloud
#install.packages('wordcloud')
library(wordcloud)
wordcloud(words = names(RC_all_table), 
          freq = RC_all_table, # 단어 출현 빈도 벡터
          max.words = 100, # 출현 빈도 순으로 100개 단어 
          random.order = F) # 중심부에 출연 빈도 높은 단어 배치

# 글자 색
wordcolor <- rep('grey', length(RC_all_table))
wordcolor[names(RC_all_table) %in% bingpos$word] <- 'blue'
wordcolor[names(RC_all_table) %in% bingneg$word] <- 'black'
wordcolor
wordcloud(words = names(RC_all_table), 
          freq = RC_all_table, 
          max.words = 100, 
          random.order = F,
          colors = wordcolor,
          ordered.colors = T)


#### 문서 비교
# 막대 그래프
RC_by_Chpt <- unlist(strsplit(paste(RC_body, collapse = ' '), 'CHAPTER'))[-1:-21] # list -> vector
RC_by_Chpt

### 전처리
# 문장 부호 삭제
RC_by_Chpt <- gsub(pattern="'s", replacement = '', x=RC_by_Chpt)
RC_by_Chpt <- gsub(RC_by_Chpt, pattern="([^[:alnum:][:blank:]'-])", replacement = '', )

RC_by_Chpt <- tolower(RC_by_Chpt) # 소문자로 변환
RC_by_Chpt <- strsplit(RC_by_Chpt, " ") 
RC_by_Chpt

# 불용어 삭제
RC_by_Chpt <- lapply(RC_by_Chpt,
                     function(x) x[! x %in% c(stopwords(), '')])

# 원형 복원
RC_by_Chpt <- lapply(RC_by_Chpt, lemmatize_strings)
typeof(RC_by_Chpt)

###
lev <- sort(unique(RC_all)) # 단어 중복 X
RC_Chpt3 <- table(factor(RC_by_Chpt[[3]], levels=lev, ordered=T))
RC_Chpt5 <- table(factor(RC_by_Chpt[[5]], levels=lev, ordered=T))
RC_Chpt5
typeof(RC_Chpt5)

RC_Chpt5_3 <- RC_Chpt5 - RC_Chpt3
RC_Chpt5_3 <- sort(RC_Chpt5_3)
RC_Chpt5_3 <- RC_Chpt5_3[abs(RC_Chpt5_3)>5] 
RC_Chpt5_3

barplot(RC_Chpt5_3, las = 2) # 상대적 단어 출현 빈도

# wordcloud
RC_Chpt35 <- cbind(RC_Chpt3, RC_Chpt5) # 단어-문서 행렬(term-document matrix): 행(단어), 열(문서)
RC_Chpt35
commonality.cloud(RC_Chpt35, max.words=200, random.order=F) # 공통 단어
comparison.cloud(RC_Chpt35, max.words=200, random.order=F) # 출현 빈도 차이

# 2-1-3. 산점도
RC_Chpt35 <- cbind(RC_Chpt35, RC_Chpt35[,1]-RC_Chpt35[,2]) # 3열: 출현 빈도 차이
RC_Chpt35
RC_Chpt35col <- ifelse(RC_Chpt35[,3] > 10, 'black',
                       ifelse(RC_Chpt35[,3] < -10, 'blue', 'grey'))
plot(RC_Chpt35[,1], RC_Chpt35[,2], type='n')
text(RC_Chpt35[,1], RC_Chpt35[,2], row.names(RC_Chpt35), col = RC_Chpt35col)


# 2-2. 단어 등장 위치
friday <- ifelse(RC_all == 'friday', 1, 0)
friday
plot(friday, type='h', ylim=c(0,1)) # 히스토그램

fear <- ifelse(RC_all == 'fear', 1, 0)
plot(fear, type='h', ylim=c(0,1)) 