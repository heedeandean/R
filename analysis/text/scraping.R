# 1.
#install.packages('httr')
#install.packages('XML')

library(httr)
webpaage <- GET('https://press.knou.ac.kr/goods/textBookList.do?condLscValue=001&condMscValue=003&condSscValue=007&condScyr=4')

library(XML)
web <- htmlParse(webpaage)

crsname <- xpathApply(web, '//*[@id="listForm"]/div/div[3]/div[4]/div[3]/table/tbody/tr[4]/td[2]/div/h5/a', xmlValue)
crsname
crsname <- gsub('\r', '', crsname)
crsname <- gsub('\n', '', crsname)
crsname <- gsub('\t', '', crsname)
crsname

ls <- rep('', 5)
ls
for (i in 1:5) {
  sub <- paste0('//*[@id="listForm"]/div/div[3]/div[4]/div[3]/table/tbody/tr[',i,']/td[2]/div/h5/a')
  ls[i] <- xpathApply(web, sub, xmlValue)
  ls[i] <- gsub('\r', '', ls[i])
  ls[i] <- gsub('\n', '', ls[i])
  ls[i] <- gsub('\t', '', ls[i])
}
ls

####
# 2.
#install.packages('rvest')
#install.packages('dplyr')
library(rvest)
library(dplyr)
exurl <- 'https://ko.wikipedia.org/wiki/%EB%B9%84%EC%A0%95%ED%98%95_%EB%8D%B0%EC%9D%B4%ED%84%B0'
html_ex <- read_html(exurl, encoding = 'UTF-8')
html_ex %>% html_nodes('.mw-parser-output p') %>% html_text() # class
html_ex %>% html_nodes('#mw-content-text p') %>% html_text() # id

####
# 3.
ex2url <- 'https://en.wikipedia.org/wiki/Economy_of_South_Korea'
html_ex2 <- read_html(ex2url, encoding = 'UTF-8')
html_ex2 %>% html_nodes('.wikitable') %>% html_table() # 표

####
# 4. 패키지
# 4-1.
#install.packages('textstem')
library(textstem) # 어간추출(stemming)

# 4-2.
library(stopwords)
stopwords(language = 'en', source = 'snowball') # 불용어 사전 (default)
stopwords('en') # 영어
stopwords('de') # 독일어

# 4-3. 감성 어휘
install.packages('tidytext')
install.packages('textdata')
library(tidytext)
library(textdata)

get_sentiments('afinn') # -5 ~ 5 점수 부여.
get_sentiments('nrc') # 10가지 감정 분류