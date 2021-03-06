# 1) 네이버 뉴스 1면의 기사들을 수집하시오.
# (https://news.naver.com/main/home.nhn)

library(rvest); library(httr); library(stringr); library(dplyr)

newsUrl <- "https://news.naver.com/main/ranking/popularDay.nhn?mid=etc&sid1=111"

html <- read_html(newsUrl)
html

links <- html_attr(html_nodes(html, '.content dt a'), 'href')
links <- links[!is.na(links)]
head(links)
length(links)

news <- list()

for (i in 1:length(links)) {
  try({
    htxt = read_html(paste0('https://news.naver.com', links[i]))
    comments = html_nodes(htxt, '#articleBodyContents')
    get_news = repair_encoding(html_text(comments))
    news[i] = str_trim(get_news)
  }, silent = F)
}

length(news)
news[[28]][1]

removeStopword = function(t) {
  t = gsub("[[:cntrl:]]", "", t) 
  t = gsub("http[s]?://[[:alnum:].\\/]+", "", t) 
  t = gsub("&[[:alnum:]]+;", "", t)
  t = gsub("[[:alnum:]]+@[[:alnum:].]+", "", t)
  t = gsub("@[[:alnum:]]+", "", t)
  t = gsub("@[[:alnum:]]+[:]?", "", t)
  t = gsub("[ㄱ-ㅎㅏ-ㅣ]","",t) 
  t = gsub("\\s{2,}", " ", t) 
  t = gsub("[[:punct:]]", "", t)  
  t = gsub("https", "", t)
  t = gsub("RT", "", t)
  t = gsub("바로가기", "", t)
  t = gsub("\\s{2,}", " ", t) 
  gsub('\\p{So}|\\p{Cn}', '', t, perl = TRUE)
}

for (i in 1:length(news)) {
  news[[i]][1] = removeStopword(news[[i]][1])
}

for (i in 1:length(news)) {
  news[[i]][1] = gsub(" flash 오류를 우회하기 위한 함수 추가function flashremoveCallback ",
                      "", news[[i]][1])
  news[[i]][1] = gsub("하기",
                      "", news[[i]][1])
  news[[i]][1] = gsub("때문",
                      "", news[[i]][1])
  news[[i]][1] = gsub("들이",
                      "", news[[i]][1])
}

head(news)


# 2) 수집 된 뉴스로 WordCloud를 작도하시오.

Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk1.8.0_191/")
library(KoNLP)
library(rJava)

tw_news <- unlist(news)

wc_news <- sapply(tw_news, extractNoun, USE.NAMES = F)

nul <- unlist(wc_news)
nul <- nul[nchar(nul) > 1]

news1 <- table(nul)
length(news1)

news2 <- head(sort(news1, decreasing = T), 100)
names(news2)
length(news2)
head(news2)

library(RColorBrewer)
library(wordcloud)

pal <- brewer.pal(9, "Set1")
wordcloud(names(news2), freq = news2, scale = c(5,0.5), rot.per = 0, 
          min.freq = 2, random.order = F, random.color = T, colors = pal)


# 3) 수집 된 뉴스로 연관성분석을 하시오.

library(arules); library(igraph); library(combinat)

nouns_news <- sapply(wc_news, unique)

nouns_news1 <- sapply(nouns_news, function(x) {
                                    Filter(function(y = '') { 
                                      nchar(y) <= 4 && nchar(y) > 1 && is.hangul(y) }, x)
                                 }
                     )

wtrans_news <- as(nouns_news1, "transactions")
wtrans_news

rules_news <- apriori(wtrans_news, parameter = list(supp = 0.2, conf = 0.5))


# confidence 기준으로 상위 30개만을 시각화

library(arulesViz); library(visNetwork)

subrules_news <- head(sort(rules_news, by = "confidence"), 30)

ig_news <- plot(subrules_news, method = "graph", control = list(type = "items"))


# interactive

ig_df_news <- get.data.frame( ig_news, what = "both" )

visNetwork(
  nodes = data.frame(id = ig_df_news$vertices$name,
                     value = ig_df_news$vertices$support,
                     title = ifelse(ig_df_news$vertices$label == "", 
                                    ig_df_news$vertices$name, ig_df_news$vertices$label), 
                     ig_df_news$vertices), edges = ig_df_news$edges) %>%
  visEdges(ig_df_news$edges) %>%
  visOptions(highlightNearest = T)

visNetwork(
  nodes = data.frame(id = ig_df_news$vertices$name,
                     value = ig_df_news$vertices$support, ig_df_news$vertices),
  edges = ig_df_news$edges
)