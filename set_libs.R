library(dplyr)
library(ggplot2)
library(gridExtra)
library(tibble)
library(stringi)
library(ggiraph)
library(devtools)
library(ggiraphExtra)
library(kormaps2014)
library(plotly)
library(dygraphs)
library(xts)
library(sqldf)

library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)

# load('data/kdata.rda')
# load('data/data_eng.rda')
# load('data/d.rda')

# mysql 연결.

library(RMySQL)
drv = dbDriver("MySQL")
conn = dbConnect(drv, host='127.0.0.1', port=3306, 
                 dbname='dooodb', user='dooo', password='dooo!')
dbSendQuery(conn, 'set character set utf8') 
dbListTables(conn)   
dbDisconnect(conn); dbUnloadDriver(drv)


# 텍스트 마이닝.

library(rJava)
library(KoNLP)
library(twitteR); library(RCurl); library(RJSONIO); library(stringr)
library(streamR); library(ROAuth)