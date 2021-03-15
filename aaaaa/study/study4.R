# update문.

tryCatch({
  dbBegin(conn)
  x = stringi::stri_enc_toutf8('띵리리리')
  dbGetQuery(conn, paste0("update Meltop set title ='", x, "'  where id=5"))
  dbCommit(conn)
},
error = function(e) { 
  dbRollback(conn)
  print(paste("Error!!", e)) 
},
warning = function(w) {
  print(paste("Warining!!", w))
},
finally = { print("실행되었습니다.") })


# 1) 멜론 탑 100 곡들의 좋아요와 랭킹간의 관계를 산점도로 작도하시오.

md = dbGetQuery(conn, "select * from Meltop")
changeCode(md)
str(md)

ggplot() +
  geom_point(data=md,
             aes(x=rank, y=likecnt),
             color='green', size = 5)

dbDisconnect(conn);
dbUnloadDriver(drv)

# 2) 멜론 탑 100 곡들의 좋아요와 장르간의 관계를 산점도로 작도하시오.

# 도혜's mysql 연결.

library(RMySQL)
drv = dbDriver("MySQL")
conn_mel = dbConnect(drv, host='35.243.112.23', port=3306, 
                 dbname='melondb', user='root', password='eileen')
dbSendQuery(conn_mel, 'set character set utf8') 
dbListTables(conn_mel)   

mg = dbGetQuery(conn_mel, "select M.genre, mean(likecnt) as likecnt
                             from MS_Song M, Song_Rank S
                            where M.song_no = S.song_no
                            group by genre;")
mg

ggplot() +
  geom_point(data=mg,
             aes(x=genre, y=likecnt),
             color='green', size = 5)


# 3) 멜론 탑 100 곡들의 장르와 랭킹간의 관계를 산점도로 작도하시오.

mg_rank = dbGetQuery(conn_mel, "select s.song_no, s.genre, r.rank 
                                  from MS_Song s inner join Song_Rank r on s.song_no = r.song_no 
                                 where r.rankdt = '20190129' 
                                 order by genre")
mg_rank

dbDisconnect(conn_mel)
dbUnloadDriver(drv)

str(mg_rank)

# 1안.

qplot(mg_rank$genre, mg_rank$rank)

# 2안.

ggplot(mg_rank) + 
  geom_point(aes(x=rank, y=genre, size = -rank, col = rank), alpha = 0.5)




